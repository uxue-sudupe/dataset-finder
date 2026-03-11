
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(readxl)
})

base_url <- "https://www.eustat.eus/bankupx/api/v1"

`%||%` <- function(x, y) if (is.null(x)) y else x

# --------- FUNCION AUXILIAR ---------

first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  x[1]
}

# --------- LEER EXCEL MAESTRO ---------

excel_maestro <- "lookup/dominios_operaciones.xlsx"

maestro_raw <- read_excel(excel_maestro)

maestro_dom <- maestro_raw %>%
  transmute(
    dominio_code = str_trim(as.character(dominio_code)),
    operacion_code = str_trim(as.character(operacion_code)),
    operacion_titulo = str_trim(as.character(operacion_titulo)),
    url_ficha_metodologica = str_trim(as.character(url_ficha_metodologica))
  ) %>%
  mutate(
    dominio_code = na_if(dominio_code, ""),
    operacion_code = na_if(operacion_code, ""),
    operacion_titulo = na_if(operacion_titulo, ""),
    url_ficha_metodologica = na_if(url_ficha_metodologica, "")
  ) %>%
  filter(!is.na(dominio_code)) %>%
  group_by(dominio_code) %>%
  summarise(
    operacion_code = first_non_na(operacion_code),
    operacion_titulo = first_non_na(operacion_titulo),
    url_ficha_metodologica = first_non_na(url_ficha_metodologica),
    .groups = "drop"
  )

# --------- FUNCIONES API ---------

get_json_safe <- function(url) {
  resp <- try(GET(url), silent = TRUE)
  if (inherits(resp, "try-error")) return(NULL)
  if (status_code(resp) != 200) return(NULL)
  
  txt <- content(resp, as = "text", encoding = "UTF-8")
  if (!nzchar(txt)) return(NULL)
  
  tryCatch(
    fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

get_catalog <- function(lang = "es") {
  url <- paste0(base_url, "/", lang, "/DB")
  out <- get_json_safe(url)
  
  if (is.null(out)) stop("No se pudo leer el catálogo")
  
  tibble(
    id = map_chr(out, ~ .x$id %||% NA_character_),
    type = map_chr(out, ~ .x$type %||% NA_character_),
    text = map_chr(out, ~ .x$text %||% NA_character_),
    updated = map_chr(out, ~ .x$updated %||% NA_character_)
  )
}

get_metadata <- function(tabla_id, lang = "es") {
  url <- paste0(base_url, "/", lang, "/DB/", URLencode(tabla_id, reserved = TRUE))
  get_json_safe(url)
}

# --------- EXTRAER dominio_code ---------

extract_domain_code <- function(id) {
  x <- str_match(id, "^PX_(?:[^_]*_)?([^_]+)")[,2]
  ifelse(is.na(x), NA_character_, x)
}

# --------- RANGO TEMPORAL ---------

extract_time_range <- function(metadata) {
  
  vars <- metadata$variables %||% list()
  
  time_var <- keep(vars, ~ isTRUE(.x$time %||% FALSE))
  
  if (length(time_var) == 0) {
    return(list(
      time_var = NA_character_,
      first_period = NA_character_,
      last_period = NA_character_
    ))
  }
  
  tv <- time_var[[1]]
  
  vals <- tv$valueTexts %||% tv$values %||% character(0)
  vals <- as.character(unlist(vals))
  
  if (length(vals) == 0) {
    return(list(
      time_var = tv$text %||% tv$code %||% NA_character_,
      first_period = NA_character_,
      last_period = NA_character_
    ))
  }
  
  list(
    time_var = tv$text %||% tv$code %||% NA_character_,
    first_period = vals[1],
    last_period = vals[length(vals)]
  )
}

# --------- DETECTAR FRECUENCIA ---------

detect_frequency <- function(metadata) {
  
  vars <- metadata$variables %||% list()
  
  var_names <- map_chr(vars, ~ tolower(.x$text %||% .x$code %||% ""))
  
  if (any(str_detect(var_names, "mes|month"))) return("mensual")
  if (any(str_detect(var_names, "trimestre|quarter"))) return("trimestral")
  
  if (any(map_lgl(vars, ~ isTRUE(.x$time %||% FALSE)))) return("anual")
  
  NA_character_
}

# --------- LIMPIAR PALABRAS ---------

clean_tokens <- function(x) {
  
  if (length(x) == 0 || all(is.na(x))) return(character(0))
  
  x |>
    tolower() |>
    str_replace_all("[[:punct:]]", " ") |>
    str_split("\\s+") |>
    unlist() |>
    unique() |>
    (\(z) z[nchar(z) >= 3])()
}

# --------- CONSTRUIR INDICE ---------

build_index_one_lang <- function(lang = "es", sleep_sec = 0.05, max_tables = NULL) {
  
  catalog <- get_catalog(lang)
  
  if (!is.null(max_tables)) {
    catalog <- head(catalog, max_tables)
  }
  
  rows <- map(seq_len(nrow(catalog)), function(i) {
    
    id <- catalog$id[i]
    cat_title <- catalog$text[i]
    updated <- catalog$updated[i]
    type <- catalog$type[i]
    
    dominio_code <- extract_domain_code(id)
    
    cat(sprintf("[%s] Tabla %d de %d: %s\n", lang, i, nrow(catalog), id))
    
    meta <- get_metadata(id, lang)
    
    Sys.sleep(sleep_sec)
    
    if (is.null(meta)) {
      
      return(tibble(
        lang = lang,
        id = id,
        dominio_code = dominio_code,
        type = type,
        title = cat_title,
        updated = updated,
        variables = list(character(0)),
        time_var = NA_character_,
        first_period = NA_character_,
        last_period = NA_character_,
        frecuencia = NA_character_,
        keywords = NA_character_,
        search_text = tolower(cat_title),
        api_url = paste0(base_url, "/", lang, "/DB/", URLencode(id, reserved = TRUE)),
        pxweb_url = paste0("https://www.eustat.eus/bankupx/pxweb/", lang, "/DB/-/", id),
        metadata_ok = FALSE
      ))
    }
    
    vars <- meta$variables %||% list()
    
    var_names <- map_chr(vars, ~ .x$text %||% .x$code %||% NA_character_)
    var_names <- var_names[!is.na(var_names)]
    
    rng <- extract_time_range(meta)
    
    frecuencia <- detect_frequency(meta)
    
    kws <- unique(c(
      clean_tokens(meta$title %||% cat_title),
      clean_tokens(var_names)
    ))
    
    tibble(
      lang = lang,
      id = id,
      dominio_code = dominio_code,
      type = type,
      title = meta$title %||% cat_title,
      updated = updated,
      variables = list(var_names),
      time_var = rng$time_var,
      first_period = rng$first_period,
      last_period = rng$last_period,
      frecuencia = frecuencia,
      keywords = paste(kws, collapse = ", "),
      search_text = paste(
        tolower(meta$title %||% cat_title),
        tolower(paste(var_names, collapse = " ")),
        tolower(paste(kws, collapse = " "))
      ),
      api_url = paste0(base_url, "/", lang, "/DB/", URLencode(id, reserved = TRUE)),
      pxweb_url = paste0("https://www.eustat.eus/bankupx/pxweb/", lang, "/DB/-/", id),
      metadata_ok = TRUE
    )
  })
  
  index_df <- bind_rows(rows) %>%
    left_join(maestro_dom, by = "dominio_code") %>%
    mutate(
      search_text = paste(
        search_text,
        coalesce(operacion_code, ""),
        coalesce(operacion_titulo, "")
      )
    )
  
  index_df
}

# --------- EJECUTAR SCRIPT ---------

cat("Construyendo índice...\n")

index_es <- build_index_one_lang(
  lang = "es",
  sleep_sec = 0.05
  # max_tables = 20   # activar para probar rápido
)

cat("Número de tablas:", nrow(index_es), "\n")

dir.create("data", showWarnings = FALSE)

jsonlite::write_json(
  index_es,
  path = "data/index_es.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat("JSON guardado en data/index_es.json\n")