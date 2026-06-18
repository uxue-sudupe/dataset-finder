    const state = {
      rawData: [],
      filteredData: [],
      currentPage: 1,
      pageSize: 25,
      sortKey: 'updated',
      sortDir: 'desc',
      titleUniverse: []
    };

    const els = {
      operationSearch: document.getElementById('operationSearch'),
      titleSearch: document.getElementById('titleSearch'),
      titleSuggestions: document.getElementById('titleSuggestions'),
      globalSearch: document.getElementById('globalSearch'),
      pageSize: document.getElementById('pageSize'),
      tbody: document.querySelector('#resultsTable tbody'),
      summary: document.getElementById('summary'),
      prevBtn: document.getElementById('prevBtn'),
      nextBtn: document.getElementById('nextBtn'),
      downloadCsvBtn: document.getElementById('downloadCsvBtn'),
      downloadJsonBtn: document.getElementById('downloadJsonBtn'),
      indexUpdated: document.getElementById('indexUpdated'),
      apiModalBackdrop: document.getElementById('apiModalBackdrop'),
      apiModalClose: document.getElementById('apiModalClose'),
      modalCloseBtn: document.getElementById('modalCloseBtn'),
      getUrlCode: document.getElementById('getUrlCode'),
      postUrlCode: document.getElementById('postUrlCode'),
      postBodyCode: document.getElementById('postBodyCode')
    };

    function normalizeText(value) {
      return String(value ?? '')
        .toLowerCase()
        .normalize('NFD')
        .replace(/[\u0300-\u036f]/g, '')
        .trim();
    }

    function arrayToDisplay(value) {
      if (Array.isArray(value)) return value.join(', ');
      return value ?? '';
    }

    function escapeHtml(str) {
      return String(str ?? '')
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#039;');
    }

    function downloadBlob(content, fileName, mimeType) {
      const blob = new Blob([content], { type: mimeType });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = fileName;
      document.body.appendChild(a);
      a.click();
      a.remove();
      URL.revokeObjectURL(url);
    }

    function buildOperationSelector(data) {
      const operations = [...new Set(data.map(d => d.operacion_titulo).filter(Boolean))]
        .sort((a, b) => a.localeCompare(b));

      els.operationSearch.innerHTML =
        `<option value="">Todas las operaciones</option>` +
        operations.map(op => `<option value="${escapeHtml(op)}">${escapeHtml(op)}</option>`).join('');
    }

    function rebuildTitleSuggestions() {
      const operationValue = els.operationSearch.value;

      let rows = state.rawData;
      if (operationValue) {
        rows = rows.filter(row => normalizeText(row.operacion_titulo) === normalizeText(operationValue));
      }

      const titles = [...new Set(rows.map(d => d.title).filter(Boolean))]
        .sort((a, b) => a.localeCompare(b));

      state.titleUniverse = titles;

      els.titleSuggestions.innerHTML = titles
        .map(title => `<option value="${escapeHtml(title)}"></option>`)
        .join('');
    }

    function matchesOperation(row, operationValue) {
      if (!operationValue) return true;
      return normalizeText(row.operacion_titulo) === normalizeText(operationValue);
    }

    function matchesTitle(row, titleValue) {
      if (!titleValue) return true;
      return normalizeText(row.title) === normalizeText(titleValue);
    }

    function matchesGlobal(row, term) {
      if (!term) return true;

      const haystack = [
        row.title,
        row.operacion_titulo,
        row.keywords,
        arrayToDisplay(row.variables),
        row.search_text,
        row.first_period,
        row.last_period,
        row.frecuencia,
        row.updated
      ].map(normalizeText).join(' ');

      return haystack.includes(term);
    }

    function parseDateValue(value) {
      const date = new Date(value);
      const time = date.getTime();
      return Number.isNaN(time) ? null : time;
    }

    function sortData() {
      const { sortKey, sortDir } = state;
      const factor = sortDir === 'asc' ? 1 : -1;

      state.filteredData.sort((a, b) => {
        if (sortKey === 'updated') {
          const av = parseDateValue(a.updated);
          const bv = parseDateValue(b.updated);
          if (av === null && bv === null) return 0;
          if (av === null) return 1;
          if (bv === null) return -1;
          return (av - bv) * factor;
        }

        let av = sortKey === 'variables' ? arrayToDisplay(a.variables) : (a[sortKey] ?? '');
        let bv = sortKey === 'variables' ? arrayToDisplay(b.variables) : (b[sortKey] ?? '');

        av = normalizeText(av);
        bv = normalizeText(bv);

        if (av < bv) return -1 * factor;
        if (av > bv) return 1 * factor;
        return 0;
      });
    }

    function applyFilters() {
      const operationValue = els.operationSearch.value;
      const titleValue = normalizeText(els.titleSearch.value);
      const globalTerm = normalizeText(els.globalSearch.value);

      let baseData = state.rawData.filter(row => {
        if (!matchesOperation(row, operationValue)) return false;
        if (!matchesTitle(row, titleValue)) return false;
        return true;
      });

      if (globalTerm) {
        baseData = baseData.filter(row => matchesGlobal(row, globalTerm));
      }

      state.filteredData = baseData;
      sortData();
      state.currentPage = 1;
      renderTable();
    }

    function renderVariableTags(variables) {
      if (!Array.isArray(variables) || variables.length === 0) return '';
      return variables.slice(0, 4).map(v => `<span class="tag">${escapeHtml(v)}</span>`).join('');
    }

    function renderTable() {
      const start = (state.currentPage - 1) * state.pageSize;
      const end = start + state.pageSize;
      const pageRows = state.filteredData.slice(start, end);

      els.tbody.innerHTML = pageRows.map((row, idx) => {
        const rownum = start + idx + 1;
        const apiLabel = row.api_url
          ? `<a href="#" class="api-link" data-api-url="${escapeHtml(row.api_url)}">api</a>`
          : '';

        const datosLabel = row.pxweb_url
          ? `<a href="${row.pxweb_url}" target="_blank" rel="noopener">datos</a>`
          : '';

        const fichaLabel = row.url_ficha_metodologica
          ? `<a href="${row.url_ficha_metodologica}" target="_blank" rel="noopener">ficha</a>`
          : '';

        return `
          <tr>
            <td>${rownum}</td>
            <td>${escapeHtml(row.operacion_titulo || '')}</td>
            <td>
              <div class="dataset-title">${escapeHtml(row.title || '')}</div>
              <div class="dataset-sub">${escapeHtml(row.id || '')}</div>
            </td>
            <td>${escapeHtml(row.frecuencia || '')}</td>
            <td>${escapeHtml(row.first_period || '')}</td>
            <td>${escapeHtml(row.last_period || '')}</td>
            <td>${escapeHtml(row.updated || '')}</td>
            <td>${renderVariableTags(row.variables)}</td>
            <td>${datosLabel}</td>
            <td>${apiLabel}</td>
            <td>${fichaLabel}</td>
          </tr>
        `;
      }).join('');

      const total = state.filteredData.length;
      const shownFrom = total === 0 ? 0 : start + 1;
      const shownTo = Math.min(end, total);
      els.summary.textContent = `Mostrando ${shownFrom} a ${shownTo} de ${total} entradas`;

      els.prevBtn.disabled = state.currentPage === 1;
      els.nextBtn.disabled = end >= total;

      bindApiLinks();
    }

    function bindSorting() {
      document.querySelectorAll('th[data-sort]').forEach(th => {
        th.addEventListener('click', () => {
          const key = th.dataset.sort;
          if (state.sortKey === key) {
            state.sortDir = state.sortDir === 'asc' ? 'desc' : 'asc';
          } else {
            state.sortKey = key;
            state.sortDir = key === 'updated' ? 'desc' : 'asc';
          }
          sortData();
          renderTable();
        });
      });
    }

    function openApiModal(apiUrl) {
      els.getUrlCode.textContent = apiUrl;
      els.postUrlCode.textContent = apiUrl;
      els.postBodyCode.textContent = JSON.stringify({
        query: [],
        response: { format: 'json-stat' }
      }, null, 2);

      els.apiModalBackdrop.classList.add('open');
      activateTab('get');
    }

    function closeApiModal() {
      els.apiModalBackdrop.classList.remove('open');
    }

    function activateTab(tabName) {
      document.querySelectorAll('.api-tab').forEach(btn => {
        btn.classList.toggle('active', btn.dataset.tab === tabName);
      });
      document.querySelectorAll('.api-panel').forEach(panel => {
        panel.classList.toggle('active', panel.dataset.panel === tabName);
      });
    }

    function bindApiLinks() {
      document.querySelectorAll('.api-link').forEach(link => {
        link.addEventListener('click', evt => {
          evt.preventDefault();
          openApiModal(link.dataset.apiUrl);
        });
      });
    }

    function bindModal() {
      els.apiModalClose.addEventListener('click', closeApiModal);
      els.modalCloseBtn.addEventListener('click', closeApiModal);
      els.apiModalBackdrop.addEventListener('click', evt => {
        if (evt.target === els.apiModalBackdrop) closeApiModal();
      });
      document.addEventListener('keydown', evt => {
        if (evt.key === 'Escape') closeApiModal();
      });

      document.querySelectorAll('.api-tab').forEach(btn => {
        btn.addEventListener('click', () => activateTab(btn.dataset.tab));
      });

      document.querySelectorAll('.copy-btn').forEach(btn => {
        btn.addEventListener('click', async () => {
          const target = document.getElementById(btn.dataset.copyTarget);
          try {
            await navigator.clipboard.writeText(target.textContent);
            btn.textContent = 'Copiado';
            setTimeout(() => { btn.textContent = 'Copiar'; }, 1200);
          } catch (e) {
            btn.textContent = 'Error';
            setTimeout(() => { btn.textContent = 'Copiar'; }, 1200);
          }
        });
      });
    }

    function prepareDownloadRows(rows) {
      return rows.map(row => ({
        lang: row.lang ?? '',
        id: row.id ?? '',
        operacion_titulo: row.operacion_titulo ?? '',
        title: row.title ?? '',
        frecuencia: row.frecuencia ?? '',
        first_period: row.first_period ?? '',
        last_period: row.last_period ?? '',
        updated: row.updated ?? '',
        variables: arrayToDisplay(row.variables),
        keywords: row.keywords ?? '',
        api_url: row.api_url ?? '',
        pxweb_url: row.pxweb_url ?? '',
        url_ficha_metodologica: row.url_ficha_metodologica ?? ''
      }));
    }

    function downloadFilteredJson() {
      const rows = prepareDownloadRows(state.filteredData);
      const payload = {
        exported_at: new Date().toISOString(),
        n_rows: rows.length,
        data: rows
      };
      downloadBlob(
        JSON.stringify(payload, null, 2),
        'eustat_resultados.json',
        'application/json;charset=utf-8'
      );
    }

    function toCsvValue(value) {
      const text = String(value ?? '');
      if (/[",\n;]/.test(text)) {
        return `"${text.replace(/"/g, '""')}"`;
      }
      return text;
    }

    function downloadFilteredCsv() {
      const rows = prepareDownloadRows(state.filteredData);
      const headers = [
        'lang',
        'id',
        'operacion_titulo',
        'title',
        'frecuencia',
        'first_period',
        'last_period',
        'updated',
        'variables',
        'keywords',
        'api_url',
        'pxweb_url',
        'url_ficha_metodologica'
      ];

      const csv = [
        headers.join(','),
        ...rows.map(row => headers.map(h => toCsvValue(row[h])).join(','))
      ].join('\n');

      downloadBlob(
        csv,
        'eustat_resultados.csv',
        'text/csv;charset=utf-8'
      );
    }

    function renderIndexMeta(payload, data) {
      const parts = [];

      if (payload.updated_at) {
        parts.push(`<span class="meta-pill">Índice actualizado: ${escapeHtml(payload.updated_at)}</span>`);
      }

      parts.push(`<span class="meta-pill">${data.length} tablas cargadas</span>`);

      els.indexUpdated.innerHTML = parts.join('');
    }

    async function loadData() {
      try {
        const response = await fetch('./data/index_es.json?ts=' + Date.now());
        const payload = await response.json();

        const data = Array.isArray(payload) ? payload : (payload.data || []);
        state.rawData = data;
        state.filteredData = [...data];

        renderIndexMeta(Array.isArray(payload) ? {} : payload, data);
        buildOperationSelector(data);
        rebuildTitleSuggestions();
        sortData();
        renderTable();

      } catch (error) {
        els.tbody.innerHTML =
          '<tr><td colspan="11">No se ha podido cargar data/index_es.json</td></tr>';
        console.error(error);
      }
    }

    els.operationSearch.addEventListener('change', () => {
      els.titleSearch.value = '';
      rebuildTitleSuggestions();
      applyFilters();
    });

    els.titleSearch.addEventListener('input', applyFilters);
    els.titleSearch.addEventListener('change', applyFilters);
    els.globalSearch.addEventListener('input', applyFilters);

    els.pageSize.addEventListener('change', () => {
      state.pageSize = Number(els.pageSize.value);
      state.currentPage = 1;
      renderTable();
    });

    els.prevBtn.addEventListener('click', () => {
      if (state.currentPage > 1) {
        state.currentPage -= 1;
        renderTable();
      }
    });

    els.nextBtn.addEventListener('click', () => {
      const maxPage = Math.ceil(state.filteredData.length / state.pageSize);
      if (state.currentPage < maxPage) {
        state.currentPage += 1;
        renderTable();
      }
    });

    els.downloadJsonBtn.addEventListener('click', downloadFilteredJson);
    els.downloadCsvBtn.addEventListener('click', downloadFilteredCsv);

    bindSorting();
    bindModal();
    loadData();
