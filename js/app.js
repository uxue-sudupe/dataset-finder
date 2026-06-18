function renderVariableTags(variables) {
  if (!Array.isArray(variables) || variables.length === 0) return '';

  return variables
    .slice(0, 4)
    .map(v => `<span class="tag">${escapeHtml(v)}</span>`)
    .join('');
}
