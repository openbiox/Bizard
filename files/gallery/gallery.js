/* Bizard Graph Gallery — client-side browsing, filtering, synonym-aware
   search, and lightbox. Shared by the English and Chinese pages;
   all page-specific text comes from the #bz-gallery-config JSON island. */

(function () {
  'use strict';

  var ZOOM_ICON =
    '<svg width="34" height="34" viewBox="0 0 24 24" fill="none" stroke="white" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">' +
    '<circle cx="11" cy="11" r="8"></circle><line x1="21" y1="21" x2="16.65" y2="16.65"></line>' +
    '<line x1="11" y1="8" x2="11" y2="14"></line><line x1="8" y1="11" x2="14" y2="11"></line></svg>';

  function readJson(id) {
    var el = document.getElementById(id);
    return el ? JSON.parse(el.textContent) : null;
  }

  function esc(value) {
    return String(value == null ? '' : value).replace(/[&<>"']/g, function (char) {
      return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#039;' }[char];
    });
  }

  function format(template, values) {
    return String(template).replace(/\{(\w+)\}/g, function (_, key) {
      return values[key] != null ? values[key] : '';
    });
  }

  var state = { query: '', collection: 'all', family: 'all' };
  var charts = [];
  var config = null;
  var collectionCounts = {};

  function strings() {
    return config.strings;
  }

  function collectionLabel(key) {
    return (config.collectionLabels && config.collectionLabels[key]) || key;
  }

  function chartText(chart) {
    return [
      chart.Name,
      chart.Description,
      chart.Type,
      chart.Level1,
      chart.Level2,
      chart.FamilyLabel
    ]
      .join(' ')
      .toLowerCase();
  }

  function expandToken(token) {
    var synonyms = (config && config.synonyms) || {};
    var has = Object.prototype.hasOwnProperty;
    var expanded = [token];
    if (has.call(synonyms, token)) {
      synonyms[token].forEach(function (term) {
        if (expanded.indexOf(term) === -1) expanded.push(term);
      });
      return expanded;
    }
    Object.keys(synonyms).forEach(function (key) {
      if (synonyms[key].indexOf(token) === -1) return;
      if (expanded.indexOf(key) === -1) expanded.push(key);
      synonyms[key].forEach(function (term) {
        if (expanded.indexOf(term) === -1) expanded.push(term);
      });
    });
    return expanded;
  }

  function filteredCharts() {
    var tokens = state.query.trim().toLowerCase().split(/\s+/).filter(Boolean);
    var tokenGroups = tokens.map(expandToken);
    var rows = charts.filter(function (chart) {
      if (state.collection !== 'all' && chart.Level1 !== state.collection) return false;
      if (state.family !== 'all' && chart.Family !== state.family) return false;
      if (!tokenGroups.length) return true;
      var text = chartText(chart);
      return tokenGroups.every(function (group) {
        return group.some(function (term) {
          return text.indexOf(term) !== -1;
        });
      });
    });

    if (tokenGroups.length) {
      rows = rows
        .map(function (chart) {
          var name = String(chart.Name).toLowerCase();
          var type = String(chart.Type).toLowerCase();
          var family = String(chart.FamilyLabel).toLowerCase();
          var description = String(chart.Description).toLowerCase();
          var score = 0;
          tokenGroups.forEach(function (group) {
            var best = 0;
            group.forEach(function (term, index) {
              var factor = index === 0 ? 1 : 0.5;
              var termScore = 0;
              if (name === term) termScore = 8;
              else if (name.indexOf(term) !== -1) termScore = 6;
              else if (type.indexOf(term) !== -1) termScore = 4;
              else if (family.indexOf(term) !== -1) termScore = 2;
              else if (description.indexOf(term) !== -1) termScore = 1;
              termScore *= factor;
              if (termScore > best) best = termScore;
            });
            score += best;
          });
          return { chart: chart, score: score };
        })
        .sort(function (a, b) {
          return b.score - a.score || a.chart.Id - b.chart.Id;
        })
        .map(function (item) {
          return item.chart;
        });
    }
    return rows;
  }

  function cardHtml(chart) {
    return (
      '<div class="bz-card">' +
      '<div class="bz-card-img-wrap" data-src="' + esc(chart.Image_url) + '" data-name="' + esc(chart.Name) + '" title="' + esc(strings().zoom) + '">' +
      '<img src="' + esc(chart.Image_url) + '" alt="' + esc(chart.Name) + '" loading="lazy">' +
      '<div class="bz-card-overlay">' + ZOOM_ICON + '</div>' +
      '</div>' +
      '<a class="bz-card-title" href="' + esc(chart.Tutorial_url) + '" target="_blank" rel="noopener" title="' + esc(chart.Description) + '">' + esc(chart.Name) + '</a>' +
      '<div class="bz-card-meta">' +
      '<span class="bz-badge" title="' + esc(strings().typeLabel) + ': ' + esc(chart.Type) + '">' + esc(chart.Type) + '</span>' +
      '<span class="bz-badge bz-badge-muted" title="' + esc(strings().categoryLabel) + ': ' + esc(chart.FamilyLabel) + '">' + esc(chart.FamilyLabel) + '</span>' +
      '</div>' +
      '</div>'
    );
  }

  function renderGrid(rows) {
    var grid = document.getElementById('bz-grid');
    var count = document.getElementById('bz-result-count');
    var s = strings();

    grid.innerHTML = rows.map(cardHtml).join('');

    if (rows.length) {
      var message = state.query.trim() || state.family !== 'all' || state.collection !== 'all'
        ? format(s.showing, { n: rows.length, total: charts.length })
        : format(s.showingAll, { total: charts.length });
      count.textContent = message;
    } else {
      count.textContent = '';
      grid.innerHTML = '<div class="bz-empty">' + esc(format(s.noResults, { query: state.query.trim() })) + '</div>';
    }
  }

  function renderCollections() {
    var wrap = document.getElementById('bz-collection-pills');
    var s = strings();
    var order = (config.collectionOrder || Object.keys(collectionCounts)).filter(function (key) {
      return collectionCounts[key];
    });

    var html =
      '<button type="button" class="bz-pill' + (state.collection === 'all' ? ' active' : '') + '" data-collection="all">' +
      esc(s.all) + ' (' + charts.length + ')</button>';
    order.forEach(function (key) {
      html +=
        '<button type="button" class="bz-pill' + (state.collection === key ? ' active' : '') + '" data-collection="' + esc(key) + '">' +
        esc(collectionLabel(key)) + ' (' + collectionCounts[key] + ')</button>';
    });
    wrap.innerHTML = html;
  }

  function renderFamilies() {
    var wrap = document.getElementById('bz-family-pills');
    var s = strings();
    var counts = {};
    var labels = {};

    charts.forEach(function (chart) {
      if (state.collection !== 'all' && chart.Level1 !== state.collection) return;
      counts[chart.Family] = (counts[chart.Family] || 0) + 1;
      if (!labels[chart.Family]) labels[chart.Family] = chart.FamilyLabel;
    });

    var keys = Object.keys(counts).sort(function (a, b) {
      return counts[b] - counts[a] || labels[a].localeCompare(labels[b]);
    });

    var visible = counts[state.family] ? state.family : 'all';
    if (visible !== state.family) state.family = 'all';

    var html =
      '<button type="button" class="bz-tag' + (state.family === 'all' ? ' active' : '') + '" data-family="all">' +
      esc(s.all) + '<span class="bz-tag-count">' + (state.collection === 'all' ? charts.length : collectionCounts[state.collection]) + '</span></button>';
    keys.forEach(function (key) {
      html +=
        '<button type="button" class="bz-tag' + (state.family === key ? ' active' : '') + '" data-family="' + esc(key) + '">' +
        esc(labels[key]) + '<span class="bz-tag-count">' + counts[key] + '</span></button>';
    });
    wrap.innerHTML = html;
  }

  function update() {
    renderGrid(filteredCharts());
    renderFamilies();
  }

  function initGallery() {
    var input = document.getElementById('bz-search-input');
    var debounceTimer = null;

    collectionCounts = {};
    charts.forEach(function (chart) {
      collectionCounts[chart.Level1] = (collectionCounts[chart.Level1] || 0) + 1;
    });

    input.addEventListener('input', function () {
      window.clearTimeout(debounceTimer);
      debounceTimer = window.setTimeout(function () {
        state.query = input.value;
        update();
      }, 250);
    });
    input.addEventListener('keydown', function (event) {
      if (event.key === 'Enter') {
        window.clearTimeout(debounceTimer);
        state.query = input.value;
        update();
      }
    });
    document.getElementById('bz-search-btn').addEventListener('click', function () {
      window.clearTimeout(debounceTimer);
      state.query = input.value;
      update();
    });
    document.getElementById('bz-clear-btn').addEventListener('click', function () {
      input.value = '';
      state.query = '';
      state.collection = 'all';
      state.family = 'all';
      renderCollections();
      update();
      input.focus();
    });

    document.getElementById('bz-collection-pills').addEventListener('click', function (event) {
      var pill = event.target.closest('[data-collection]');
      if (!pill) return;
      state.collection = pill.getAttribute('data-collection');
      state.family = 'all';
      renderCollections();
      update();
    });
    document.getElementById('bz-family-pills').addEventListener('click', function (event) {
      var tag = event.target.closest('[data-family]');
      if (!tag) return;
      state.family = tag.getAttribute('data-family');
      renderFamilies();
      renderGrid(filteredCharts());
    });

    var grid = document.getElementById('bz-grid');
    grid.addEventListener('click', function (event) {
      var wrap = event.target.closest('.bz-card-img-wrap');
      if (wrap) openLightbox(wrap.getAttribute('data-src'), wrap.getAttribute('data-name'));
    });
    grid.addEventListener('load', function (event) {
      if (event.target && event.target.tagName === 'IMG') event.target.classList.add('bz-loaded');
    }, true);

    renderCollections();
    update();
  }

  /* Lightbox */
  function openLightbox(src, name) {
    var lightbox = document.getElementById('bz-lightbox');
    var image = document.getElementById('bz-lightbox-image');
    image.src = src;
    image.alt = name || '';
    lightbox.classList.add('active');
    document.body.style.overflow = 'hidden';
  }

  function closeLightbox() {
    var lightbox = document.getElementById('bz-lightbox');
    lightbox.classList.remove('active');
    document.getElementById('bz-lightbox-image').src = '';
    document.body.style.overflow = '';
  }

  function init() {
    charts = readJson('bz-gallery-data') || [];
    config = readJson('bz-gallery-config');
    if (!config || !charts.length) return;

    var lightbox = document.getElementById('bz-lightbox');
    lightbox.addEventListener('click', closeLightbox);
    document.addEventListener('keydown', function (event) {
      if (event.key === 'Escape') closeLightbox();
    });

    initGallery();
  }

  window.bzShowLightbox = openLightbox;

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
