/* Bizard Graph Gallery — client-side browsing, filtering, search, lightbox,
   and the smart chart recommender. Shared by the English and Chinese pages;
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

  function filteredCharts() {
    var tokens = state.query.trim().toLowerCase().split(/\s+/).filter(Boolean);
    var rows = charts.filter(function (chart) {
      if (state.collection !== 'all' && chart.Level1 !== state.collection) return false;
      if (state.family !== 'all' && chart.Family !== state.family) return false;
      if (!tokens.length) return true;
      var text = chartText(chart);
      return tokens.every(function (token) {
        return text.indexOf(token) !== -1;
      });
    });

    if (tokens.length) {
      rows = rows
        .map(function (chart) {
          var name = String(chart.Name).toLowerCase();
          var type = String(chart.Type).toLowerCase();
          var score = 0;
          tokens.forEach(function (token) {
            if (name === token) score += 8;
            else if (name.indexOf(token) !== -1) score += 6;
            if (type.indexOf(token) !== -1) score += 4;
            if (String(chart.FamilyLabel).toLowerCase().indexOf(token) !== -1) score += 2;
            if (String(chart.Description).toLowerCase().indexOf(token) !== -1) score += 1;
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

  /* Smart chart recommender (bilingual, keyword + intent based) */
  var keywordsData = null;

  function loadKeywords() {
    if (keywordsData) return Promise.resolve(keywordsData);
    return Promise.all([
      fetch(config.recommender.keywordsEn).then(function (res) {
        if (!res.ok) throw new Error('Failed to load English keywords');
        return res.json();
      }),
      fetch(config.recommender.keywordsZh).then(function (res) {
        if (!res.ok) throw new Error('Failed to load Chinese keywords');
        return res.json();
      })
    ]).then(function (results) {
      keywordsData = {
        synonyms: Object.assign({}, results[0].synonyms, results[1].synonyms),
        intentKeywords: Object.assign({}, results[0].intentKeywords, results[1].intentKeywords)
      };
      return keywordsData;
    });
  }

  function tokenize(text) {
    return text
      .toLowerCase()
      .split(/[\s,.!?;:'"()\[\]]+/)
      .filter(function (token) {
        return token.length > 0 && !/[\u4e00-\u9fa5]/.test(token);
      });
  }

  function matchesPhrase(haystack, phrase) {
    var normalizedHaystack = String(haystack).toLowerCase().replace(/[\s_-]+/g, ' ');
    var needle = String(phrase).toLowerCase().replace(/[\s_-]+/g, ' ');
    if (/[\u4e00-\u9fa5]/.test(needle)) return normalizedHaystack.indexOf(needle) !== -1;
    var escapedNeedle = needle.replace(/[.*+?^${}()|[\]\\]/g, '\\$&').replace(/\s+/g, '\\s+');
    return new RegExp('\\b' + escapedNeedle + '\\b').test(normalizedHaystack);
  }

  function extractKeywords(text, synonyms) {
    var found = new Set();
    var textLower = text.toLowerCase();

    Object.keys(synonyms).forEach(function (key) {
      if (matchesPhrase(textLower, key)) {
        found.add(key);
        synonyms[key].forEach(function (synonym) {
          found.add(synonym);
        });
      }
      synonyms[key].forEach(function (synonym) {
        if (matchesPhrase(textLower, synonym)) {
          found.add(synonym);
          found.add(key);
          synonyms[key].forEach(function (sibling) {
            found.add(sibling);
          });
        }
      });
    });

    tokenize(text).forEach(function (token) {
      found.add(token);
    });
    return Array.from(found);
  }

  function expandSynonyms(tokens, synonyms) {
    var expanded = new Set(tokens);
    tokens.forEach(function (token) {
      var tokenLower = token.toLowerCase();

      if (synonyms[token] || synonyms[tokenLower]) {
        (synonyms[token] || synonyms[tokenLower]).forEach(function (synonym) {
          expanded.add(synonym);
          if (!/[\u4e00-\u9fa5]/.test(synonym)) expanded.add(synonym.toLowerCase());
        });
      }
      Object.keys(synonyms).forEach(function (key) {
        var values = synonyms[key];
        if (values.indexOf(token) !== -1 || values.indexOf(tokenLower) !== -1 || key === token || key === tokenLower) {
          expanded.add(key);
          values.forEach(function (synonym) {
            expanded.add(synonym);
          });
        }
      });
    });
    return Array.from(expanded);
  }

  function matchIntents(text, intentKeywords) {
    return Object.keys(intentKeywords).filter(function (intent) {
      return intentKeywords[intent].some(function (keyword) {
        return matchesPhrase(text, keyword);
      });
    });
  }

  function recommendCharts() {
    var inputEl = document.getElementById('bz-recommender-input');
    var resultsEl = document.getElementById('bz-recommender-results');
    var button = document.getElementById('bz-recommender-btn');
    var s = strings();
    var input = inputEl.value.trim();

    if (!input) {
      resultsEl.innerHTML = '<p>' + esc(s.recommenderEmptyInput) + '</p>';
      return;
    }

    var originalText = button.textContent;
    button.disabled = true;
    button.textContent = s.recommenderSearching;
    resultsEl.innerHTML = '<p>' + esc(s.recommenderSearching) + '</p>';

    loadKeywords()
      .then(function (keywords) {
        var extracted = extractKeywords(input, keywords.synonyms);
        var expandedTokens = expandSynonyms(extracted, keywords.synonyms);
        var matchedIntents = matchIntents(input, keywords.intentKeywords);

        var results = charts
          .map(function (chart) {
            var text = chartText(chart);
            var score = 0;
            expandedTokens.forEach(function (token) {
              var tokenLower = token.toLowerCase();
              if (text.indexOf(tokenLower) !== -1) score += 1;
            });
            score += matchedIntents.length * 2;
            expandedTokens.forEach(function (token) {
              if (String(chart.Type).indexOf(token) !== -1 || String(chart.Type).toLowerCase().indexOf(token.toLowerCase()) !== -1) score += 3;
            });
            return {
              chart: chart,
              score: score,
              matched: expandedTokens.filter(function (token) {
                return text.indexOf(String(token).toLowerCase()) !== -1;
              })
            };
          })
          .filter(function (result) {
            return result.score > 0;
          })
          .sort(function (a, b) {
            return b.score - a.score;
          })
          .slice(0, 5);

        if (!results.length) {
          resultsEl.innerHTML = '<p>' + esc(s.recommenderNoResults) + '</p>';
          return;
        }

        resultsEl.innerHTML =
          '<p><strong>' + esc(format(s.recommenderResultsTitle, { n: results.length })) + '</strong></p>' +
          results
            .map(function (result) {
              return (
                '<div class="bz-recommender-card">' +
                '<img src="' + esc(result.chart.Image_url) + '" alt="' + esc(result.chart.Name) + '" loading="lazy" data-bz-lb-src="' + esc(result.chart.Image_url) + '" data-bz-lb-name="' + esc(result.chart.Name) + '">' +
                '<div class="bz-rec-body">' +
                '<a href="' + esc(result.chart.Tutorial_url) + '" target="_blank" rel="noopener">' + esc(result.chart.Name) + '</a>' +
                '<p class="bz-rec-desc">' + esc(result.chart.Description) + '</p>' +
                '<p class="bz-rec-match">' + esc(s.recommenderMatched) + ' ' + esc(result.matched.slice(0, 5).join(', ')) + '</p>' +
                '</div>' +
                '</div>'
              );
            })
            .join('');
      })
      .catch(function (error) {
        console.error('Chart recommendation failed:', error);
        resultsEl.innerHTML = '<p>' + esc(s.recommenderError) + '</p>';
      })
      .finally(function () {
        button.disabled = false;
        button.textContent = originalText;
      });
  }

  function initRecommender() {
    var results = document.getElementById('bz-recommender-results');
    if (!results) return;
    results.addEventListener('click', function (event) {
      var image = event.target.closest('img[data-bz-lb-src]');
      if (image) openLightbox(image.getAttribute('data-bz-lb-src'), image.getAttribute('data-bz-lb-name'));
    });
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
    initRecommender();
  }

  window.bzShowLightbox = openLightbox;
  window.bzRecommendCharts = recommendCharts;

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
