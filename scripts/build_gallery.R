# Shared builder for the Graph Gallery pages (English and Chinese).
# Each page sources this file and calls build_gallery_html() with `results: asis`.

humanize_family <- function(key) {
  key <- gsub("-", " ", key)
  key <- gsub("(?<=[a-z])(?=[A-Z])", " ", key, perl = TRUE)
  gsub("(?<=[A-Z])(?=[A-Z][a-z])", " ", key, perl = TRUE)
}

gallery_strings <- function(lang = c("en", "zh")) {
  lang <- match.arg(lang)
  if (lang == "en") {
    list(
      all = "All",
      searchPlaceholder = "Search by chart name, type, keyword, or data scenario...",
      searchButton = "Search",
      clearButton = "Clear",
      showingAll = "Showing {total} charts",
      showing = "Showing {n} of {total} charts",
      noResults = "No charts found for \u201c{query}\u201d. Try different keywords or clear the filters.",
      byCollection = "Browse by collection",
      byFamily = "Filter by chart family",
      zoom = "Click to zoom in",
      typeLabel = "Type",
      categoryLabel = "Category",
      searchLabel = "Search charts",
      recommenderPlaceholder = "e.g., I want to compare gene expression between different cancer types...",
      recommenderButton = "Recommend Charts",
      recommenderEmptyInput = "Please enter a description of your visualization goal.",
      recommenderSearching = "Searching for matching charts...",
      recommenderResultsTitle = "Matching results ({n}):",
      recommenderNoResults = "No matching charts found. Try different keywords.",
      recommenderMatched = "Matched keywords:",
      recommenderError = "An error occurred while searching. Please try again."
    )
  } else {
    list(
      all = "全部",
      searchPlaceholder = "按图表名称、类型、关键词或数据场景搜索...",
      searchButton = "搜索",
      clearButton = "清除",
      showingAll = "共 {total} 张图表",
      showing = "显示 {n} / {total} 张图表",
      noResults = "没有找到与\u201c{query}\u201d匹配的图表，请尝试其他关键词或清除筛选条件。",
      byCollection = "按合集浏览",
      byFamily = "按图表类别筛选",
      zoom = "点击放大预览",
      typeLabel = "类型",
      categoryLabel = "类别",
      searchLabel = "搜索图表",
      recommenderPlaceholder = "例如：我想比较不同癌症类型之间的基因表达差异...",
      recommenderButton = "推荐图表",
      recommenderEmptyInput = "请先描述您的可视化目标。",
      recommenderSearching = "正在搜索匹配的图表...",
      recommenderResultsTitle = "匹配结果（{n}）：",
      recommenderNoResults = "没有找到匹配的图表，请尝试其他关键词。",
      recommenderMatched = "匹配关键词：",
      recommenderError = "搜索时出现错误，请重试。"
    )
  }
}

build_gallery_html <- function(csv_path, lang = c("en", "zh")) {
  lang <- match.arg(lang)

  gallery <- read.csv(csv_path, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  gallery[is.na(gallery)] <- ""

  basics_families <- c(
    "Distribution", "Correlation", "Ranking", "Composition",
    "Evolution", "Proportion", "Animation"
  )

  family_key <- ifelse(
    gallery$Level1 == "BASICS" & gallery$Level2 %in% basics_families,
    gallery$Level2,
    ifelse(gallery$Level1 %in% c("CLINICS", "OMICS"), gallery$Level2, gallery$Level1)
  )

  if (lang == "en") {
    family_label <- humanize_family(family_key)
    family_label[family_key == "BASICS"] <- "Other Basics"
    family_label[family_key == "HIPLOT"] <- "Hiplot"
    family_label[family_key == "PYTHON"] <- "Python"
    family_label[family_key == "JULIA"] <- "Julia"
    family_label[family_key == "OMICS"] <- "Omics"
    family_label[family_key == "CLINICS"] <- "Clinics"
    collection_labels <- list(
      BASICS = "BASICS", OMICS = "Omics", CLINICS = "Clinics",
      PYTHON = "Python", HIPLOT = "Hiplot", JULIA = "Julia"
    )
  } else {
    zh_family_map <- c(
      Distribution = "分布图", Correlation = "相关图", Ranking = "排序图",
      Composition = "组合图", Evolution = "演化图", Proportion = "比例图",
      Animation = "动画图", BASICS = "其他基础图", OMICS = "组学图",
      CLINICS = "临床图", PYTHON = "Python 图", HIPLOT = "Hiplot 图",
      JULIA = "Julia 图"
    )
    family_type <- tapply(gallery$Type, family_key, function(types) types[[1]])
    fallback <- unname(family_type[family_key])
    mapped <- unname(zh_family_map[family_key])
    family_label <- ifelse(
      is.na(mapped),
      ifelse(is.na(fallback) | fallback == "", humanize_family(family_key), fallback),
      mapped
    )
    collection_labels <- list(
      BASICS = "基础图表", OMICS = "组学", CLINICS = "临床",
      PYTHON = "Python", HIPLOT = "Hiplot", JULIA = "Julia"
    )
  }

  gallery$Family <- family_key
  gallery$FamilyLabel <- family_label

  strings <- gallery_strings(lang)

  config <- list(
    strings = strings,
    collectionLabels = collection_labels,
    collectionOrder = c("BASICS", "OMICS", "CLINICS", "PYTHON", "HIPLOT", "JULIA"),
    recommender = list(
      keywordsEn = "files/recommender_keywords.json",
      keywordsZh = "files/recommender_keywords_zh.json"
    )
  )

  data_json <- jsonlite::toJSON(gallery, na = "null")
  data_json <- gsub("<", "\\u003c", data_json, fixed = TRUE)
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE)
  config_json <- gsub("<", "\\u003c", config_json, fixed = TRUE)

  paste0(
    '<link rel="stylesheet" href="files/gallery/gallery.css">\n',
    '<script src="files/gallery/gallery.js" defer></script>\n',
    '<script type="application/json" id="bz-gallery-data">', data_json, '</script>\n',
    '<script type="application/json" id="bz-gallery-config">', config_json, '</script>\n',
    # The whole gallery DOM must be emitted on a single line: Pandoc re-parses
    # multi-line raw HTML and wraps fragments in <p> tags.
    paste0(
      '<div class="bz-gallery">',
      '<div class="bz-search-card">',
      '<div class="bz-search-wrapper">',
      '<input type="text" id="bz-search-input" placeholder="', strings$searchPlaceholder, '" aria-label="', strings$searchLabel, '" autocomplete="off">',
      '<button type="button" class="bz-btn bz-btn-primary" id="bz-search-btn">', strings$searchButton, '</button>',
      '<button type="button" class="bz-btn bz-btn-secondary" id="bz-clear-btn">', strings$clearButton, '</button>',
      '</div>',
      '<div class="bz-result-count" id="bz-result-count" aria-live="polite"></div>',
      '<div class="bz-filter-group">',
      '<div class="bz-filter-title">', strings$byCollection, '</div>',
      '<div class="bz-pill-row" id="bz-collection-pills"></div>',
      '</div>',
      '<div class="bz-filter-group">',
      '<div class="bz-filter-title">', strings$byFamily, '</div>',
      '<div class="bz-pill-row" id="bz-family-pills"></div>',
      '</div>',
      '</div>',
      '<div class="bz-grid" id="bz-grid"></div>',
      '<div class="bz-lightbox" id="bz-lightbox" role="dialog" aria-modal="true">',
      '<span class="bz-lightbox-close" aria-hidden="true">&times;</span>',
      '<img id="bz-lightbox-image" alt="">',
      '</div>',
      '</div>'
    )
  )
}

build_recommender_html <- function(lang = c("en", "zh")) {
  lang <- match.arg(lang)
  strings <- gallery_strings(lang)

  paste0(
    '<div class="bz-gallery">',
    '<div class="bz-recommender">',
    '<textarea id="bz-recommender-input" rows="3" placeholder="', strings$recommenderPlaceholder, '"></textarea>',
    '<div class="bz-recommender-actions"><button type="button" class="bz-btn bz-btn-primary" id="bz-recommender-btn" onclick="bzRecommendCharts()">', strings$recommenderButton, '</button></div>',
    '<div class="bz-recommender-results" id="bz-recommender-results" aria-live="polite"></div>',
    '</div>',
    '</div>'
  )
}
