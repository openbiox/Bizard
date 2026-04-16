-- skill-button.lua
-- Adds a unified "Download Bizard AI Skill" button to each tutorial page.
-- Instead of loading per-tutorial skills, this provides a single ZIP download
-- containing the complete Bizard skill document and gallery data.

function Pandoc(doc)
  -- Only add to documents with a title
  if not doc.meta.title then return doc end

  local title = pandoc.utils.stringify(doc.meta.title)

  -- Skip non-tutorial pages
  local doc_path = os.getenv("QUARTO_DOCUMENT_PATH") or ""
  local skip = {"index", "About", "Skills", "Tutorial", "GraphGallery", "ToolLinks", "_render"}
  for _, pat in ipairs(skip) do
    if doc_path:find(pat) then return doc end
  end

  local button_html = [[
<div class="skill-button-container" style="margin:1rem 0;padding:0.8rem 1rem;background:linear-gradient(135deg,#f0f7ff,#e8f4f8);border-radius:8px;border:1px solid #d0e3f0;">
  <div style="display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:0.5rem;">
    <span style="font-size:0.9rem;color:#2c5282;">
      <strong>🤖 AI Skill</strong> — Download the Bizard unified skill for your AI assistant
    </span>
    <a id="bizard-skill-download" href="bizard-skill.zip" download
      style="background:#3a86ff;color:#fff;border:none;padding:0.45rem 1rem;border-radius:6px;cursor:pointer;font-size:0.85rem;font-weight:600;white-space:nowrap;text-decoration:none;display:inline-flex;align-items:center;gap:0.3rem;">
      ⬇️ Download Skill ZIP
    </a>
  </div>
</div>
<script>
(function() {
  // Compute the correct relative path to the site root based on URL depth.
  // Pages can be at Category/Page.html (depth 1) or zh/Category/Page.html (depth 2).
  var parts = window.location.pathname.replace(/\/+$/, '').split('/');
  // parts: ['', 'Bizard', 'Category', 'Page.html'] or ['', 'Bizard', 'zh', 'Category', 'Page.html']
  // We need to go up from the page to the site root (where bizard-skill.zip lives).
  // Count path segments after the site root.
  var segmentsAfterRoot = parts.length - 2; // minus '' and site-root
  var prefix = '';
  for (var i = 0; i < segmentsAfterRoot; i++) prefix += '../';
  if (!prefix) prefix = './';
  var link = document.getElementById('bizard-skill-download');
  if (link) link.href = prefix + 'bizard-skill.zip';
})();
</script>
]]

  local new_blocks = {pandoc.RawBlock("html", button_html)}
  for _, block in ipairs(doc.blocks) do
    table.insert(new_blocks, block)
  end
  doc.blocks = new_blocks
  return doc
end

