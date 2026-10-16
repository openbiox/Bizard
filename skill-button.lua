-- skill-button.lua
-- Adds the Bizard AI bar to every page: skill ZIP download, MCP server
-- (local + hosted) setup lines, and a citation note with the paper DOI.
-- Pages under /zh/ switch the text to Chinese at runtime.

function Pandoc(doc)
  -- Only add to documents with a title
  if not doc.meta.title then return doc end

  local button_html = [[
<div class="skill-button-container" style="margin:1rem 0;padding:0.8rem 1rem;background:linear-gradient(135deg,#f0f7ff,#e8f4f8);border-radius:8px;border:1px solid #d0e3f0;">
  <div style="display:flex;flex-direction:column;gap:0.5rem;">
    <div style="display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:0.5rem;">
      <span id="bizard-ai-skill-text" style="font-size:0.9rem;color:#2c5282;">
        <strong>🤖 AI Skill</strong> — Download the Bizard unified skill for your AI assistant
      </span>
      <a id="bizard-skill-download" href="bizard-skill.zip" download
        style="background:#3a86ff;color:#fff;border:none;padding:0.45rem 1rem;border-radius:6px;cursor:pointer;font-size:0.85rem;font-weight:600;white-space:nowrap;text-decoration:none;display:inline-flex;align-items:center;gap:0.3rem;">
        ⬇️ Download Skill ZIP
      </a>
    </div>
    <div style="display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:0.5rem;border-top:1px dashed #d0e3f0;padding-top:0.5rem;">
      <span id="bizard-mcp-text" style="font-size:0.9rem;color:#2c5282;">
        <strong>🔌 MCP Server</strong> — Let AI agents (Claude Code, Cursor, …) query Bizard live:
        <code style="background:#fff;border:1px solid #d0e3f0;border-radius:4px;padding:0.1rem 0.4rem;font-size:0.8rem;">claude mcp add bizard -- npx @openbiox/bizard-mcp</code>
        — or the free hosted instance (no install, faster from mainland China):
        <code style="background:#fff;border:1px solid #d0e3f0;border-radius:4px;padding:0.1rem 0.4rem;font-size:0.8rem;">claude mcp add --transport http bizard http://biotree.top:38123/bizard-mcp/mcp</code>
      </span>
      <a id="bizard-mcp-docs" href="https://openbiox.github.io/Bizard/Skills.html#mcp-docs" target="_blank" rel="noopener"
        style="background:#3a86ff;color:#fff;border:none;padding:0.45rem 1rem;border-radius:6px;cursor:pointer;font-size:0.85rem;font-weight:600;white-space:nowrap;text-decoration:none;display:inline-flex;align-items:center;gap:0.3rem;">
        📖 MCP Docs
      </a>
    </div>
    <div style="display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:0.5rem;border-top:1px dashed #d0e3f0;padding-top:0.5rem;">
      <span id="bizard-cite-text" style="font-size:0.9rem;color:#2c5282;">
        <strong>📖 Cite Bizard</strong> — if Bizard helps your research, please cite:
        Li, K., Zheng, H., Huang, K., et al. (2026). Bizard: A Community-Driven Platform for Accelerating and Enhancing Biomedical Data Visualization. <em>iMetaMed</em>, e70038.
      </span>
      <a href="https://doi.org/10.1002/imm3.70038" target="_blank" rel="noopener"
        style="background:#2b8a3e;color:#fff;border:none;padding:0.45rem 1rem;border-radius:6px;cursor:pointer;font-size:0.85rem;font-weight:600;white-space:nowrap;text-decoration:none;display:inline-flex;align-items:center;gap:0.3rem;">
        🔗 DOI / BibTeX
      </a>
    </div>
  </div>
</div>
<script>
(function() {
  // Compute the correct relative path to the site root based on URL depth.
  // Pages can be at Category/Page.html (depth 1) or zh/Category/Page.html (depth 2).
  var parts = window.location.pathname.replace(/\/+$/, '').split('/');
  // parts: ['', 'Bizard', 'Category', 'Page.html'] or ['', 'Bizard', 'zh', 'Category', 'Page.html']
  // We need to go up from the page to the site root (where bizard-skill.zip lives).
  // Count directory segments between site-root and the file (exclude '' + site-root + filename).
  var segmentsAfterRoot = Math.max(0, parts.length - 3); // minus '', site-root, and filename
  var prefix = '';
  for (var i = 0; i < segmentsAfterRoot; i++) prefix += '../';
  if (!prefix) prefix = './';
  var link = document.getElementById('bizard-skill-download');
  if (link) link.href = prefix + 'bizard-skill.zip';
  var isZh = window.location.pathname.indexOf('/zh/') !== -1;
  var docs = document.getElementById('bizard-mcp-docs');
  if (docs && isZh) {
    // The zh docs page lives at /zh/Skills.html — one level above the page's
    // zh root, i.e. one '../' less than the site-root prefix.
    var zhPrefix = prefix.replace(/\.\.\/$/, '');
    if (!zhPrefix) zhPrefix = './';
    docs.href = zhPrefix + 'Skills.html#mcp-docs';
  }
  if (isZh) {
    var texts = {
      'bizard-ai-skill-text': '<strong>🤖 AI 技能包</strong> — 为你的 AI 助手下载 Bizard 统一技能包',
      'bizard-mcp-text': '<strong>🔌 MCP 服务器</strong> — 让 AI 智能体（Claude Code、Cursor 等）实时查询 Bizard：' +
        '<code style="background:#fff;border:1px solid #d0e3f0;border-radius:4px;padding:0.1rem 0.4rem;font-size:0.8rem;">claude mcp add bizard -- npx @openbiox/bizard-mcp</code>' +
        ' — 或免安装托管实例（大陆访问更快）：' +
        '<code style="background:#fff;border:1px solid #d0e3f0;border-radius:4px;padding:0.1rem 0.4rem;font-size:0.8rem;">claude mcp add --transport http bizard http://biotree.top:38123/bizard-mcp/mcp</code>',
      'bizard-cite-text': '<strong>📖 引用 Bizard</strong> — 如果 Bizard 对你的研究有帮助，请引用：' +
        'Li, K., Zheng, H., Huang, K., et al. (2026). Bizard: A Community-Driven Platform for Accelerating and Enhancing Biomedical Data Visualization. <em>iMetaMed</em>, e70038.'
    };
    for (var id in texts) {
      var el = document.getElementById(id);
      if (el) el.innerHTML = texts[id];
    }
    if (link) link.innerHTML = '⬇️ 下载技能包';
    if (docs) docs.innerHTML = '📖 MCP 文档';
  }
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
