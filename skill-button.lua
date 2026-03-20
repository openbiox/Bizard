-- skill-button.lua
-- Adds a "Get AI Skill" button to each tutorial page that loads
-- skill data from skills/index.json

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

  local esc_title = title:gsub("'", "\\'")

  local button_html = string.format([[
<div class="skill-button-container" style="margin:1rem 0;padding:0.8rem 1rem;background:linear-gradient(135deg,#f0f7ff,#e8f4f8);border-radius:8px;border:1px solid #d0e3f0;">
  <div style="display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:0.5rem;">
    <span style="font-size:0.9rem;color:#2c5282;">
      <strong>🤖 AI Skill</strong> — Copy this tutorial's skill into your AI assistant
    </span>
    <button onclick="document.getElementById('tutorial-skill-modal').style.display='flex'"
      style="background:#3a86ff;color:#fff;border:none;padding:0.45rem 1rem;border-radius:6px;cursor:pointer;font-size:0.85rem;font-weight:600;white-space:nowrap;">
      📋 Get AI Skill
    </button>
  </div>
</div>
<div id="tutorial-skill-modal" style="display:none;position:fixed;inset:0;background:rgba(0,0,0,.45);z-index:9999;overflow-y:auto;padding:2rem 1rem;justify-content:center;align-items:flex-start;">
  <div style="max-width:680px;width:100%%;margin-top:5vh;background:#fff;border-radius:10px;padding:1.5rem 2rem;position:relative;">
    <button onclick="document.getElementById('tutorial-skill-modal').style.display='none'"
      style="position:absolute;top:0.8rem;right:1rem;background:#f0f0f0;border:none;border-radius:4px;padding:0.3rem 0.7rem;cursor:pointer;font-size:1rem;">✕</button>
    <h3 style="margin-top:0;">📋 AI Skill: %s</h3>
    <p style="font-size:0.85rem;color:#666;">Copy the text below into your AI assistant's context window.</p>
    <pre id="tutorial-skill-content" style="background:#f7f7f7;padding:1rem;border-radius:6px;font-size:0.8rem;overflow-x:auto;white-space:pre-wrap;max-height:50vh;overflow-y:auto;">Loading skill...</pre>
    <div style="margin-top:0.8rem;display:flex;gap:0.5rem;">
      <button onclick="navigator.clipboard.writeText(document.getElementById('tutorial-skill-content').textContent).then(function(){this.textContent='✅ Copied!';var b=this;setTimeout(function(){b.textContent='📋 Copy Skill'},2000)}.bind(this))"
        style="background:#3a86ff;color:#fff;border:none;padding:0.45rem 1rem;border-radius:6px;cursor:pointer;font-weight:600;">📋 Copy Skill</button>
    </div>
  </div>
</div>
<script>
(function() {
  var title = '%s';
  var jsonUrl = (window.location.pathname.split('/').length > 3 ? '../../' : '../') + 'skills/index.json';
  fetch(jsonUrl).then(function(r){return r.ok?r.json():[]}).catch(function(){return []}).then(function(skills){
    var pageName = location.pathname.split('/').pop().replace('.html','');
    var match = skills.find(function(s){
      return s.name===title || s.source_file.replace('.qmd','').split('/').pop()===pageName;
    });
    var el = document.getElementById('tutorial-skill-content');
    if(match && el){
      el.textContent = '# Skill: '+match.name+' ('+(match.language||'R')+')\n\n'
        +'## Category\n'+match.category+'\n\n'
        +'## When to use\n'+match.use_when+'\n\n'
        +'## Required packages\n'+(match.packages.length?match.packages.map(function(p){return '- '+p}).join('\n'):'(see tutorial)')+'\n\n'
        +'## Full tutorial\n'+match.tutorial_url;
    } else if(el) {
      el.textContent='Skill not yet generated. Run generate_skills.py to create it.';
    }
  });
})();
</script>
]], esc_title, esc_title)

  local new_blocks = {pandoc.RawBlock("html", button_html)}
  for _, block in ipairs(doc.blocks) do
    table.insert(new_blocks, block)
  end
  doc.blocks = new_blocks
  return doc
end
