# Bizard AI Skill — Installation Guide

## What's in this package?

| File | Purpose |
|------|---------|
| `SKILL.md` | The AI skill instruction document — load this into your AI assistant |
| `gallery_data.csv` | 793 visualization examples with image URLs, tutorial links, descriptions, and categories |
| `gallery_data_zh.csv` | Chinese version of the gallery data |
| `README.md` | This file |

## Quick Start

### ChatGPT / GPT-4

1. Go to **Settings → Personalization → Custom Instructions** (or create a GPT)
2. Paste the contents of `SKILL.md` into the system prompt / instructions
3. Upload `gallery_data.csv` as a knowledge file
4. Ask: *"I want to compare gene expression across cancer subtypes"*

### Claude

1. Start a new project or conversation
2. Upload `SKILL.md` and `gallery_data.csv` as project files
3. Ask: *"Help me create a volcano plot for my differential expression results"*

### GitHub Copilot Chat

1. Add `SKILL.md` and `gallery_data.csv` to your workspace
2. Reference them with `#file:SKILL.md` in Copilot Chat
3. Ask: *"Based on the Bizard skill, suggest a visualization for my survival data"*

### Local LLMs (Ollama, LM Studio, etc.)

1. Load `SKILL.md` as part of your system prompt
2. Provide `gallery_data.csv` as context when asking visualization questions
3. The skill will guide the model to recommend appropriate chart types and code

## What can this skill do?

Once installed, your AI assistant can:

- **Recommend** the best visualization type for your biomedical data
- **Generate** reproducible R / Python / Julia code based on Bizard tutorials
- **Link** to the full tutorial at https://openbiox.github.io/Bizard/ for detailed customization
- **Search** the gallery data to find specific figure examples

## Prefer a live connection? Use the Bizard MCP server

For AI **agents** that can call tools (Claude Code, Cursor, Cline, …), the static
skill package above is not the only option — Bizard ships an MCP server,
**`@openbiox/bizard-mcp`**, that serves the live chart lifecycle over the
[Model Context Protocol](https://modelcontextprotocol.io): bilingual search across
800+ chart tutorials, runnable code with dependencies and data URLs, structured
adaptation guidance, citation metadata, and template-checked contribution — all
from a CI-generated snapshot of the same sources as this skill, so they never drift.

Quick start with Claude Code:

```bash
claude mcp add bizard -- npx @openbiox/bizard-mcp
```

Any other MCP client works too, over stdio (`npx @openbiox/bizard-mcp`) or
Streamable HTTP (`npx @openbiox/bizard-mcp --http`). Full tool list,
configuration, and privacy notes: <https://github.com/openbiox/Bizard/tree/main/mcp>

## Learn More

- **Website**: https://openbiox.github.io/Bizard/
- **Repository**: https://github.com/openbiox/Bizard
- **License**: CC-BY-NC — Bizard Collaboration Group, Luo Lab, and Wang Lab
