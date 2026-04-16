# Bizard AI Skill — Installation Guide

## What's in this package?

| File | Purpose |
|------|---------|
| `skill.md` | The AI skill instruction document — load this into your AI assistant |
| `gallery_data.csv` | 793 visualization examples with image URLs, tutorial links, descriptions, and categories |
| `gallery_data_zh.csv` | Chinese version of the gallery data |
| `README.md` | This file |

## Quick Start

### ChatGPT / GPT-4

1. Go to **Settings → Personalization → Custom Instructions** (or create a GPT)
2. Paste the contents of `skill.md` into the system prompt / instructions
3. Upload `gallery_data.csv` as a knowledge file
4. Ask: *"I want to compare gene expression across cancer subtypes"*

### Claude

1. Start a new project or conversation
2. Upload `skill.md` and `gallery_data.csv` as project files
3. Ask: *"Help me create a volcano plot for my differential expression results"*

### GitHub Copilot Chat

1. Add `skill.md` and `gallery_data.csv` to your workspace
2. Reference them with `#file:skill.md` in Copilot Chat
3. Ask: *"Based on the Bizard skill, suggest a visualization for my survival data"*

### Local LLMs (Ollama, LM Studio, etc.)

1. Load `skill.md` as part of your system prompt
2. Provide `gallery_data.csv` as context when asking visualization questions
3. The skill will guide the model to recommend appropriate chart types and code

## What can this skill do?

Once installed, your AI assistant can:

- **Recommend** the best visualization type for your biomedical data
- **Generate** reproducible R / Python / Julia code based on Bizard tutorials
- **Link** to the full tutorial at https://openbiox.github.io/Bizard/ for detailed customization
- **Search** the gallery data to find specific figure examples

## Learn More

- **Website**: https://openbiox.github.io/Bizard/
- **Repository**: https://github.com/openbiox/Bizard
- **License**: CC-BY-NC — Bizard Collaboration Group, Luo Lab, and Wang Lab
