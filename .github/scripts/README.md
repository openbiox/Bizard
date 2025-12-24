# Automatic QMD Translation System

This directory contains the automatic translation system for Bizard's bilingual documentation.

## Overview

The translation system automatically:
- Detects new or modified QMD files in pull requests
- Translates between English and Chinese using AI (OpenAI GPT models)
- Preserves code blocks, YAML frontmatter, and markdown formatting
- Checks for spelling errors and typos
- Commits translated files back to the PR

## Files

- **`translate_qmd.py`**: Python script for translating QMD files
- **`translation-blacklist.txt`**: Files/patterns to exclude from automatic translation
- **`../workflows/auto-translate.yml`**: GitHub Action workflow

## How It Works

### Automatic Translation Workflow

1. When a PR is opened or updated with `.qmd` file changes:
   - The workflow detects all changed QMD files
   - For each file without a translation pair, it automatically generates the translation
   - For files with both language versions, it performs validation and spell-checking

2. File naming convention:
   - English files: `filename.qmd`
   - Chinese files: `filename.zh.qmd`

3. The workflow commits translations back to the PR with a comment

### Translation Features

- **Language Detection**: Automatically detects source language (English or Chinese)
- **Code Preservation**: Code blocks are preserved exactly as-is
- **YAML Translation**: Translates title and other fields in frontmatter
- **Markdown Support**: Maintains all markdown formatting
- **Spell Checking**: Optional spell-check and grammar validation

## Configuration

### Setting Up Translation Provider

The workflow supports multiple AI providers for translation.

#### Option 1: OpenAI (Recommended)

1. Go to repository Settings → Secrets and variables → Actions
2. Create a new secret named `OPENAI_API_KEY`
3. Paste your OpenAI API key from [OpenAI Platform](https://platform.openai.com/)

#### Option 2: Alternative AI Providers (e.g., Xiaomi MiMo, DeepSeek)

For alternative providers, configure three secrets:

1. `AI_Model_API_KEY`: Your API key for the provider
2. `AI_Model_BASE_URL`: The base URL for the API endpoint (e.g., `https://api.xiaomimomo.com/v1`)
3. `AI_Model_Name`: The model name to use (e.g., `mimo-v2-flash`)

**Example for Xiaomi MiMo:**
- `AI_Model_API_KEY`: Your MiMo API key
- `AI_Model_BASE_URL`: `https://api.xiaomimomo.com/v1`
- `AI_Model_Name`: `mimo-v2-flash`

**Priority:** `AI_Model_*` variables → `OPENAI_API_KEY` (for backward compatibility)

### Blacklist Configuration

To exclude files from automatic translation, edit `.github/translation-blacklist.txt`:

```
# Example patterns
README.md
_quarto.yml
**/test-*.qmd
Template/**
```

Supports:
- Exact file paths: `About.qmd`
- Wildcards: `**/test-*.qmd`
- Directory exclusions: `Template/**`

## Manual Usage

You can also use the translation script manually:

```bash
# Install dependencies
pip install openai

# Option 1: Using OpenAI
export OPENAI_API_KEY="your-key-here"
python .github/scripts/translate_qmd.py input.qmd

# Option 2: Using alternative providers (e.g., Xiaomi MiMo)
export AI_Model_API_KEY="your-mimo-key"
export AI_Model_BASE_URL="https://api.xiaomimomo.com/v1"
export AI_Model_Name="mimo-v2-flash"
python .github/scripts/translate_qmd.py input.qmd

# Option 3: Using command line arguments
python .github/scripts/translate_qmd.py input.qmd \
  --api-key "your-key" \
  --base-url "https://api.xiaomimomo.com/v1" \
  --model "mimo-v2-flash"

# Translate multiple files
python .github/scripts/translate_qmd.py file1.qmd file2.qmd

# Specify target language
python .github/scripts/translate_qmd.py input.qmd --target-lang zh

# Check spelling only
python .github/scripts/translate_qmd.py input.qmd --check-spelling

# Use different model
python .github/scripts/translate_qmd.py input.qmd --model gpt-4o-mini
```

## Workflow Behavior

### Scenario 1: New English File Added

PR contains: `NewPlot.qmd` (English)

**Result**: Workflow automatically generates `NewPlot.zh.qmd` (Chinese)

### Scenario 2: New Chinese File Added

PR contains: `NewPlot.zh.qmd` (Chinese)

**Result**: Workflow automatically generates `NewPlot.qmd` (English)

### Scenario 3: Both Languages Added

PR contains: `NewPlot.qmd` and `NewPlot.zh.qmd`

**Result**: Workflow validates both files and checks for spelling errors

### Scenario 4: Updating Existing File

PR contains: `ExistingPlot.qmd` (modified), but `ExistingPlot.zh.qmd` already exists

**Result**: No automatic translation (to avoid overwriting manual edits)

## Best Practices

1. **Review AI Translations**: Always review auto-generated translations for:
   - Technical terminology accuracy
   - Biomedical term correctness
   - Cultural appropriateness
   - Code comment translations

2. **Manual Corrections**: If the auto-translation needs fixes:
   - Edit the translated file directly in the PR
   - Push your corrections
   - The workflow won't overwrite your manual edits

3. **Blacklist Usage**: Add files to blacklist if:
   - They contain sensitive information
   - They're templates or configuration files
   - They require specialized translation

4. **For Complex Documents**: Consider manual translation for:
   - Documents with extensive technical jargon
   - Legal or policy documents
   - Documents requiring specific terminology

## Troubleshooting

### Translation Fails

- **Check API Key**: Ensure `OPENAI_API_KEY` secret is set correctly
- **Check API Limits**: Verify your OpenAI account has sufficient credits
- **File Size**: Very large files may hit API token limits

### Unexpected Behavior

- **Check Blacklist**: File might be in blacklist
- **Check File Format**: Ensure file has proper QMD format with YAML frontmatter
- **Check Permissions**: Workflow needs write permissions

### Quality Issues

- **Use Better Model**: Switch from `gpt-4o-mini` to `gpt-4` for higher quality
- **Manual Review**: Some content always requires human review
- **Provide Context**: Add domain-specific terminology guide

## Contributing

To improve the translation system:

1. Test with sample files
2. Report issues with specific examples
3. Suggest improvements to prompts
4. Add language-specific handling

## Model Selection

The workflow uses `gpt-4o-mini` by default for cost efficiency. Options:

- `gpt-4o-mini`: Faster, cheaper, good for most content
- `gpt-4o`: Better quality, moderate cost
- `gpt-4`: Highest quality, higher cost

To change, edit the workflow file and modify `--model` parameter.

## Support

For issues or questions:
- Open an issue in the repository
- Tag maintainers: @ShixiangWang
- Check OpenAI API status if translations fail
