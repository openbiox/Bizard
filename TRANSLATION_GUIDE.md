# 自动翻译系统设置指南 / Auto-Translation Setup Guide

[English](#english) | [中文](#chinese)

---

<a name="english"></a>
## English

### Quick Start

This repository includes an automatic translation system that translates QMD files between English and Chinese when you create or update pull requests.

### How It Works

1. **When you add/modify a `.qmd` file in a PR:**
   - If you add `Tutorial.qmd` (English), the system automatically creates `Tutorial.zh.qmd` (Chinese)
   - If you add `Tutorial.zh.qmd` (Chinese), the system automatically creates `Tutorial.qmd` (English)

2. **When you add both language versions:**
   - The system validates both files for spelling and grammar
   - It checks for consistency between the two versions

3. **The translation is added to your PR automatically**
   - A bot will commit the translated file
   - You can review and edit the translation if needed

### File Naming Convention

- **English files**: `filename.qmd`
- **Chinese files**: `filename.zh.qmd`

Examples:
- `BarPlot.qmd` ↔ `BarPlot.zh.qmd`
- `About.qmd` ↔ `About.zh.qmd`

### What Gets Translated

✅ **Translated:**
- Text content (paragraphs, headings, lists)
- Image captions and alt text
- YAML frontmatter (title, description, etc.)

❌ **NOT Translated:**
- Code blocks (R, Python, etc.)
- URLs and links
- File paths
- Package names

### Excluding Files from Translation

To prevent certain files from being automatically translated, add them to `.github/translation-blacklist.txt`:

```
# Add one pattern per line
README.md
Template/**
**/test-*.qmd
```

### Reviewing Translations

The AI translation is good but not perfect. Please review:
- **Technical terms**: Ensure biomedical terminology is correct
- **Code comments**: Verify they make sense in context
- **Cultural adaptation**: Some phrases may need adjustment
- **Formatting**: Check that markdown is preserved correctly

To edit a translation:
1. Open the translated file in your PR
2. Click "Edit file"
3. Make your corrections
4. Commit the changes

### Requirements for Maintainers

The workflow supports multiple AI providers:

**Option 1: OpenAI (Default)**
1. Obtain an API key from [OpenAI Platform](https://platform.openai.com/)
2. Go to repository Settings → Secrets and variables → Actions
3. Create a secret named `OPENAI_API_KEY`
4. Paste your API key

**Option 2: Alternative Providers (e.g., Xiaomi MiMo)**
1. Create three secrets in repository settings:
   - `AI_Model_API_KEY`: Your API key
   - `AI_Model_BASE_URL`: API endpoint (e.g., `https://api.xiaomimomo.com/v1`)
   - `AI_Model_Name`: Model name (e.g., `mimo-v2-flash`)

**Note:** GitHub Copilot models are not directly accessible in GitHub Actions. Use OpenAI or alternative providers.

### Cost Estimation

- Default model: `gpt-4o-mini` (optimized for cost)
- Average cost per translation: ~$0.01-0.05 USD (OpenAI pricing)
- Monthly cost (estimated): $5-20 USD depending on PR volume
- Alternative providers may have different pricing

---

<a name="chinese"></a>
## 中文

### 快速开始

本仓库包含自动翻译系统，在您创建或更新拉取请求时，自动在英文和中文之间翻译 QMD 文件。

### 工作原理

1. **当您在 PR 中添加/修改 `.qmd` 文件时：**
   - 如果您添加 `Tutorial.qmd`（英文），系统自动创建 `Tutorial.zh.qmd`（中文）
   - 如果您添加 `Tutorial.zh.qmd`（中文），系统自动创建 `Tutorial.qmd`（英文）

2. **当您同时添加两种语言版本时：**
   - 系统验证两个文件的拼写和语法
   - 检查两个版本之间的一致性

3. **翻译会自动添加到您的 PR 中**
   - 机器人会提交翻译后的文件
   - 您可以审查并在需要时编辑翻译

### 文件命名规范

- **英文文件**：`filename.qmd`
- **中文文件**：`filename.zh.qmd`

示例：
- `BarPlot.qmd` ↔ `BarPlot.zh.qmd`
- `About.qmd` ↔ `About.zh.qmd`

### 翻译内容

✅ **会被翻译：**
- 文本内容（段落、标题、列表）
- 图片说明和替代文本
- YAML 前置内容（标题、描述等）

❌ **不会被翻译：**
- 代码块（R、Python 等）
- URL 和链接
- 文件路径
- 包名称

### 排除特定文件

要防止某些文件被自动翻译，请将它们添加到 `.github/translation-blacklist.txt`：

```
# 每行一个模式
README.md
Template/**
**/test-*.qmd
```

### 审查翻译

AI 翻译质量不错，但并非完美。请审查：
- **专业术语**：确保生物医学术语正确
- **代码注释**：验证在上下文中是否合理
- **文化适配**：某些短语可能需要调整
- **格式**：检查 markdown 是否正确保留

编辑翻译：
1. 在 PR 中打开翻译后的文件
2. 点击"编辑文件"
3. 进行修正
4. 提交更改

### 维护者要求

工作流支持多种 AI 提供商：

**选项 1：OpenAI（默认）**
1. 从 [OpenAI Platform](https://platform.openai.com/) 获取 API 密钥
2. 转到仓库设置 → Secrets and variables → Actions
3. 创建名为 `OPENAI_API_KEY` 的密钥
4. 粘贴您的 API 密钥

**选项 2：替代提供商（如小米 MiMo）**
1. 在仓库设置中创建三个密钥：
   - `AI_Model_API_KEY`：您的 API 密钥
   - `AI_Model_BASE_URL`：API 端点（如 `https://api.xiaomimomo.com/v1`）
   - `AI_Model_Name`：模型名称（如 `mimo-v2-flash`）

**注意：** GitHub Copilot 模型无法直接在 GitHub Actions 中访问。请使用 OpenAI 或其他替代提供商。

### 成本估算

- 默认模型：`gpt-4o-mini`（成本优化）
- 每次翻译平均成本：约 0.01-0.05 美元（OpenAI 定价）
- 月度成本（估算）：5-20 美元，取决于 PR 数量
- 替代提供商可能有不同的定价

---

## Support / 支持

For issues or questions / 如有问题：
- Open an issue in the repository / 在仓库中开启 issue
- Tag maintainers: @ShixiangWang
- See documentation: `.github/scripts/README.md`
