# AI Provider Configuration Examples

This document provides configuration examples for different AI providers.

## Supported Providers

The translation system supports any OpenAI-compatible API. Configure using environment variables or GitHub secrets.

### OpenAI (Default)

**GitHub Secrets:**
```
OPENAI_API_KEY: sk-...your-openai-key...
```

**Environment Variables:**
```bash
export OPENAI_API_KEY="sk-...your-openai-key..."
```

**Cost:** ~$0.01-0.05 per translation with gpt-4o-mini

---

### Xiaomi MiMo (小米大模型)

**GitHub Secrets:**
```
AI_Model_API_KEY: your-mimo-api-key
AI_Model_BASE_URL: https://api.xiaomimomo.com/v1
AI_Model_Name: mimo-v2-flash
```

**Environment Variables:**
```bash
export AI_Model_API_KEY="your-mimo-api-key"
export AI_Model_BASE_URL="https://api.xiaomimomo.com/v1"
export AI_Model_Name="mimo-v2-flash"
```

**Manual Script Usage:**
```python
import os
from openai import OpenAI

client = OpenAI(
    api_key=os.environ.get("AI_Model_API_KEY"),
    base_url="https://api.xiaomimomo.com/v1"
)

# The translation script handles this automatically
```

**Available Models:**
- `mimo-v2-flash` - Fast, cost-effective
- `mimo-v2-pro` - Higher quality

**获取密钥 (Get API Key):** Check Xiaomi AI Platform for API access

---

### DeepSeek (深度求索)

**GitHub Secrets:**
```
AI_Model_API_KEY: your-deepseek-key
AI_Model_BASE_URL: https://api.deepseek.com/v1
AI_Model_Name: deepseek-chat
```

**Environment Variables:**
```bash
export AI_Model_API_KEY="your-deepseek-key"
export AI_Model_BASE_URL="https://api.deepseek.com/v1"
export AI_Model_Name="deepseek-chat"
```

**Available Models:**
- `deepseek-chat` - General purpose
- `deepseek-coder` - Code-focused

---

### Alibaba Cloud / 阿里云 (通义千问)

**GitHub Secrets:**
```
AI_Model_API_KEY: your-dashscope-key
AI_Model_BASE_URL: https://dashscope.aliyuncs.com/compatible-mode/v1
AI_Model_Name: qwen-turbo
```

**Environment Variables:**
```bash
export AI_Model_API_KEY="your-dashscope-key"
export AI_Model_BASE_URL="https://dashscope.aliyuncs.com/compatible-mode/v1"
export AI_Model_Name="qwen-turbo"
```

**Available Models:**
- `qwen-turbo` - Fast and affordable
- `qwen-plus` - Balanced quality
- `qwen-max` - Best quality

---

### Other OpenAI-Compatible Providers

Any provider with OpenAI-compatible API can be used:

1. **Azure OpenAI**
   ```
   AI_Model_BASE_URL: https://your-resource.openai.azure.com/
   AI_Model_API_KEY: your-azure-key
   AI_Model_Name: gpt-4
   ```

2. **Custom Local Models (e.g., with LM Studio, Ollama)**
   ```
   AI_Model_BASE_URL: http://localhost:1234/v1
   AI_Model_API_KEY: not-needed
   AI_Model_Name: your-local-model
   ```

---

## Configuration Priority

The system checks environment variables in this order:

1. **Command line arguments** (highest priority)
   ```bash
   python translate_qmd.py file.qmd --api-key KEY --base-url URL --model MODEL
   ```

2. **New environment variables**
   - `AI_Model_API_KEY`
   - `AI_Model_BASE_URL`
   - `AI_Model_Name`

3. **Legacy environment variable** (backward compatibility)
   - `OPENAI_API_KEY`

4. **Default values** (lowest priority)
   - Model: `gpt-4o-mini`
   - Base URL: OpenAI's default

---

## Testing Configuration

Test your configuration before using in production:

```bash
# Set your environment variables
export AI_Model_API_KEY="your-key"
export AI_Model_BASE_URL="your-base-url"
export AI_Model_Name="your-model"

# Create a test file
echo '---
title: Test
---

This is a test.' > test.qmd

# Run translation
python .github/scripts/translate_qmd.py test.qmd

# Check the output
cat test.zh.qmd
```

---

## Cost Comparison

Approximate costs per 1000 translations (varies by content length):

| Provider | Model | Cost (USD) | Speed | Quality |
|----------|-------|------------|-------|---------|
| OpenAI | gpt-4o-mini | $10-50 | Fast | High |
| OpenAI | gpt-4o | $30-150 | Medium | Very High |
| Xiaomi MiMo | mimo-v2-flash | Check provider | Very Fast | Good |
| DeepSeek | deepseek-chat | $1-10 | Fast | Good |
| Alibaba | qwen-turbo | $5-20 | Fast | Good |

*Note: Costs are estimates and subject to change. Check provider pricing.*

---

## Troubleshooting

### Error: "API key not valid"
- Verify your API key is correct
- Check if you need to activate/enable the API in provider console
- Ensure sufficient credits/quota

### Error: "Model not found"
- Verify the model name is correct for your provider
- Check provider documentation for available models
- Some providers require model activation

### Error: "Connection refused"
- Verify the base_url is correct
- Check if provider is accessible from GitHub Actions
- Some providers may block GitHub IPs

### Translation quality issues
- Try a more powerful model (e.g., mimo-v2-pro instead of mimo-v2-flash)
- Adjust temperature in the code (lower = more consistent)
- Add domain-specific context to the system prompt

---

## Contributing

To add support for a new provider:

1. Test the provider with OpenAI-compatible API
2. Document the configuration in this file
3. Add troubleshooting tips if needed
4. Submit a PR with your changes
