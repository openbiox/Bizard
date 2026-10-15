# Bizard MCP:覆盖用户全生命周期的图表检索与贡献服务 — 设计文档

- 状态:Draft v1(2026-10-15,brainstorming 产出,待用户终审)
- 范围:仅设计与规划,**不含实现**(用户明确要求)
- 关联:issue #305(MCP + ScientificFigureLibrary 先例)、issue #111(AI 集成构想)、issue #4(积分制)、issue #181(标准化流程)

---

## 1. 目标与问题定义

### 1.1 要解决的问题

Bizard 现有 AI 交付物(静态 `SKILL.md` + `gallery_data.csv` ZIP、客户端关键词推荐器)存在结构性局限:

1. **图文参考性差距**(用户原话:"官网图文的参考性与用户实际使用的有出入"):静态教程展示的是教程自带示例数据上的"完美输出",用户拿到自己的真实数据后,数据形状、包版本、依赖安装、参数选择都会出问题。静态文件无法感知用户数据,无法给出行级适配指引。
2. **无使用反馈回路**:团队不知道哪些图被真正"采用",哪些教程在 AI 辅助复现中频繁失败,内容改进没有数据依据。
3. **无贡献通路**:AI 交互中天然产生的"新图变体、纠错、翻译"没有结构化通道回流到仓库;贡献流程(积分制、模板)对 AI 不可见。
4. **接入形态受限**:ZIP 需要手动安装且内容会过期;不支持 MCP 的用户与支持 MCP 的用户看到的能力差异大。

### 1.2 目标

设计一个 MCP(Model Context Protocol)服务器,覆盖用户全生命周期四阶段:

| 阶段 | 用户问题 | MCP 能力 |
|------|---------|---------|
| **查询** | 我想找某种图 | 双语检索 gallery(798+ 图表)与教程 |
| **使用** | 我要在我自己的数据上出这张图 | 返回可运行代码、依赖、数据 URL + 结构化适配指引(宿主 AI 执行) |
| **参考** | 论文/报告中如何规范引用 | 引用元数据(BibTeX、DOI、URL、许可声明) |
| **贡献** | 我发现问题/想新增教程 | 模板校验 + 以用户身份发起 issue/PR(代理认证) |

### 1.3 非目标(一期明确不做)

- ❌ 服务器端执行绘图代码(用户已确认:宿主 AI 执行,SFL 同款原则)
- ❌ 服务器端遥测/用户数据收集(用户已确认:仅本地匿名记录)
- ❌ OAuth 账号体系、用户管理
- ❌ MCP Apps 交互式 UI(列为二期候选,工具 schema 预留 `_meta` 扩展位)
- ❌ 取代 SKILL.md / bizard-skill.zip(用户已确认:同源生成,skill 兼容共存)

### 1.4 成功标准

1. 本地用户:`npx @openbiox/bizard-mcp` 一条命令接入 Claude Code/Cursor,四阶段工具全部可用,离线时检索降级可用。
2. 远程用户:官方托管实例通过 Streamable HTTP 接入 claude.ai/ChatGPT,零安装,查询/使用/参考完整可用,贡献自动降级为草稿+deep-link。
3. 数据一致性:MCP 内容与 SKILL.md、官网渲染内容来自同一 CI 产物,三方零漂移。
4. 反馈可用:本地 JSONL 记录被检索/查看/适配的图表 ID,贡献时可附带,为内容改进提供依据。
5. 贡献通路:从 AI 对话到 GitHub issue/PR 的闭环,落地为用户本人身份,兼容现有积分审核流程。

---

## 2. 调研结论摘要

### 2.1 MCP 平台现状(2026-10)

- 规范版本 2026-07-28(GA),schema 以 TypeScript 为第一表示。
- 传输:**stdio**(本地)与 **Streamable HTTP**(远程;SSE 已弃用)。stdio 服务器绝不可写 stdout 之外的日志。
- 服务器能力三件套:**Tools**(模型主动调用)、**Resources**(客户端拉取)、**Prompts**(用户触发的结构化工作流)。
- **Tool annotations**(readOnlyHint/destructiveHint/idempotentHint/openWorldHint)是**不可信提示**,仅供客户端 UI 参考,服务端仍须自行校验。
- **MCP Apps**(2026-01 首个官方扩展):工具可在沙箱 iframe 中返回交互式 UI,ChatGPT/Claude/Goose/VS Code 已支持。
- MCP Registry 与 MCPB(打包格式)可用于分发发现。

### 2.2 关键先例:ScientificFigureLibrary(SFL,issue #305 指定参考)

SFL(sfl.bio)是"科研图表库 + MCP"的成熟实现,对本设计的可借鉴点与差异点:

| SFL 做法 | Bizard MCP 的取舍 |
|---------|-------------------|
| 本地优先,全局 Library 磁盘存储 | **不需要**:Bizard 是只读内容站点,无用户私有库概念;本地仅需缓存层 |
| provider registry + Ed25519 签名 feed | **不需要**(YAGNI):Bizard 单一内容源,CI 即信任根 |
| import→review→publish 不可变 release | **部分借鉴**:内容包(bundle)按站点版本发布,不可变、可回溯 |
| 绝不执行绘图代码,宿主 agent 执行 | **采纳**(用户已确认) |
| 更新通知追加到首个工具结果 | **采纳** |
| `figure_library_search` 返回 provider 限定结果 + `exactSelector` | **借鉴**:搜索结果带 `tutorialUrl`/`selector` 精确锚点 |
| network-access.json 代理白名单 | **简化**:服务器自身仅出站拉取 bundle;贡献走用户本地 gh |
| MCP App UI | **二期候选** |

### 2.3 学术文献要点

- **MCP-Universe**(arXiv 2508.14704):真实世界 MCP 基准,表明工具描述质量与结构化返回对任务成功率影响显著 → 工具描述与返回 envelope 需精心设计。
- **"MCP Tool Descriptions Are Smelly!"**(arXiv 2602.14878):工具描述存在描述缺失/过长/含糊等坏味道 → 每个工具描述遵循"一句话功能+参数约束+返回结构"模板。
- **MCP Security Bench**(arXiv 2510.15994):工具结果注入是主要攻击面 → 所有工具返回内容视为不可信数据,贡献草稿中的用户自由文本需截断与转义。
- **EVOR / DeepCodeSeek**(2402.12317 / 2509.25716):检索增强的代码生成中,"可执行示例+环境说明"是关键资产 → `bizard_get_code` 必须返回带依赖安装守卫的完整可运行块。
- **Charts-of-Thought**(2508.04842):结构化分步提示可显著提升图表问答质量 → 支撑 Prompts 工作流设计。
- **Bizard 论文**(arXiv 2503.06845,iMetaMed e70038):引用元数据以论文为准。

### 2.4 现有资产盘点

- `SKILL.md`(9 KB):统一 AI skill,含 12 分类表、决策指南、R/Python/Julia 代码规范(install-guard 模式、COS 数据 URL)、响应格式(推荐/代码/教程链接/备选)。
- `files/gallery_data.csv`(236 KB,798 条):`Id,Name,Image_url,Tutorial_url,Description,Type,Level1,Level2`。
- `files/recommender_keywords*.json`:201 同义词条目、143 意图映射,双语。
- `generate_skills.py`:QMD→skill 离线/LLM 双模式转换器,CI 自动再生成。
- `sync-gallery.py`:图表 QMD 与 gallery CSV 自动同步。
- 积分制(issue #4):+10 教程、+3 优化、+2 翻译等,PR/issue 人工审核。
- 贡献模板:`Template/contribution_0323.md`、`visualization_guidance_*.qmd`。

---

## 3. 总体架构

### 3.1 核心决策(用户已确认)

| # | 决策点 | 结论 |
|---|--------|------|
| 1 | 双部署分工 | **同一份实现**:开发者托管服务器版,用户可本地部署完全一样的版本,任选接入方式 |
| 2 | 代码执行 | 服务器不执行,宿主 AI 执行 |
| 3 | 与 SKILL.md 关系 | 同源生成,skill 兼容共存 |
| 4 | 实现语言 | TypeScript(官方 SDK) |
| 5 | 贡献认证 | 代理发 issue/PR,按环境降级 |
| 6 | 使用反馈 | 仅本地匿名 JSONL 记录 |

### 3.2 架构图

```dot
digraph bizard_mcp {
  rankdir=LR;
  subgraph cluster_ci {
    label="GitHub Actions CI(现有管线扩展)";
    QMD [label="257 篇教程 QMD"];
    CSV [label="gallery_data*.csv"];
    KW  [label="recommender_keywords*.json"];
    GEN [label="build-bundle.py\n(新)"];
    ART [label="内容包 bundle\nmcp-bundle-v{N}.json + llms.txt"];
    OUT [label="_site 静态发布\n(GitHub Pages)"];
    QMD -> GEN; CSV -> GEN; KW -> GEN;
    GEN -> ART -> OUT;
  }
  subgraph cluster_srv {
    label="@openbiox/bizard-mcp(单包双传输)";
    CORE [label="核心层\n(工具逻辑/检索/缓存/降级)"];
    STDI [label="stdio 传输\n(本地)"];
    HTTP [label="Streamable HTTP 传输\n(托管)"];
    CORE -> STDI; CORE -> HTTP;
  }
  subgraph cluster_user {
    label="用户侧";
    HOST [label="宿主 AI\n(Claude Code/ChatGPT/…)"];
    FS   [label="本地缓存\n~/.cache/bizard-mcp/"];
    LOG  [label="匿名 JSONL\nusage-log.jsonl"];
    GH   [label="用户 gh CLI / GITHUB_TOKEN"];
  }
  OUT ->|首次运行下载\n内置快照兜底| CORE;
  CORE -> FS; CORE -> LOG;
  HOST <->
  CORE [label="MCP 协议"];
  GH ->|级别1/2:\n以用户身份发 issue/PR| REPO [label="openbiox/Bizard\nissue + PR"];
  HOST ->|级别3:\n草稿 + deep-link| REPO;
}
```

### 3.3 单包双传输

一个 npm 包 `@openbiox/bizard-mcp`,两种启动形态:

```bash
# 本地(推荐用户方式):stdio,接入任何支持 MCP 的客户端
npx @openbiox/bizard-mcp                # 默认 stdio

# 托管(Bizard 开发者运行):Streamable HTTP
BIZARD_HTTP_PORT=3000 npx @openbiox/bizard-mcp --http
```

- 传输层薄壳:入口文件按 `--http` 标志选择 `StdioServerTransport` 或 Streamable HTTP handler;所有工具逻辑在 `core/`,对传输无感知。
- 本地与托管运行**同一镜像、同一工具集、同一内容包**;唯一差异是运行时检测到的凭证环境(见 §6)。
- Node ≥ 22(对齐 SFL 先例)。

### 3.4 内容包(bundle):数据同源机制

**目标**:MCP、SKILL.md、官网三方永不漂移;npm 包内置快照保证离线可用。

**产物**(由新增 CI 脚本 `build-bundle.py` 从现有产物生成):

```
mcp-bundle-v{N}.json          # 单文件 bundle,~300 KB 量级
├── meta: { siteVersion, bundleVersion, generatedAt, license }
├── categories: [12 分类 + 中英文别名]
├── charts: [{ id, name, nameZh, type, level1, level2,
│              tutorialUrl, tutorialUrlZh, imageUrl, description,
│              codeRefs: { r?, python?, julia? } }]
├── codeSnippets: [{ chartId, lang, code, packages[], dataUrls[], sourceAnchor }]
├── keywords: (recommender_keywords*.json 直通,供检索加权)
└── citation: { paperDoi: "10.1002/imm3.70038", paperUrl, bibtex, license: "CC-BY-NC-4.0" }
```

**数据流**:

1. `build-bundle.py` 读取 `files/gallery_data*.csv`、`files/recommender_keywords*.json`,解析每篇 QMD 的代码块(复用 `generate_skills.py` 的既有正则提取器,DRY)。
2. 生成 bundle 后计算内容哈希;与 SKILL.md 再生成同一触发(`**.qmd` push),同一次 CI 运行产出,保证原子一致。
3. bundle 发布到 `_site/files/mcp/bundle.json`(随站点静态托管),同时快照进 npm 包 `src/snapshot/`。
4. `sync-gallery.py` 与 `build-bundle.py` 在 CI 中先后运行,后者消费前者产物,顺序有 CI 保证。

**版本与更新**:

- bundle 版本号 = 站点部署序号;服务器启动时记录版本。
- 本地服务器:启动时后台 HEAD 请求检查远端 bundle 的 `ETag`;有更新则热替换内存数据,并将**一次性更新通知**(SFL 模式)追加到该会话的下一个工具结果:`[Bizard 内容包已更新至 v{N}:新增 X 图、更新 Y 图]`。
- 离线/网络失败:静默使用内置快照(不报错、不阻塞),日志写入 stderr。

### 3.5 llms.txt(附加项,吸收自方案 B)

站点根路径额外发布 `llms.txt`(Jeremy Howard 提案):列出 12 分类、798 图表名及规范 URL,供**任何** AI(含不支持 MCP 的)一句话理解站点结构。与 MCP 服务器共用 `build-bundle.py` 的输出,边际成本近零。

---

## 4. 工具集设计(全生命周期)

### 4.1 查询阶段

**`bizard_search`** — 核心检索工具

- 输入:`query`(string,必填)、`language`(en/zh,默认 en)、`category`(可选过滤)、`limit`(默认 5,上限 20)
- 行为:对 `name/nameZh/description/type/level1/level2` 做加权词法匹配(复用 recommender_keywords 的同义词扩展;无 LLM,确定性结果);返回图表卡片数组。
- 每条结果:`{ id, name, nameZh, description, type, imageUrl, tutorialUrl, selector? }`(selector 为教程页内具体图表锚点,如 `#fig-1.1BasicViolin`)。
- 空结果时返回**相邻建议**(同 Level1 分类 Top3),不返回裸空数组——MCP-Universe 结论:引导性空结果显著提升下游任务成功率。

**`bizard_list_categories`** — 浏览入口

- 输入:无必填;返回 12 分类树(含中英别名、每类图表数)。供宿主 AI 在用户需求模糊时先分类再检索。

### 4.2 使用阶段(弥合图文差距的核心)

**`bizard_get_tutorial`** — 教程结构化内容

- 输入:`chartId`;返回:教程章节结构、Setup 依赖清单(包+版本)、示例数据说明、关键参数表、图文 URL。
- 内容从 bundle 的 codeSnippets + QMD 元数据装配;不含全文(全文由宿主按 URL 自取,控制 token)。

**`bizard_get_code`** — 可运行代码获取

- 输入:`chartId`、`language`(r/python/julia,必填);返回:`{ code, packages[], dataUrls[], installGuard, notes }`。
- code 保持教程原码语义(含 install-guard 模式);dataUrls 为 COS 等示例数据直链,宿主 AI 可直接下载替接管用户数据。

**`bizard_adapt_guidance`** — 适配指引(提示词工程落点,弥合差距的关键工具)

- 输入:`chartId`、`language`、`userDataContext`(string,用户对自己数据的描述,可选)
- 返回结构化适配检查单(提示词工程,由 SKILL.md 现有规范升级):
  1. **数据形状检查**:该图要求的最小数据结构(列数、类型、长/宽格式)与自检代码片段
  2. **依赖准备**:精确安装命令(install-guard)、常见版本坑
  3. **参数映射**:用户场景 → 教程代码中需修改的参数位(逐条"位置+改法")
  4. **常见失败模式**:该图类型的 Top 失败案例及排查顺序(如分布图:数据过少/因子水平缺失)
  5. **替代方案**:同 `Level1` 分类下 2-3 个备选图 + 一句话适用差异
- 返回为 JSON 结构,宿主 AI 据此驱动自身代码执行循环。**流程化**:把"专家改图时脑中的检查序列"显式编码,而非依赖宿主 AI 自由发挥。

### 4.3 参考阶段

**`bizard_cite`** — 引用生成

- 输入:`chartId`(或 `paperOnly: true`)、`format`(bibtex/text/json,默认 bibtex)
- 返回:Bizard 论文引用(DOI `10.1002/imm3.70038`)+ 具体教程 URL + 许可声明(CC-BY-NC 4.0,含商用限制提示)+ 图表作者字段(若 QMD YAML 有 author 元数据)。
- 文本格式含一段可直接粘贴的"数据与方法"模板句(双语)。

### 4.4 贡献阶段

**`bizard_contribute_check`** — 提交前校验(纯函数,无副作用)

- 输入:`contributionType`(new-tutorial/fix/translation/gallery-image)、`content`(草稿或 QMD 路径)、`language`
- 校验:Template 结构完整性(Setup 章节固定模板,对齐 issue #181 PR #182)、代码块语言守卫、图片引用规范、双语字段齐全性;返回 `{ pass, issues[], checklist[], pointsPreview }`(pointsPreview 按 issue #4 积分表预估,如 new-tutorial → +10)。

**`bizard_contribute_submit`** — 认证按环境降级提交

- 输入:`contributionType`、`title`、`body`(已过 check)、`asPR`(boolean,仅当上传完整 QMD 文件内容时可为 true)
- **三级降级链**(每次调用时探测,透明声明):

| 级别 | 条件 | 行为 |
|------|-----|------|
| 1 | 本地,`gh` CLI 已登录 | 以用户身份创建 issue(或草稿分支+PR,若 `asPR` 且内容完整) |
| 2 | 无 gh 但有 `GITHUB_TOKEN` | GitHub API 提交(能力随 token 权限) |
| 3 | 均无(典型:托管实例) | 返回格式化草稿 + 预填 deep-link(`…/issues/new?title=&body=`),用户点击完成最后一步 |

- **透明性**:结果首行明确"已提交为 issue #N"或"当前环境无凭证,已生成草稿链接";绝不静默失败。
- **安全**:服务器自身零凭证;用户自由文本在拼入 issue body 前截断(单字段 ≤ 4000 字符)与转义;托管实例级别 3 无代发能力,无滥用面。
- 积分审核仍走 issue/PR 现有人工流程,零改动。

### 4.5 资源(Resources)与提示词(Prompts)

**Resources**(客户端可拉取):

- `bizard://catalog`(全目录 JSON,等同 bundle.charts 精简版)
- `bizard://tutorial/{chartId}`(单教程结构化内容)
- `bizard://llms-txt`(站点 llms.txt 直通)

**Prompts**(用户可显式调用,流程化工作流):

| 名称 | 场景 | 内容骨架 |
|------|------|---------|
| `recommend` | 需求模糊 → 选图 | 需求澄清问题树 → 检索 → 多维对比(数据类型/展示重点/审美) → 推荐+备选 |
| `adapt` | 已选图 → 适配用户数据 | `bizard_adapt_guidance` 检查单驱动 → 让宿主先读用户数据形状 → 逐步改代码 → 自查出图 |
| `troubleshoot` | 复现失败 | 失败信息分类(数据/依赖/语法) → 对应排查树 → 常见失败模式库匹配 |
| `contribute` | 用户想回馈 | 引导选择贡献类型 → 生成模板草稿 → `contribute_check` → 提交/降级 |

四个 prompt 的文本由 CI 从 `skill-spec.md` 与 Template 同源生成(单一事实源)。

---

## 5. 反馈记录(仅本地)

- 位置:`~/.cache/bizard-mcp/usage-log.jsonl`(跟随缓存目录,可 `BIZARD_USAGE_LOG` 覆盖)。
- 事件:`{ ts, event: search|view|adapt|cite, chartId, language, queryHash }`——`queryHash` 为查询文本的 SHA-256 前 8 位(不可逆,防原文泄露)。
- 消费:`bizard_contribute_check` 返回的 checklist 中可选附带"最近使用信号摘要"(用户确认后才进入贡献内容);**无任何网络上报**。
- 关闭方式:`BIZARD_USAGE_LOG=off` 完全禁用。

---

## 6. 部署形态

### 6.1 本地部署(用户自助)

```bash
npx @openbiox/bizard-mcp            # stdio,一条命令
```

- 客户端配置(Claude Code 示例):`claude mcp add bizard -- npx @openbiox/bizard-mcp`
- 完整能力:四阶段工具全开,贡献为级别 1/2。
- 离线:内置快照兜底,检索/代码/引用可用,仅内容更新检查失效。

### 6.2 托管部署(Bizard 团队运行)

- 同一包,`--http` 启动 Streamable HTTP;前置反代 + 静态 OAuth 资源服务器元数据(一期仅支持公开匿名访问,无鉴权——数据全公开,仅做限流)。
- 限流:简单令牌桶(IP 维度,如 60 req/min),防爬取滥用;429 附 Retry-After。
- 贡献自动为级别 3(草稿+deep-link),无需任何账号体系。
- 部署载体一期建议:GitHub Actions 定时任务过重,推荐单容器(Fly.io/Cloud Run 免费/低配档)或先以 `huggingface.co/spaces` 零成本托管试点,二期再评估。

---

## 7. 错误处理与边界

| 场景 | 行为 |
|------|------|
| chartId 不存在 | 返回最近似(名称模糊匹配 Top3)+ 有效 id 列表节选,HTTP 语义不适用(工具层永远返回结构化结果,错误进 `isError` 字段) |
| bundle 损坏/缺字段 | 启动时 schema 校验失败 → 回退内置快照并告警 |
| gh 探测异常(超时/未登录) | 视为级别 3,结果中说明原因 |
| QMD 元数据缺失(老教程) | codeRefs 为空数组,`get_code` 返回"该教程暂无结构化代码,附教程 URL"降级路径 |
| 检索零命中 | 相邻分类建议(见 §4.1) |
| 用户文本超长 | 截断 + 明确提示已截断 |

---

## 8. 测试策略

- **单元**(Vitest):检索评分器(同义词扩展、双语、category 过滤)、降级链探测逻辑(注入 mock gh/token 环境)、bundle schema 校验、适配检查单生成器、usage-log 写入(含 `=off` 分支)。
- **集成**:stdio 端到端(官方 `@modelcontextprotocol/inspector` 或 SDK client):启动 → initialize → 逐工具调用 → 断言返回 schema;HTTP 模式同套 client 换传输复用。
- **契约快照**:bundle 内容 hash 变更检测,CI 中对比 SKILL.md/gallery CSV 与 bundle 的一致性(防漂移的机器门禁)。
- **覆盖率目标**:80%(用户全局规约)。

---

## 9. 里程碑规划(供 writing-plans 阶段细化)

| 阶段 | 内容 | 交付 |
|------|------|------|
| M1 | bundle 生成脚本 + CI 集成 + llms.txt | 内容管线先跑通(不动 MCP) |
| M2 | 核心层:检索/教程/代码/引用四工具 + stdio | 本地可用,发布 npm |
| M3 | 适配指引 + 4 prompts + 本地 usage log | 「使用」阶段完整 |
| M4 | 贡献工具(校验+降级链)+ Streamable HTTP + 托管部署 | 全生命周期 + 双部署 |
| 二期候选 | MCP Apps UI、服务器代跑预览、匿名遥测、SFL provider 互连(issue #111 提及的协同) | — |

各里程碑均为独立可交付,M1 不依赖 MCP 知识,M2 起可由熟悉 TS 的贡献者接手。

---

## 10. 风险与对策

| 风险 | 对策 |
|------|------|
| bundle 体积随教程增长膨胀 | codeSnippets 仅存代码+元数据,不含正文;超限时按分类分片 |
| generate_skills.py 正则演进导致双脚本漂移 | M1 时把代码块提取函数抽为共享模块,两脚本同 import(而非复制) |
| 托管实例被滥用(高频抓取) | 限流 + bundle 公开可下载(诚实客户端直接下快照),工具调用非唯一渠道 |
| CC-BY-NC 许可被 AI 误用声明 | `bizard_cite` 固化许可声明文本,`get_code` 返回值附带许可行 |
| zh 内容质量不一致(部分教程无 zh 版) | bundle 中 nameZh/tutorialUrlZh 可空,检索按 language 权重回退英文 |
| SFL 风格的更新通知打扰 | 一次性追加,版本会话内只提示一次 |

---

## 11. 与用户确认记录

1. 部署分工:同一份实现,开发者托管 + 用户本地自部署,任选接入 → 单包双传输。
2. 代码执行:不执行,宿主 AI 执行。
3. 与 SKILL.md 关系:同源生成,skill 兼容共存。
4. 实现语言:TypeScript SDK。
5. 贡献认证:代理发 issue/PR,按环境降级(gh → token → 草稿)。
6. 使用反馈:仅本地匿名 JSONL 记录,无服务器端收集。

---

*本文档为 brainstorming 产出,后续将按 superpowers:writing-plans 流程产出实现计划(仍不实施)。*
