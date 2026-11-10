## awesome genome visualizations 整合待办清单（逐项标记整合方式）

对 [awesome-genome-visualization](https://github.com/cmdcolin/awesome-genome-visualization) 条目库（数据源 `TOOLS.json`，快照 **574 条**，无重名）逐条梳理，标记后续整合进 Bizard 的方式。清单按整合方式分四节：

- 🔁 **补充进现有 qmd**（186 条 → 收敛到 12 篇现有教程）：作为对应教程的图型/版式补充参考，择优吸收，一般不单独成文
- ✨ **新增独立 qmd**（172 条 → 收敛为 20 篇新教程）：Bizard 目前没有对应图型，需新增文档（括号内为建议目录/文件名，可调整）
- ⏸️ **暂缓**（206 条）：交互式浏览器、桌面 GUI、终端工具、商业软件与失效项目，不满足"静态可复现出图"，待后续专题再议
- ❓ **需调研**（10 条）：信息不足以判定图型/可复现性，待确认后并入上述三节

每条目下方统一标注两种实现状态（定义与核验方法见第六节）：

- `[包]` = 2026-11-07 已核验可从 CRAN / Bioconductor / PyPI 安装（含已锁进 `renv.lock` 者），实现时可直接调用
- `[重实现]` = 本次核验未确认可直接安装的包（多为 Web 服务 / GUI / CLI / 商业软件 / 失效项目，或仅 GitHub 源码），**须按本条的图型描述用 R/Python 从头实现**。这是"实现路径"标记，不是"社区没有包"的断言
- `[x]` = 已处理：已并入目标页，或按"超过 5 年无运维更新 / 依赖过重 / 无法静态复现"标准明确跳过并记录理由；`[ ]` = 暂不纳入实施面（交互式 / 商业 / 失效 / 纯分析 / 信息不足）

### 0. 与 #436（FigureYa）的关键差异

| 维度 | #436 FigureYa | 本清单 awesome-genome-visualization |
|---|---|---|
| 条目性质 | 318 个图文档，本体就是 R 绘图脚本 | **混合体**：仅 220/574 条标注了 R/Python/Julia 技术栈，其余是 Web 服务、桌面 GUI、CLI 工具、商业软件或已失效项目 |
| 代码复用 | 参数与案例可直接迁移 | **多数无代码可复用**，须先做"图型解剖"再重新实现 |
| 筛选标准 | 图是否与 Bizard 现有 qmd 对应 | **该图型能否被 Bizard 用 R/Python 静态、可复现地重画** |
| 落地动作 | 补充 / 新增 qmd | 同上，但每条要先记录"图的构成要素 + 科学含义"，再决定实现路径 |

因此本清单刻意不按"有没有源码"排序，而按**图型 → 目标文档 → 实现状态**三层记录，避免把无法静态复现的浏览器界面误当成教程选题。

### 一、总览

| 归属 | 条目数 | 其中标注 R/Python/Julia | 落地形式 |
|---|---:|---:|---|
| 🔁 补充进现有 qmd | 186 | 123 | 12 篇现有教程 |
| ✨ 新增独立 qmd | 172 | 81 | 20 篇新教程 |
| ⏸️ 暂缓 | 206 | 13 | — |
| ❓ 需调研 | 10 | 3 | 待定 |
| **合计** | **574** | **220** | **约 32 篇文档的改动面** |

关键结论：

1. 574 条里，真正"有 R/Python/Julia 实现"的只有 220 条；本清单把其中 **204 条**纳入实施面（第二节 123 + 第三节 81），其余 154 条标注 `[重实现]`。
2. 最大的空白是 **基因组图（GFA/组装图 25 条）与结构变异（27 条）**，这两块在 Bizard 现有 31 篇 Omics 文档里是 0 覆盖。
3. 更新最省力的做法是先补第二节的 12 篇（多为"同图型换实现/加版式"），再按第三节逐篇立项。

### 二、🔁 补充进现有 qmd（186 条 / 12 篇）

以下条目不单独成文，而是并入现有教程作为"更多实现与版式参考"。每篇给出当前实现（`现状`）与建议吸收点。

#### 1) `Omics/ChromosomePlot.qmd` —— 14 条

现状：`RIdeogram` 单一静态核型（`rtracklayer` 读入注释）。建议补充交互/多轨道核型与"信号锚定核型"的可迁移版式。

- [x] [chromoMap](https://lakshay-anand.github.io/chromoMap/index.html)（2022；[文献](https://doi.org/10.1186/s12859-021-04556-z)） `[包]`：CRAN；交互式（htmlwidget）染色体元素映射与"染色体热图"轨道
- [x] [eweitz Ideogram](https://github.com/eweitz/ideogram) `[重实现]`：JS 核型组件，其"带 + 标注 + 富集环"版式可作静态重画参考
- [x] [ggbio](http://www.bioconductor.org/packages/2.11/bioc/vignettes/ggbio/inst/doc/ggbio.pdf)（2012；[文献](https://doi.org/10.1186/gb-2012-13-8-r77)） `[包]`：Bioconductor；`autoplot(GRanges)` 的 Ideogram / GrandLinear 基因组概览视图
- [x] [Ideogram](https://github.com/RCollins13/HumanIdiogramLibrary)（2018；[文献](https://doi.org/10.5281/zenodo.1210135)） `[重实现]`：SVG 输出的纯核型图，适合作为最简基线
- [x] [Ideogram viewer](https://bioinformatics.mdanderson.org/public-software/ideogramviewer/) `[重实现]`：交互式核型浏览，取其染色体分带配色规范
- [x] [Ideoplot](https://github.com/mchaisso/Ideoplot) `[重实现]`：纯 base-graphics 的轻依赖核型绘制
- [x] [IdeoViz](https://www.bioconductor.org/packages/release/bioc/vignettes/IdeoViz/inst/doc/Vignette.pdf) `[包]`：把信号轨道直接锚定到核型条带
- [x] [karyoploteR](https://github.com/bernatgel/karyoploteR)（2017；[文献](https://doi.org/10.1093/bioinformatics/btx346)） `[包]`：Bioconductor；可在核型上叠加任意自定义 data panel（30+ 高层绘图函数），补足 RIdeogram 轨道固定的短板
- [x] [KaryotypeSVG](https://github.com/andreasprlic/karyotypeSVG) `[重实现]`：SVG 核型生成，可直接嵌入矢量排版
- [x] [NCBI Genome Decoration](https://www.ncbi.nlm.nih.gov/genome/tools/gdp/) `[重实现]`：NCBI 官方染色体重排装饰图，用于重排/易位的染色体级标注
- [x] [pyideogram](https://github.com/Balthasar-eu/pyideogram) `[包]`：Python 侧核型绘制，便于和 pyCirclize 共用数据层
- [x] [RIdeogram](https://github.com/TickingClock1992/RIdeogram)（2020；[文献](https://doi.org/10.7717/peerj-cs.251)） `[包]`：已用于该教程；补充说明其输出为 SVG 矢量（非栅格），且轨道位置固定
- [x] [shinyChromosome](http://150.109.59.144:3838/shinyChromosome/)（2019；[文献](https://doi.org/10.1016/j.gpb.2019.07.003)） `[重实现]`：CRAN/Shiny；交互式染色体可视化，可导出静态图
- [x] [UCSC Genome Graphs](http://genome.ucsc.edu/cgi-bin/hgGenome)（2002；[文献](https://doi.org/10.1101/gr.229102)） `[重实现]`：以基因组坐标为单位展示任意数值（GWAS/保守性/覆盖度）的全基因组折线

#### 2) `Omics/GeneStructurePlot.qmd` —— 25 条

现状：`gggenes` 基因箭头 + `ggtree` 树。建议补充多轨道组合、比对特征排版与覆盖度叠轨。

- [x] [aplot](https://yulab-smu.top/aplot/cases.html#genomics-track-example)（2025；[文献](https://doi.org/10.1016/j.xinn.2025.100958)） `[包]`：CRAN；把不同 y 轴的多面板按行/列对齐拼接（该教程已有使用）
- [x] [ArchR](https://www.archrproject.com/articles/Articles/tutorial.html)（2021；[文献](https://doi.org/10.1038/s41588-021-00790-6)） `[重实现]`：R；scATAC 的 genome browser track 风格输出
- [x] [BAMdash](https://github.com/jonas-fuchs/BAMdash/) `[包]`：Python；BAM 覆盖度的快速缩略图
- [x] [ExonIntron](http://wormweb.org/exonintron) `[重实现]`：静态外显子/内含子示意图，作最简基因模型基线
- [x] [FeatureViewer](https://github.com/calipho-sib/feature-viewer)（2017；[文献](https://doi.org/10.5281/zenodo.596941)） `[重实现]`：Web；蛋白/核酸特征的轨道化标注
- [x] [GCSnap](https://github.com/JoanaMPereira/GCsnap)（2021；[文献](https://doi.org/10.1016/j.jmb.2021.166943)） `[重实现]`：Python；邻居基因排序（gene order）的上下文矩阵
- [x] [geneviewer](https://nvelden.github.io/geneviewer/) `[包]`：R；箭头 + 外显子 + 缩放刷选的交互式基因上下文
- [x] [Geneviz](https://jrderuiter.github.io/geneviz/usage.html) `[重实现]`：Python；基因结构示意图
- [x] [genomegraphs](http://bioconductor.org/packages/2.5/bioc/vignettes/GenomeGraphs/inst/doc/GenomeGraphs.pdf)（2009；[文献](https://doi.org/10.1186/1471-2105-10-2)） `[重实现]`：Bioconductor 历年归档；早期 GenomeGraphs 的轨道对象模型，作历史方案说明
- [x] [ggcoverage](https://cran.r-project.org/web/packages/ggcoverage/vignettes/ggcoverage.html)（2022；[文献](https://doi.org/10.1101/2022.09.01.503744)） `[重实现]`：R（CRAN 2023-11-16 归档，需 install_github）；ggplot2 语法糖绘制覆盖度轨道
- [x] [GGgenes](https://github.com/wilkox/gggenes) `[包]`：已用于该教程；补充多物种 facet 的 `forward` 方向翻转说明
- [x] [ggtranscript](https://github.com/dzhang32/ggtranscript)（2022；[文献](https://doi.org/10.1101/2022.03.28.486050)） `[重实现]`：R（GitHub，非 CRAN/Bioconductor）；以转录本为对象的 ggplot2 基因模型语法
- [x] [gtrellis](http://bioconductor.org/packages/devel/bioc/vignettes/gtrellis/inst/doc/gtrellis.html)（2016；[文献](https://doi.org/10.1186/s12859-016-1051-4)） `[包]`：Bioconductor；多物种/多类别分面平铺的基因组概览网格
- [x] [Gviz](https://bioconductor.org/packages/release/bioc/html/Gviz.html)（2016；[文献](https://doi.org/10.1007/978-1-4939-3578-9_16)） `[包]`：Bioconductor；DataTrack / GeneRegionTrack / AnnotationTrack / IdeogramTrack 的分轨叠加模型，是该图型的规范形态
- [x] [Millefy](https://github.com/yuifu/millefy)（2020；[文献](https://doi.org/10.1186/s12864-020-6542-z)） `[重实现]`：R（GitHub，非 CRAN/Bioconductor）；单细胞每细胞一行覆盖度的热图轨道
- [x] [multiGenomicContext](https://github.com/Sanrrone/multiGenomicContext) `[重实现]`：Python+R；多基因组上下文的对齐排列
- [x] [Plotgardener](https://github.com/PhanstielLab/plotgardener/)（2022；[文献](https://doi.org/10.1093/bioinformatics/btac057)） `[包]`：Bioconductor；以"画布 + 英寸坐标"做绝对布局，最贴近期刊多面板排版需求
- [x] [RepViz](https://bmcresnotes.biomedcentral.com/articles/10.1186/s13104-019-4473-z)（2019；[文献](https://doi.org/10.1186/s13104-019-4473-z)） `[重实现]`：R；重复样本间的一致性信号轨道
- [x] [signac](https://github.com/timoast/signac/)（2021；[文献](https://doi.org/10.1038/s41592-021-01282-5)） `[包]`：R；scATAC 覆盖度轨道与 `CoveragePlot`
- [x] [Sushi](https://www.bioconductor.org/packages/release/bioc/vignettes/Sushi/inst/doc/Sushi.pdf)（2014；[文献](https://doi.org/10.1093/bioinformatics/btu379)） `[重实现]`：已于 Bioconductor 3.16 移除（GitHub 最后提交 2021-10-26）；仅作函数语义参考，不列为依赖
- [x] [svist4get](https://link.springer.com/article/10.1186/s12859-019-2706-8)（2019；[文献](https://doi.org/10.1186/s12859-019-2706-8)） `[包]`：Python；信号轨道 + 基因结构的一体化出图（SVG）
- [x] [TnT](https://tnt.marlin.pub/articles/introduction)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw210)） `[包]`：Web/R；`TNTboard` 的基因结构轨道语法（ggtree 前身之一）
- [x] [Trackplot (R)](https://github.com/PoisonAlien/trackplot)（2024；[文献](https://doi.org/10.1093/bioadv/vbae031)） `[重实现]`：R；覆盖度 + 基因结构的轻量方案
- [x] [trackViewer](https://bioconductor.org/packages/release/bioc/vignettes/trackViewer/inst/doc/trackViewer.html)（2019；[文献](https://doi.org/10.1038/s41592-019-0430-y)） `[包]`：Bioconductor；多轨道 + 交互浏览；其 `lolliplot()` 已在 Ranking/Lollipop 使用
- [x] [wiggleplotr](http://bioconductor.org/packages/devel/bioc/vignettes/wiggleplotr/inst/doc/wiggleplotr.html) `[包]`：Bioconductor；按转录本（而非基因组）坐标归一化覆盖度，用于可变剪接比较

#### 3) `Omics/MultiSeqsAlignment.qmd` —— 16 条

现状：`ggmsa` 比对矩阵 / 序列 logo。建议补充二级结构条、守恒分箱着色与"比对 + 树"对齐。

- [x] [alignfigR](https://cran.r-project.org/web/packages/alignfigR/vignettes/my-vignette.html) `[重实现]`：CRAN；把比对结果转成可拼接的 ggplot2 片段
- [x] [AliTreeViz](https://www.npmjs.com/package/alitreeviz) `[重实现]`：JS；把比对列与树上叶序联动，用于"比对 + 树"对齐
- [x] [Boxshade](https://embnet.vital-it.ch/software/BOX_form.html)（2000；[文献](https://doi.org/10.1186/gb-2000-1-2-reports2047)） `[重实现]`：经典 `shade` 守恒着色（黑=完全保守、灰=相似），按列频率分箱即可重实现
- [x] [CIAlign](https://github.com/KatyBrown/CIAlign)（2022；[文献](https://doi.org/10.7717/peerj.12983)） `[包]`：Python；比对清理（去 gap/修剪）后再绘图的实用前置步骤
- [x] [ESPript](http://espript.ibcp.fr/ESPript/ESPript/)（1999；[文献](https://doi.org/10.1093/bioinformatics/15.4.305)） `[重实现]`：Web/CLI；二级结构条 + 一致性着色 + 疏水簇柱图三段式版式，ggmsa 无原生对应
- [x] [ete](http://etetoolkit.org/gallery/)（2016；[文献](https://doi.org/10.1093/molbev/msw046)） `[包]`：Python；树 + 比对 + 结构域的三联面板
- [x] [ggmsa](http://yulab-smu.top/ggmsa/)（2022；[文献](https://doi.org/10.1093/bib/bbac222)） `[包]`：已用于该教程
- [x] [ggtree](https://github.com/YuLab-SMU/ggtree)（2016；[文献](https://doi.org/10.1111/2041-210X.12628)） `[包]`：`msaplot()` 把比对矩阵挂到系统发生树上（该教程 GeneStructurePlot 亦已使用）
- [x] [msaR](https://github.com/zachcp/msaR) `[包]`：CRAN；BioJS/MSA 的 R 封装，可取其后端渲染思路
- [x] [pyBoxshade](https://github.com/mdbaron42/pyBoxshade) `[重实现]`：Python；boxshade 的脚本化实现
- [x] [pymsaplotter](https://github.com/orangeSi/pymsaploter) `[重实现]`：Python；比对矩阵的紧凑排布
- [x] [react-msaview](https://github.com/GMOD/react-msaview) `[重实现]`：JS/React 比对查看器（JBrowse 生态），取其列标尺与保守性轨道设计
- [x] [rMSA](https://github.com/mhahsler/rMSA/blob/master/vignettes/rMSA_vignette.pdf) `[重实现]`：R（GitHub，非 CRAN/Bioconductor）；可自动生成 boxshade 风格着色
- [x] [seqvisr](https://github.com/vragh/seqvisr/) `[重实现]`：R（GitHub，非 CRAN/Bioconductor）；ggplot2 原生比对可视化，可作 ggmsa 的轻依赖替代
- [x] [TeXShade](http://mirrors.ibiblio.org/CTAN/macros/latex/contrib/texshade/texshade.pdf)（2000；[文献](https://doi.org/10.1093/bioinformatics/16.2.135)） `[重实现]`：桌面；LaTeX 排版的比对图，其色标定义被广泛沿用
- [x] [Wasabi](http://wasabiapp.org/)（2015；[文献](https://doi.org/10.1093/molbev/msv333)） `[重实现]`：Web；比对编辑器，其着色与列标尺版式可直接借鉴

#### 4) `Omics/MotifPlot.qmd` —— 1 条

现状：`ggseqlogo` 序列 logo（DNA/蛋白）。建议补充 RNA 结构 logo（含碱基配对弧）。

- [x] [LogoJS](https://logojs.wenglab.org/app/gallery/)（2020；[文献](https://doi.org/10.1093/bioinformatics/btaa192)） `[重实现]`：Web/JS；支持 RNA 结构 logo（弧线/碱基配对标记），ggseqlogo 仅支持序列 logo

#### 5) `Omics/ManhattanPlot.qmd` —— 12 条

现状：`qqman` 曼哈顿图 + `aplot` 拼接。建议补充区域关联图（LocusZoom 风格：LD 着色 + 基因轨 + 重组率）。

- [x] [BigTop](https://blog.dnanexus.com/2019-05-21-bigtop-data-visualization/) `[重实现]`：Web；以 P 值高度排序的"BigTop"图，与曼哈顿图互补
- [x] [CMPlot](https://github.com/YinLiLin/CMplot)（2021；[文献](https://doi.org/10.1016/j.gpb.2020.10.007)） `[包]`：已用于 GwasSnpPlot；其 SNP density / LD heatmap 四联图版式可回流该教程
- [x] [GeneticsMakie.jl](https://github.com/mmkim1210/GeneticsMakie.jl)（2022；[文献](https://doi.org/10.1101/2022.04.18.488573)） `[重实现]`：Julia；GWAS + 基因结构 + LD 的 Makie 实现，对应 Bizard Julia 侧缺口
- [x] [ggplot2 manhattan plot](https://danielroelfs.com/blog/how-i-create-manhattan-plots-using-ggplot/) `[重实现]`：以累积 bp（cumulative position）替代分面的做法，与 facet 方案二选一
- [x] [ggwas](https://bczech.github.io/ggwas/) `[重实现]`：ggplot2 的 GWAS 图语法糖
- [x] [LocusBlend](https://github.com/Belloy-Lab/LocusBlend)（2026；[文献](https://doi.org/10.64898/2026.07.15.26358129)） `[重实现]`：2026；同时按多个 index variant 着色的区域图，可作多信号位点小节
- [x] [Locuszoom](http://locuszoom.org/)（2010；[文献](https://doi.org/10.1093/bioinformatics/btq419)） `[重实现]`：区域关联图的原始定义（LD 色标 + 基因轨 + 重组率）
- [x] [Locuszoom.js](http://locuszoom.org/locuszoomjs.php)（2021；[文献](https://doi.org/10.1093/bioinformatics/btab186)） `[重实现]`：同上，浏览器版；取其中 LD 分箱配色与基因轨对齐规则
- [x] [locuszoomr](https://cran.r-project.org/web/packages/locuszoomr/vignettes/locuszoomr.html)（2024；[文献](https://doi.org/10.1093/bioadv/vbaf006)） `[包]`：CRAN；区域关联图（LocusZoom 风格），按 LD 与 index variant 着色，Bizard 现为 0 覆盖
- [x] [pixy](https://pixy.readthedocs.io/en/latest/plotting.html#a-genome-wide-plot-of-summary-statistics)（2021；[文献](https://doi.org/https://doi.org/10.1111/1755-0998.13326)） `[包]`：Python；多染色体汇总统计量的 `facet_grid` 通用版式（可移植到 ggplot2）
- [x] [RACER](https://oliviasabik.github.io/RACERweb/articles/IntroToRACER.html)（2018；[文献](https://doi.org/10.1101/495366)） `[重实现]`：区域关联图 + 基因注释，R 实现，可作 LocusZoom 的 R 侧对照
- [x] [topr](https://github.com/totajuliusd/topr/)（2023；[文献](https://doi.org/10.1186/s12859-023-05301-4)） `[包]`：CRAN；底层 ggplot2，出版级曼哈顿图并可并排放柱状/箱线富集

#### 6) `Omics/GwasSnpPlot.qmd` —— 3 条

现状：`CMplot` 的 GWAS 汇总视图。建议补充 VCF 前置质控图与纯合区（ROH）沿染色体带状图。

- [x] [bcftools roh](https://samtools.github.io/bcftools/howtos/roh-calling.html)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw044)） `[重实现]`：Python+C；纯合区（ROH）沿染色体的带状图
- [x] [Human genome dating](https://human.genome.dating/region/chr1_13250000)（2020；[文献](https://doi.org/10.1371/journal.pbio.3000586)） `[重实现]`：Web；以染色体为横轴的等位基因频率/年代梯度，作多轨道版式参考
- [x] [vcfR](https://knausb.github.io/vcfR_documentation/)（2016；[文献](https://doi.org/10.1111/1755-0998.12549)） `[包]`：CRAN；VCF 读入与 `chromoqc` 式的染色体质量控制图，是 GWAS 图的数据前置

#### 7) `Omics/ScCnvPlot.qmd` —— 24 条

现状：`ComplexHeatmap` + `circlize` 的单细胞 CNV 排序热图。建议补充"散点 + 分段"叠加、B 等位频率二维图与覆盖度三联缩略图。

- [x] [aCNVViewer](https://github.com/FJD-CEPH/aCNViewer)（2017；[文献](https://doi.org/10.1371/journal.pone.0189334)） `[重实现]`：Python；为 CNV 区间直接绘制基因注释条带
- [x] [BAMScale](https://github.com/ncbi/BAMscale)（2020；[文献](https://doi.org/10.1186/s13072-020-00343-x)） `[重实现]`：C+R；BAM 直出全基因组覆盖/CNV 折线
- [x] [bcftools cnv](https://samtools.github.io/bcftools/howtos/cnv-calling.html) `[重实现]`：Python+C；`cnv` 子命令的 CNV 折线（轻量基线）
- [x] [casper](https://github.com/akdess/CaSpER)（2020；[文献](https://doi.org/10.1038/s41467-019-13779-x)） `[包]`：R；单细胞/ bulk 的 CNV 谱与贝叶斯分段
- [x] [chrov](https://github.com/rraadd88/chrov) `[包]`：Python；环形 CNV 概览
- [x] [CNANorm](https://www.bioconductor.org/packages/release/bioc/html/CNAnorm.html)（2011；[文献](https://doi.org/10.1093/bioinformatics/btr593)） `[重实现]`：Bioconductor；配对肿瘤-正常样本的 CN 比值与分段
- [x] [CNSpector](https://github.com/PapenfussLab/CNspector)（2019；[文献](https://doi.org/10.1038/s41598-019-42858-8)） `[重实现]`：R/Shiny；CNV 分段与交互查看
- [x] [CNView](https://github.com/RCollins13/CNView)（2016；[文献](https://doi.org/10.1101/049536)） `[重实现]`：R；环形或线性 CNV 分段
- [x] [CNVkit](https://cnvkit.readthedocs.io/en/stable/plots.html)（2016；[文献](https://doi.org/10.1371/journal.pcbi.1004873)） `[包]`：Python；散点 + 分段（segmented）叠加与权重轴，Bizard 现仅单细胞基因排序热图
- [x] [CNVPlot](https://github.com/dantaki/CNVplot)（2015；[文献](https://doi.org/10.1101/030270)） `[重实现]`：R；数组 CGH 的 CNV 分段与基因标注
- [x] [CNVpytor](https://github.com/abyzovlab/CNVpytor)（2021；[文献](https://doi.org/10.1093/gigascience/giab074)） `[包]`：Python；读深度 + BAF 双面板与交互缩放
- [x] [copykat](https://github.com/navinlabcode/copykat/blob/master/vignettes/copycat-vignettes.pdf)（2021；[文献](https://doi.org/10.1038/s41587-020-00795-2)） `[重实现]`：R；单细胞 CNV 预测的染色体热图（与 ScCnvPlot 同型，可对照）
- [x] [copynumber](https://www.bioconductor.org/packages/release/bioc/vignettes/copynumber/inst/doc/copynumber.pdf)（2012；[文献](https://doi.org/10.1186/1471-2164-13-591)） `[重实现]`：Bioconductor；多片段多色染色体分段图
- [x] [CoveragePlotter](http://lindenb.github.io/jvarkit/CoveragePlotter.html) `[重实现]`：Java；覆盖度与 CNV 的堆叠图，作版式参考
- [x] [covviz](https://brwnj.github.io/covviz/) `[重实现]`：JS；覆盖度 + 群体频率的叠加图
- [x] [GCI](https://github.com/yeeus/GCI)（2024；[文献](https://doi.org/10.1093/bioinformatics/btae633)） `[重实现]`：Python；基因组覆盖度概览
- [x] [genevisR](https://bioconductor.org/packages/release/bioc/vignettes/GenVisR/inst/doc/Intro.html)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw325)） `[重实现]`：R；`GenVisR` 的 CNV 瀑布/热图与染色体视图
- [x] [genome_coverage_plotter](https://github.com/matted/genome_coverage_plotter) `[重实现]`：Python；全基因组覆盖度折线
- [x] [Hagfish](https://github.com/mfiers/hagfish/wiki/Plots) `[重实现]`：Python；覆盖度堆叠与 CN 推断
- [x] [mocha](https://github.com/freeseek/mocha)（2018；[文献](https://doi.org/10.1038/s41586-018-0321-x)） `[重实现]`：C/WDL/R；等位基因特异的 CN 分段与可视化
- [x] [numbat](https://kharchenkolab.github.io/numbat/)（2022；[文献](https://doi.org/10.1101/2022.02.07.479314)） `[包]`：R；单细胞 CNV 的等位基因特异的 HMM 状态条带
- [x] [scCNV_heatmap](https://github.com/StefanKurtenbach/scCNV_heatmap) `[重实现]`：Python；单细胞 CNV 热图
- [x] [SeeNV](https://github.com/MSBradshaw/SeeNV)（2024；[文献](https://doi.org/10.1101/2024.05.08.593244)） `[重实现]`：Python；CNV + SV + 覆盖度三联缩略图
- [x] [sequenza](https://cran.r-project.org/web/packages/sequenza/vignettes/sequenza.html#plots-and-results)（2015；[文献](https://doi.org/10.1093/annonc/mdu479)） `[重实现]`：CRAN；深度比 vs B 等位频率二维散点 + 纯度/倍体轮廓

#### 8) `Python/CircosPlot.qmd` —— 29 条

现状：Python `pyCirclize` 扇区/弦图。建议补充多基因组环、质粒/细胞器环与比较基因组环的版式。

- [x] [BioCircos.js](http://bioinfo.ibp.ac.cn/biocircos/)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw041)） `[重实现]`：JS；环形轨道 + 弦的轻量实现
- [x] [BRICK (BRIG-like circular knowledgebase)](https://brick.ink/) `[重实现]`：JS；BRIG 的浏览器版，取同心环与比对弧的层级规则
- [x] [BRIG](http://brig.sourceforge.net)（2011；[文献](https://doi.org/10.1186/1471-2164-12-402)） `[重实现]`：桌面 Java；多基因组同心圆环 + 中心两两比对弧（Bizard 无此版式）
- [x] [cgplot](https://github.com/dfguan/cgplot) `[重实现]`：Python；环形基因组图
- [x] [CGView](http://cgview.ca/)（2008；[文献](https://doi.org/10.1093/nar/gkn179)） `[重实现]`：Java/JS；环状图轨道定义的事实标准（前身）
- [x] [CGView.js](https://js.cgview.ca/) `[重实现]`：JS；原核/细胞器基因组的双层环（正负链分列）+ 标注引线版式
- [x] [CHROMEISTER](https://github.com/estebanpw/chromeister)（2019；[文献](https://doi.org/10.1038/s41598-019-46773-w)） `[重实现]`：C+R；染色体间重排的环形图
- [x] [Circleator](http://jonathancrabtree.github.io/Circleator/index.html)（2014；[文献](https://doi.org/10.1093/bioinformatics/btu505)） `[重实现]`：Perl；按功能类别切扇区（而非固定 12 点）
- [x] [circlize](https://github.com/jokergoo/circlize)（2014；[文献](https://doi.org/10.1093/bioinformatics/btu393)） `[包]`：CRAN（已在 renv.lock）；R 侧环形布局的事实标准，`circos.genomicTrack` 系列与本图型一一对应
- [x] [Circos](http://circos.ca/)（2009；[文献](https://doi.org/10.1101/gr.092759.109)） `[重实现]`：Perl；karyotype / ticks 轨道命名与几何约定沿用其规范
- [x] [CircosJS](https://github.com/nicgirault/circosJS) `[重实现]`：JS；弦图式环形布局
- [x] [circularMT](https://github.com/msjimc/circularMT)（2024；[文献](https://doi.org/10.1093/bioinformatics/btae450)） `[重实现]`：C#；线粒体基因组的环状注释
- [x] [CVit](https://sourceforge.net/projects/cvit/)（2011；[文献](https://doi.org/10.1155/2011/373875)） `[重实现]`：Perl；环形基因组比较
- [x] [Cvit.js](https://github.com/LegumeFederation/cvitjs) `[重实现]`：JS；CVit 的浏览器版
- [x] [DNAPlotLib](https://github.com/VoigtLab/dnaplotlib)（2016；[文献](https://doi.org/10.1021/acssynbio.6b00252)） `[包]`：Python；合成生物学质粒/构建体的语义化环图
- [x] [DNAPlotter](https://www.sanger.ac.uk/science/tools/dnaplotter)（2008；[文献](https://doi.org/10.1093/bioinformatics/btn578)） `[重实现]`：Java；Artemis 风格的双链 CDS 内外环
- [x] [gbdraw](https://github.com/satoshikawato/gbdraw) `[包]`：Python（PyPI 0.14.5，MIT）；GenBank 直出，线性/环状双模式，GC content + GC skew 内外双轨
- [x] [GenomeProjector](https://github.com/gaou/g-language/wiki)（2009；[文献](https://doi.org/10.1186/1471-2105-10-31)） `[重实现]`：Perl；把基因组映射到环面的"柱面"布局（历史方案，作版式参考）
- [x] [genoVi](https://github.com/robotoD/GenoVi)（2023；[文献](https://doi.org/10.1371/journal.pcbi.1010998)） `[包]`：Python；COG 类别分色 + 多基因组环
- [x] [GGisy](https://github.com/Sanrrone/GGisy) `[重实现]`：Python+R；GC 含量/GC skew 的环形概览
- [x] [IslandViewer and IslandPlot](https://bl.ocks.org/lairdm/c6c235dbfa6e6ee61565)（2015；[文献](https://doi.org/10.1093/nar/gkv401)） `[重实现]`：Web/D3；基因组岛（PAI）的环形/线性标注
- [x] [JupiterPlot](https://github.com/JustinChu/JupiterPlot)（2018；[文献](https://doi.org/10.5281/zenodo.1241235)） `[重实现]`：Perl；Synteny + 覆盖度同框的环形总览
- [x] [mcutils](https://github.com/mchaisso/mcutils) `[重实现]`：R/C++；染色体级比对块的环形展示
- [x] [mummer2circos](https://github.com/metagenlab/mummer2circos) `[重实现]`：Python；把 MUMmer 比对结果直接映射成环形共线性
- [x] [OGDRAW](https://chlorobox.mpimp-golm.mpg.de/OGDraw.html)（2019；[文献](https://doi.org/10.1093/nar/gkz238)） `[重实现]`：Web；叶绿体/线粒体注释的 IRC/LSC/SSC 分区版式
- [x] [OmicCircos](http://bioconductor.org/packages/release/bioc/html/OmicCircos.html)（2014；[文献](https://doi.org/10.4137/CIN.S13495)） `[包]`：Bioconductor；CNV/SV 的环形多层轨道
- [x] [pyCirclize](https://github.com/moshi4/pyCirclize) `[包]`：已用于该教程；补充 genomic sector（区间坐标）示例
- [x] [pyCircos](https://github.com/ponnhide/pyCircos) `[包]`：Python；GC skew / GC content 内外双轨环形图
- [x] [vcf2circos](https://github.com/bioinfo-chru-strasbourg/vcf2circos) `[重实现]`：Python/Plotly；把 VCF 变异直接映射为环形轨道（基于 pcircos）

#### 9) `Omics/CollinearityPlot.qmd` —— 29 条

现状：`RIdeogram` 沿染色体色带示意共线性。建议补充多种点阵图（dotplot）风格、着色方案与局部重排展示。

- [x] [AliTV](https://alitvteam.github.io/AliTV/d3/AliTV.html)（2017；[文献](https://doi.org/10.7717/peerj-cs.116)） `[重实现]`：Web/JS/D3；交互式多基因组比对总览（上：覆盖曲线；中：比对带；下：基因箭头）
- [x] [asynt](https://github.com/simonhmartin/asynt) `[重实现]`：R；同源块的快速静态点阵
- [x] [D-GENIES](http://dgenies.toulouse.inra.fr/)（2018；[文献](https://doi.org/10.7717/peerj.4958)） `[包]`：Web；矩阵式多对多 dotplot + 每对独立配色版式
- [x] [dotPlotly](https://github.com/tpoorten/dotPlotly/) `[重实现]`：R/Shiny；交互缩放/框选的点阵（可作 Interactivity 示例）
- [x] [FlexiDot](https://github.com/molbio-dresden/flexidot)（2018；[文献](https://doi.org/10.1093/bioinformatics/bty395)） `[包]`：Python；热图式（覆盖率分箱）自比对，而非散点
- [x] [GENESPACE](https://github.com/jtlovell/GENESPACE/)（2022；[文献](https://doi.org/10.7554/eLife.78526)） `[重实现]`：R；gene space（非基因组坐标）的多物种同源顺序版式
- [x] [GenomicBreaks](https://oist.github.io/GenomicBreaks/articles/GenomicBreaks.html) `[重实现]`：R（GitHub）；`plotSubtracts` 的差异富集点阵（互易 vs 非互易比对）
- [x] [genoPlotR](http://genoplotr.r-forge.r-project.org/)（2010；[文献](https://doi.org/10.1093/bioinformatics/btq413)） `[包]`：CRAN；`plot_gene_map` 的分段注释（segmentation）+ 比对块（comparison）双对象模型
- [x] [gggenomes](https://github.com/thackl/gggenomes)（2024；[文献](https://doi.org/10.48550/arXiv.2411.13556)） `[包]`：CRAN；ggplot2 语法下多基因组轨 + 同源带（ribbon）+ GC 含量轨，是现有 RIdeogram 方案最直接的现代化替代
- [x] [ggplot2 mummerplot](https://jmonlong.github.io/Hippocamplus/2017/09/19/mummerplots-with-ggplot2/) `[重实现]`：坐标轴收缩（zoom）与反向匹配（+/-）着色约定
- [x] [Hobrac](https://github.com/Genoscope-LBGB/hobrac)（2026；[文献](https://doi.org/10.64898/2026.07.17.739168)） `[重实现]`：Python（2026）；同源参考搜索后直接出点阵（含 BUSCO 基因点阵）
- [x] [last-dotplot](https://gitlab.com/mcfrith/last) `[重实现]`：Python/C++；LAST 比对输出的点阵（可按注释分色）
- [x] [local-rearrangements](https://github.com/mcfrith/local-rearrangements) `[重实现]`：Python；带注释分色的比对块
- [x] [MCScan (python version)](https://github.com/tanghaibao/jcvi/wiki/MCscan-(Python-version))（2015；[文献](https://doi.org/10.1186/s13059-014-0573-1)） `[包]`：Python；`jcvi.graphics.karyotype` 的双/多物种 karyotype + 共线性块（PyPI 有 jcvi）
- [x] [Minidot](https://github.com/thackl/minidot) `[重实现]`：R；paf/coords 到 ggplot2 的最小转换器
- [x] [Mummer-idotplot](https://github.com/ryought/mummer-idotplot) `[重实现]`：Web/CLI；交互式点阵浏览
- [x] [Mummerplot](http://mummer.sourceforge.net/)（2004；[文献](https://doi.org/10.1186/gb-2004-5-2-r12)） `[重实现]`：Perl+GNUplot；经典三色（同向/反向/未匹配）点阵
- [x] [ntSynt](https://github.com/BirolLab/ntSynt-viz)（2026；[文献](https://doi.org/10.1093/jeb/voag079)） `[重实现]`：R（GitHub）；基于 gggenomes 的多基因组同源块
- [x] [odp (oxford dot plots)](https://github.com/conchoecia/odp)（2023；[文献](https://doi.org/10.1038/s41586-023-05936-6)） `[包]`：Python；多向同源的点阵矩阵
- [x] [oxford-plots](https://github.com/jherrero/oxford-plots) `[重实现]`：R/Perl；按 paf 过滤阈值的注释式点阵
- [x] [paf2dot](https://github.com/pangenome/paf2dot) `[重实现]`：GNUplot；PAF 到点阵的最小转换
- [x] [paf2dotplot](https://github.com/moold/paf2dotplot) `[重实现]`：R；dotPlotly 风格的 base-graphics 等价实现
- [x] [pafr](https://github.com/dwinter/pafr) `[重实现]`：R（CRAN 2025-07-10 归档，GitHub 存活）；`read_paf()` + `dotplot()` 的 PAF 直读接口，只作重实现参考
- [x] [pauvre](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6991124/figure/fig-3/)（2020；[文献](https://doi.org/10.7717/peerj.8356)） `[包]`：Python；基于 pysam 的比对/组装质控点阵
- [x] [plotsr](https://github.com/schneebergerlab/plotsr)（2022；[文献](https://doi.org/10.1101/2022.01.24.477489)） `[重实现]`：Python；染色体级倒置/易位事件块标注（PyPI 无包，需 conda/GitHub）
- [x] [StainedGlass](https://github.com/mrvollger/StainedGlass)（2022；[文献](https://doi.org/10.1093/bioinformatics/btac018)） `[包]`：R/Python/JS；滑动窗口 identity 热图 + 序列方向（λ 相位）的重复区自比对
- [x] [SyMap](http://www.agcol.arizona.edu/software/symap/)（2006；[文献](https://doi.org/10.1101/gr.5396706)） `[重实现]`：桌面；"泡状"（bubble）视图显示多床比对密度
- [x] [TidyLocalSynteny](https://github.com/cxli233/TidyLocalSynteny) `[重实现]`：R；局部（而非全基因组）共线性放大图
- [x] [WGDI](https://github.com/SunPengChuan/wgdi)（2021；[文献](https://doi.org/10.1101/2021.04.29.441969)） `[包]`：Python；多物种共线性 + Ks 分布联合图

#### 10) `Omics/SyntenyBlocksPlot.qmd` —— 22 条

现状：`syntR` 同源块。建议补充多基因组 ribbon/线性块图，并统一"方向编码"规范。

- [x] [chromsyn](https://github.com/slimsuite/chromsyn)（2022；[文献](https://doi.org/10.1101/2022.04.22.489119)） `[重实现]`：R；多向（multi-way）染色体级重排
- [x] [Cinteny](http://cinteny.cchmc.org/)（2007；[文献](https://doi.org/10.1186/1471-2105-8-82)） `[重实现]`：Web/Server；染色体重排距离与同源块
- [x] [DAGchainer](http://dagchainer.sourceforge.net/)（2004；[文献](https://doi.org/10.1093/bioinformatics/bth397)） `[重实现]`：Perl/Java；以 DAG 链式算法定义同源块（命名来源）
- [x] [hoodini](https://github.com/pentamorfico/hoodini) `[重实现]`：JS；多向同源的"排序不确定性"可视化
- [x] [LinearDisplay.pl](https://github.com/JCVenterInstitute/LinearDisplay)（2019；[文献](https://doi.org/10.1038/s41586-019-1742-x)） `[重实现]`：Perl；基因顺序的线性排布脚本
- [x] [lovis4u](https://github.com/art-egorov/lovis4u)（2024；[文献](https://doi.org/10.1101/2024.09.11.612399)） `[包]`：Python；多向同源块排序可视化
- [x] [MCScanX](https://github.com/wyp1125/MCScanX)（2012；[文献](https://doi.org/10.1093/nar/gkr1293)） `[重实现]`：Perl；共线性块的经典检测工具，其输出是多数共线性图的输入格式基准
- [x] [RectChr](https://github.com/BGI-shenzhen/RectChr) `[重实现]`：Perl；矩形染色体 + 共线性带（轻量静态）
- [x] [SafFire](https://mrvollger.github.io/SafFire/#ref=CHM13_v1.1&query=GRCh38) `[重实现]`：JS；交互式同源块浏览
- [x] [ShinySyn](https://doi.org/10.1093/bioinformatics/btac503)（2022；[文献](https://doi.org/10.1093/bioinformatics/btac503)） `[重实现]`：R/Shiny；交互式共线性查看
- [x] [SimpleSynteny](https://www.dveltri.com/simplesynteny/about.html)（2016；[文献](https://doi.org/10.1093/nar/gkw330)） `[重实现]`：Web；微共线性的简化版式
- [x] [Sybil](http://sybil.sourceforge.net/)（2007；[文献](https://doi.org/10.1007/978-1-59745-547-3_6)） `[重实现]`：Web/Perl；比较基因组浏览
- [x] [SynBrowse](https://doi.org/10.1093/bioinformatics/bti555)（2005；[文献](https://doi.org/10.1093/bioinformatics/bti555)） `[重实现]`：Web/Perl；GBrowse 的同源扩展
- [x] [Synima](https://github.com/rhysf/Synima)（2017；[文献](https://doi.org/10.1186/s12859-017-1939-7)） `[重实现]`：Perl；同源块的两两对照
- [x] [syntenyPlotByR](https://github.com/shingocat/syntenyPlotByR) `[重实现]`：R；极简 base-graphics 方案
- [x] [syntenyPlotteR](https://github.com/marta-fb/syntenyPlotteR)（2023；[文献](https://doi.org/10.1093/bioadv/vbad161)） `[包]`：CRAN；三套版式（linear / ribbon / color）切换
- [x] [SyntenyPortal](http://bioinfo.konkuk.ac.kr/synteny_portal/)（2016；[文献](https://doi.org/10.1093/nar/gkw310)） `[重实现]`：Web/JS；环 + 直带混合（同源块既画成弧也画成平行带）
- [x] [syntR](https://www.samuklab.com/syntR/articles/syntr_tutorial.html) `[包]`：已用于该教程
- [x] [SynTView](http://hub18.hosting.pasteur.fr/SynTView/documentation/)（2013；[文献](https://doi.org/10.1186/1471-2105-14-277)） `[重实现]`：桌面/Web；多基因组同源浏览
- [x] [SynVisio](https://synvisio.github.io/)（2022；[文献](https://doi.org/10.1007/978-1-0716-2067-0_16)） `[重实现]`：JS/D3；交互式多基因组共线性
- [x] [SYNY](https://github.com/PombertLab/SYNY)（2024；[文献](https://doi.org/10.1101/2024.05.09.593317)） `[重实现]`：Perl/Python；同源块的统计汇总与作图
- [x] [ziplign](https://github.com/martinghunt/ziplign) `[重实现]`：Godot；局部共线性对齐

#### 11) `Ranking/Lollipop.qmd` —— 9 条

现状：ggplot2 通用棒棒糖图（示例数据偏通用）。建议补充生物场景变体：蛋白结构域/PTM/变异位点定位。

- [x] [drawProteins](https://github.com/brennanpincardiff/drawProteins)（2018；[文献](https://doi.org/10.12688/f1000research.14541.1)） `[包]`：Bioconductor；`draw_canvas()` / `draw_chains()` / `draw_domains()` 的 UniProt 直读分层语法
- [x] [GeneViTo](http://athina.biol.uoa.gr/bioinformatics/GENEVITO/)（2003；[文献](https://doi.org/10.1186/1471-2105-4-53)） `[重实现]`：桌面 Java；蛋白结构域的基因级视图
- [x] [Lollipops](https://github.com/joiningdata/lollipops)（2016；[文献](https://doi.org/10.1371/journal.pone.0160519)） `[重实现]`：Go；高斯核平滑的突变密度底纹（Bizard 现为纯点+杆）
- [x] [nightingale protvista](https://ebi-webcomponents.github.io/nightingale/#/msa)（2017；[文献](https://doi.org/10.1093/bioinformatics/btx120)） `[重实现]`：Web/TS；Track / Variation / Sequence 三层的轨道化结构域图规范
- [x] [PFAM generate_graphic](https://pfam.xfam.org/generate_graphic) `[重实现]`：Web；Pfam 结构域架构的官方配色与排布
- [x] [Prosite MyDomains](https://prosite.expasy.org/mydomains)（2007；[文献](https://doi.org/10.1093/nar/gkm977)） `[重实现]`：Web；结构域架构的简化条带
- [x] [Protael](http://sanshu.github.io/protaelweb/)（2015；[文献](https://doi.org/10.1093/bioinformatics/btv605)） `[重实现]`：Web/JS；蛋白序列与注释的 SVG 输出
- [x] [ProteinPaint](https://pecan.stjude.cloud/proteinpaint)（2015；[文献](https://doi.org/10.1038/ng.3466)） `[重实现]`：Web/JS；以蛋白为主轴的变异/结构域/表达多轨道（St. Jude 儿科肿瘤常用）
- [x] [pViz](https://github.com/Genentech/pviz)（2014；[文献](https://doi.org/10.1093/bioinformatics/btu567)） `[重实现]`：Web/JS；蛋白特征与变异的轻量轨道

#### 12) `Hiplot/087-hic-heatmap.qmd` —— 2 条

现状：ggplot2 绘制 Hi-C 三角热图。建议补充旋转版式、分块边界与可导出的静态排版建议。

- [x] [HiCPlotter](https://github.com/kcakdemir/HiCPlotter)（2015；[文献](https://doi.org/10.1186/s13059-015-0767-1)） `[重实现]`：Python；Hi-C 热图与 TAD 边界叠加
- [x] [Juicebox](https://github.com/aidenlab/Juicebox)（2016；[文献](https://doi.org/10.1016/j.cels.2015.07.012)） `[重实现]`：Java；Hi-C 三角热图 + 归一化向量与 TAD 标注


> **下一条评论继续**：第三节（新增独立 qmd 172 条）、第四节（暂缓 206 条）、第五节（需调研 10 条）、第六节（依赖可用性与科学准确性红线）。

> 承上条评论：标题、第 0–2 节（与 #436 的差异、总览、补充进现有 qmd 186 条）见上一条评论。

### 三、✨ 新增独立 qmd（172 条 → 20 篇）

每篇给出"缺口"论证与条目勾选清单。括号内为建议文件名，可调整；`[重实现]` 需按条目注释里的图型描述自行实现。

#### 1) `Omics/SashimiPlot.qmd`（7 条）

缺口：Bizard 无剪接/读段覆盖弧形图。注：AGV 把 `GGsashimi` 标为 R，实际是 **Python 前端 + 调用 `R --vanilla --slave` 子进程**（`ggsashimi.py:591`），仅依赖 `pysam`，故本条目按 Python 实现更省依赖。

- [x] [GGsashimi](https://github.com/guigolab/ggsashimi)（2018；[文献](https://doi.org/10.1371/journal.pcbi.1006360)） `[重实现]`：Python 前端 + `R --vanilla` 后端（ggplot2/grid/gtable），仅依赖 pysam；多分组多轨 Sashimi 的参考实现
- [x] [IsoVis](https://isomix.org/isovis/)（2024；[文献](https://doi.org/10.1093/nar/gkae343)） `[重实现]`：Web/桌面；多长度亚型（isoform switch）的堆叠条 + 剪接事件矩阵
- [x] [SashimiPlot](https://miso.readthedocs.io/en/fastmiso/sashimi.html)（2015；[文献](https://doi.org/10.1093/bioinformatics/btv034)） `[重实现]`：Python（MISO）；原始 Sashimi 定义与 `--exon`/`--intron` 参数语义
- [x] [Slinker](https://github.com/Oshlack/Slinker)（2021；[文献](https://doi.org/10.12688/f1000research.74836.1)） `[包]`：Python；Sashimi + 转录本结构 + 蛋白结构域的三联纵向版式
- [x] [Spliceclust](https://github.com/pkimes/spliceclust) `[重实现]`：R；按剪接模式聚类后再画 Sashimi，可作进阶小节
- [x] [splicejam](https://github.com/jmw86069/splicejam)（2019；[文献](https://doi.org/10.1016/j.celrep.2019.08.093)） `[重实现]`：R；`geom_diagonal_wide` 风格剪接弧 + 热图联合
- [x] [Vials](http://vials.io/)（2016；[文献](https://doi.org/10.1109/TVCG.2015.2467911)） `[重实现]`：Web；剪接比率（ratio）而非 read count 的表达量轨

#### 2) `Omics/GeneFusionPlot.qmd`（8 条）

缺口：Bizard 无基因融合的可视化（转录本 + 断点 + 覆盖度三合一）。

- [x] [AGFusion](https://github.com/murphycj/AGFusion)（2016；[文献](https://doi.org/10.1101/080903)） `[重实现]`：Python；以蛋白结构域为坐标轴的融合（而非基因组坐标）
- [x] [arriba](https://github.com/suhrig/arriba)（2021；[文献](https://doi.org/10.1101/gr.257246.119)） `[重实现]`：C++/R；融合检测与可视化联动
- [x] [Chimeraviz](https://github.com/stianlagstad/chimeraviz)（2017；[文献](https://doi.org/10.1093/bioinformatics/btx329)） `[包]`：Bioconductor；`plot_fusion()`（双基因转录本 + 融合 junction 覆盖 + 核型）与 `plot_circle()`（RCircos 环形融合图），支持 STAR-Fusion/deFuse/EricScript 等导入
- [x] [clinker (gene fusion software)](https://github.com/Oshlack/Clinker/)（2018；[文献](https://doi.org/10.1093/gigascience/giy079)） `[包]`：R；多样本融合一致性的横向对比
- [x] [FusionInspector](https://github.com/FusionInspector/FusionInspector/wiki)（2021；[文献](https://doi.org/10.1101/2021.08.02.454639)） `[重实现]`：Perl/JS；融合的组装与读段验证视图
- [x] [GeneFuse](https://opengene.org/GeneFuse/report.html)（2018；[文献](https://doi.org/10.7150/ijbs.24626)） `[重实现]`：C/C++；融合断点的轻量静态示意
- [x] [MAVIS](http://mavis.bcgsc.ca/)（2018；[文献](https://doi.org/10.1093/bioinformatics/bty621)） `[包]`：Python；融合断点的证据分类色带（split reads / spanning reads / assembly）
- [x] [SVhawkeye](https://github.com/yywan0913/SVhawkeye)（2024；[文献](https://doi.org/10.3389/fgene.2024.1352443)） `[重实现]`：Python/R；融合/结构变异的读段证据视图

#### 3) `Omics/GenomeGraphPlot.qmd`（25 条）

缺口：Bizard 无组装/泛基因组图（GFA/sequence tube map）的静态重画，共 25 条，是本次最大的空白块。

- [x] [AGB assembly graph browser](https://github.com/almiheenko/AGB)（2019；[文献](https://doi.org/10.1093/bioinformatics/btz072)） `[重实现]`：Web/Python；组装图浏览
- [x] [Bandage](https://github.com/rrwick/Bandage/)（2015；[文献](https://doi.org/10.1093/bioinformatics/btv383)） `[重实现]`：桌面；GFA 图布局的视觉基准（节点=contig、边=overlap、单例=环形）
- [x] [BandageJS](https://github.com/GMOD/BandageJS) `[重实现]`：Web/TS；Bandage 的浏览器重写
- [x] [ctag](https://github.com/AbeelLab/ctag) `[重实现]`：Java/Scala；组装图的层次化布局
- [x] [gfaestus](https://github.com/chfi/gfaestus) `[重实现]`：Rust；GFA 图的 GPU 渲染
- [x] [gfalook](https://github.com/pangenome/gfalook) `[重实现]`：Rust；终端 GFA 布局
- [x] [GfaViz](https://github.com/ggonnella/gfaviz)（2018；[文献](https://doi.org/10.1093/bioinformatics/bty1046)） `[重实现]`：C++；GFA 的静态矢量输出
- [x] [ggbandage](https://github.com/GMOD/ggbandage) `[包]`：R/ggplot2（GitHub，GMOD）；把 Bandage 的组装图绘制成 ggplot2 图层
- [x] [ggtubemap](https://github.com/GMOD/ggtubemap) `[包]`：R/ggplot2（GitHub，GMOD）；sequenceTubeMap 式"管道"图——把 pangenome 路径画成缠绕的彩色管道
- [x] [graphgenomeviewer](https://cmdcolin.github.io/graphgenomeviewer/) `[重实现]`：Web/JS；图基因组浏览
- [x] [Icarus QUAST](http://quast.sourceforge.net/icarus.html)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw379)） `[重实现]`：Web；QUAST 报告中的组装图（Icarus）
- [x] [IGGE](https://github.com/immersivegraphgenomeexplorer/IGGE)（2021；[文献](https://doi.org/10.1109/SEGAH52098.2021.9551857)） `[重实现]`：交互式图基因组浏览器
- [x] [MoMi-G](https://github.com/MoMI-G/MoMI-G)（2019；[文献](https://doi.org/10.1186/s12859-019-3145-2)） `[重实现]`：Web/JS；多尺度变异图浏览
- [x] [odgi draw + odgi viz + odgi inject](https://odgi.readthedocs.io/en/latest/index.html)（2021；[文献](https://doi.org/10.1101/2021.11.10.467921)） `[重实现]`：C++；1D 排序（path-guided）布局，节点按路径顺序铺开而非力导向
- [x] [PanAbyss](https://github.com/Pange31/PanAbyss) `[重实现]`：Python/JS；cytoscape.js 的泛基因组图浏览
- [x] [panGraphViewer](https://github.com/TF-Chan-Lab/panGraphViewer) `[重实现]`：Python；泛基因组图 + 注释轨道
- [x] [PangyPlot](https://github.com/ScottMastro/pangyplot)（2025；[文献](https://doi.org/10.1101/2025.10.31.684064)） `[重实现]`：Python；泛基因组图绘制
- [x] [pgv (graph tool)](https://w-gao.github.io/pgv/) `[包]`：JS/Python；图浏览与坐标/注释检索
- [x] [sequence tube map](https://github.com/vgteam/sequenceTubeMap)（2019；[文献](https://doi.org/10.1093/bioinformatics/btz597)） `[重实现]`：Web/JS；把序列比对画成"管道"，等位/路径分叉用不同颜色分支
- [x] [SGTK (scaffold graph toolkit)](https://olga24912.github.io/SGTK/)（2018；[文献](https://doi.org/10.1093/bioinformatics/bty956)） `[重实现]`：JS/C++；scaffold 图的可视工具包
- [x] [Shasta](https://chanzuckerberg.github.io/shasta/ComputationalMethods.html#ReadGraph)（2020；[文献](https://doi.org/10.1038/s41587-020-0503-6)） `[重实现]`：Web/CLI；组装图的交互概览
- [x] [SloppyyGFA](https://github.com/mdondrup/SloppyyGFA) `[重实现]`：Web/TS/WebGL；大 GFA 的 WebGL 渲染
- [x] [strangepg](https://github.com/qwx9/strangepg) `[重实现]`：C；终端式图浏览
- [x] [VAG](https://github.com/lipingfangs/VAG)（2023；[文献](https://doi.org/10.1101/2023.01.20.524849)） `[包]`：Python/JS；泛基因组图的变异感知布局
- [x] [VRPG](https://github.com/codeatcg/VRPG)（2023；[文献](https://doi.org/10.1101/2023.01.20.524991)） `[重实现]`：Web/C++/Python；泛基因组区域的局部重建与显示

#### 4) `Omics/StructuralVariantPlot.qmd`（27 条）

缺口：Bizard 无结构变异（SV）专题图（弧线/带状/圆环拓扑），共 27 条。

- [x] [AmpliconArchitect](https://github.com/virajbdeshpande/AmpliconArchitect)（2019；[文献](https://doi.org/10.1038/s41467-018-08200-y)） `[重实现]`：Python；扩增子（ecDNA）的环状重排结构图
- [x] [asgart](https://github.com/delehef/asgart)（2018；[文献](https://doi.org/10.1093/bioinformatics/bty172)） `[重实现]`：Rust；SV 断点的弧线图
- [x] [bigly](https://github.com/brentp/bigly) `[重实现]`：Go/Python；SV 的读段支持与弧线
- [x] [Delly-maze](https://github.com/dellytools/maze)（2012；[文献](https://doi.org/10.1093/bioinformatics/bts378)） `[重实现]`：Python；SV 的"迷宫"式断点连接图
- [x] [Discoplot](https://github.com/mjsull/DiscoPlot)（2015；[文献](https://doi.org/10.7287/peerj.preprints.1038v1)） `[包]`：Python；断点连接（discoplot）的圆/弧表示
- [x] [Genome STRiP](http://software.broadinstitute.org/software/genomestrip/)（2015；[文献](https://doi.org/10.1038/ng.3200)） `[重实现]`：静态；群体级 SV 的染色体带状图
- [x] [GenomeView](https://github.com/nspies/genomeview)（2018；[文献](https://doi.org/10.1101/355636)） `[重实现]`：Python；聚合的 SV 堆叠条（VA）
- [x] [gGnomes](https://github.com/mskilab/gGnome) `[重实现]`：Web/R；SV 断点-区间（breakend）的嵌套表示
- [x] [gGnomes.js](https://github.com/mskilab/gGnome.js) `[重实现]`：Web/JS；同上，浏览器版
- [x] [Kero-BROWSE](https://kero.hgc.jp/examples/CLCL/hg38/index.html)（2017；[文献](https://doi.org/10.1093/nar/gkx1001)） `[重实现]`：Web/JS；SV 浏览
- [x] [LINX](https://github.com/hartwigmedical/hmftools/tree/master/linx)（2019；[文献](https://doi.org/10.1101/781013)） `[重实现]`：Java；SV 的聚类与链条可视化（PURPLE 生态）
- [x] [NeoLoopFinder](https://github.com/XiaoTaoWang/NeoLoopFinder)（2021；[文献](https://doi.org/10.1038/s41592-021-01164-w)） `[重实现]`：Python；Hi-C 上新形成环（neo-loop）的弧线标注
- [x] [Pairoscope](http://pairoscope.sourceforge.net/) `[重实现]`：C++；配对读段的"配对"可视化
- [x] [PopSV](https://github.com/jmonlong/PopSV/blob/master/3-Visualization.md)（2018；[文献](https://doi.org/10.1371/journal.pgen.1007285)） `[重实现]`：Web/R；群体 SV 的曼哈顿式与断点图
- [x] [PURPLE](https://github.com/hartwigmedical/hmftools/blob/master/purity-ploidy-estimator/README.md)（2019；[文献](https://doi.org/10.1101/781013)） `[重实现]`：SV/CN 的等位基因特异整合图
- [x] [ReConPlot](https://github.com/cortes-ciriano-lab/ReConPlot)（2023；[文献](https://doi.org/10.1101/2023.02.24.529890)） `[重实现]`：R（GitHub）；重构（reconstruction）事件的染色体弧线图
- [x] [Ribbon](http://genomeribbon.com/)（2020；[文献](https://doi.org/10.1093/bioinformatics/btaa680)） `[重实现]`：Web/JS；单基因组内重排的带状（ribbon）图
- [x] [Samplot](https://github.com/ryanlayer/samplot)（2021；[文献](https://doi.org/10.1186/s13059-021-02380-5)） `[包]`：Python；按表型分组的样本堆叠覆盖度 + 断点连线
- [x] [Savant](http://bioinformatics-ca.github.io/savant_genome_browser_lab_2015/)（2012；[文献](https://doi.org/10.1093/nar/gks427)） `[重实现]`：桌面 Java；SV 浏览
- [x] [Smash++](https://github.com/smortezah/smashpp)（2020；[文献](https://doi.org/10.1093/gigascience/giaa048)） `[重实现]`：C++；多基因组比对的压缩索引与点阵
- [x] [spinteny](https://github.com/skinner/spinteny) `[重实现]`：JS；多重同源（multi-way）的"纺锤"排序视图
- [x] [SVbyEye](https://htmlpreview.github.io/?https://github.com/daewoooo/SVbyEye/blob/master/man/doc/SVbyEye.html)（2025；[文献](https://doi.org/10.1093/bioinformatics/btaf332)） `[重实现]`：R（GitHub，MIT）；"自比对 + 互比对"双联（plotAVA），颜色编码 identity 与方向
- [x] [SVPV](https://github.com/VCCRI/SVPV)（2017；[文献](https://doi.org/10.1093/bioinformatics/btx117)） `[重实现]`：Python；SV 的读段证据与断点
- [x] [SVTopo](https://github.com/PacificBiosciences/SVTopo)（2025；[文献](https://doi.org/10.1186/s12864-025-12088-6)） `[重实现]`：Rust（2025）；SV 的拓扑示意
- [x] [svv](https://github.com/ryanlayer/svv) `[包]`：Python；SV 的读段堆叠与断点
- [x] [svviz](https://github.com/svviz/svviz)（2015；[文献](https://doi.org/10.1093/bioinformatics/btv478)） `[包]`：Python；SV 的读段比对证据图
- [x] [svviz2](https://github.com/nspies/svviz2)（2015；[文献](https://doi.org/10.1093/bioinformatics/btv478)） `[重实现]`：Python；svviz 的重写，长读长支持

#### 5) `Omics/WholeGenomeAlignmentPlot.qmd`（8 条）

缺口：Bizard 无全基因组比对全景图（Mauve 式块状比对）。

- [x] [A-liner](https://github.com/mokuno3430/a-liner)（2026；[文献](https://doi.org/10.1093/bioinformatics/btag408)） `[重实现]`：Python（2026）；基因/外显子/散点/高亮四类轨道的线性比对
- [x] [Comparative assembly hub snake track](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4296145/)（2014；[文献](https://doi.org/10.1093/bioinformatics/btu534)） `[重实现]`：Web；比较组装 hub 的"蛇形"轨道（多物种比对沿一条折线展开）
- [x] [DNASkittle](https://www.dnaskittle.com/)（2009；[文献](https://doi.org/10.1186/1471-2105-10-452)） `[重实现]`：Web/Python/JS；把长序列压缩成"可读"的像素带
- [x] [FluentDNA](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7203487/)（2020；[文献](https://doi.org/10.3389/fgene.2020.00292)） `[重实现]`：Web；DNA 序列可读化（把碱基当文本排版）
- [x] [Genome-plots-processing](https://github.com/filip-husnik/genome-plots-processing)（2016；[文献](https://doi.org/10.1073/pnas.1603910113)） `[重实现]`：Processing；按同源块对齐多基因组基因箭头（而非按自身坐标）
- [x] [mauve-viewer](https://github.com/nconrad/mauve-viewer) `[重实现]`：Web/JS；Mauve 同源块（LCB）沿基因组的色块重排图
- [x] [progressiveMauve viewer](http://darlinglab.org/mauve/user-guide/viewer.html)（2004；[文献](https://doi.org/10.1101/gr.2289704)） `[重实现]`：桌面 Java；渐进式多基因组比对的块重排视图
- [x] [SequenceSurveyor](https://graphics.cs.wisc.edu/Vis/SequenceSurveyor/index.html)（2011；[文献](https://doi.org/10.1109/TVCG.2011.232)） `[重实现]`：Adobe AIR；像素级（pixel-level）基因组概览，每像素=固定 bp

#### 6) `Omics/MultiGenomeComparisonPlot.qmd`（11 条）

缺口：Bizard 无"多基因组并排 + 同源块连线"的比较视图（与 `SyntenyBlocksPlot` 的块内视角互补）。

- [x] [AccuSyn](https://accusyn.usask.ca/) `[重实现]`：JS；一致性（consensus）同源块在多样本间的堆叠
- [x] [CHITRA](https://github.com/BioinformaticsOnLine/CHITRA)（2026；[文献](https://doi.org/10.1093/bioadv/vbag200)） `[重实现]`：Web/TS/D3（2026）；同源块 + 基因箭头 + 连续尺度的多物种比对
- [x] [CrowsNest](https://pgsb.helmholtz-muenchen.de/crowsnest/help.html)（2016；[文献](https://doi.org/10.1007/978-1-4939-3167-5_8)） `[重实现]`：Python；circos 风格的基因邻域圆图
- [x] [EasyFig](https://mjsull.github.io/Easyfig/)（2011；[文献](https://doi.org/10.1093/bioinformatics/btr039)） `[包]`：Python/GUI；BLAST 比对灰阶带 + 基因箭头的经典版式（可读性基准）
- [x] [GenomeSyn](https://github.com/jmsong2/GenomeSyn) `[重实现]`：Perl；多基因组共线性（GenomeSyn 系列）
- [x] [Gview](https://server.gview.ca/examples)（2010；[文献](https://doi.org/10.1093/bioinformatics/btq588)） `[重实现]`：Perl/Java；多基因组环状 + 中心比较网格模板
- [x] [Mumemto](https://github.com/vikshiv/mumemto)（2025；[文献](https://doi.org/10.1186/s13059-025-03644-0)） `[重实现]`：Python/C++；多向同源块的"排序不确定性"可视化
- [x] [NGenomeSyn](https://github.com/hewm2008/NGenomeSyn)（2023；[文献](https://doi.org/10.1093/bioinformatics/btad121)） `[重实现]`：Perl；线性 + 环状 + 共线性三合一
- [x] [SyntenyPair Explorer](https://github.com/GibbonsLabGenomics/SyntenyPair-Explorer)（2026；[文献](https://doi.org/10.64898/2026.07.23.740353)） `[重实现]`：Web/JS（2026）；两两同源探索
- [x] [VicMAG](https://github.com/TsudaYusuke/VicMAG)（2026；[文献](https://doi.org/10.1093/nargab/lqag109)） `[重实现]`：Python（2026）；大量 MAG 并列环的批量排版（小多图 layout 问题）
- [x] [XMatchView](https://www.bcgsc.ca/resources/software/xmatchview)（2018；[文献](https://doi.org/10.21105/joss.00497)） `[重实现]`：桌面/Python；比对断点的点阵与注释

#### 7) `Omics/LinkageDisequilibriumPlot.qmd`（2 条）

缺口：Bizard 无连锁不平衡（LD）热图/块图，仅 2 条但都是经典实现，值得独立成文。

- [x] [Haploview](https://www.broadinstitute.org/haploview/haploview)（2004；[文献](https://doi.org/10.1093/bioinformatics/bth457)） `[重实现]`：桌面 Java；三角 LD 菱形网格（D′ / r² 双色标）+ block 划分
- [x] [LDBlockShow](https://github.com/BGI-shenzhen/LDBlockShow)（2020；[文献](https://doi.org/10.1093/bib/bbaa227)） `[重实现]`：C++/Perl；LD block 三角 + 基因结构轨 + 多群体分面三合一（首推）

#### 8) `Omics/GeneticMapPlot.qmd`（6 条）

缺口：Bizard 无遗传图谱/比较遗传图谱（CMap 式）。

- [x] [chromPlot](https://www.bioconductor.org/packages/release/bioc/vignettes/chromPlot/inst/doc/chromPlot.pdf)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw137)） `[包]`：Bioconductor；以 Genetic map（cM）为坐标轴，RIdeogram/karyoploteR 均不支持遗传距离轴
- [x] [CMap](http://gmod.org/wiki/CMap)（2009；[文献](https://doi.org/10.1093/bioinformatics/btp458)） `[重实现]`：Web/Perl；遗传图谱（cM）与物理图谱（bp）双轴对齐——Bizard 无此图型
- [x] [PhenoGram](https://ritchielab.org/software/phenogram-downloads)（2013；[文献](https://doi.org/10.1186/1756-0381-6-18)） `[重实现]`：Web/CLI/Ruby；以染色体为轴的基因/表型位置图
- [x] [pretzel](https://github.com/plantinformatics/pretzel)（2019；[文献](https://doi.org/10.1101/517953)） `[重实现]`：JS/D3；环形单倍型 + 基因模型
- [x] [Tripal MapViewer](https://github.com/statonlab/tripal_map)（2019；[文献](https://doi.org/10.1093/database/baz100)） `[重实现]`：PHP/JS；Tripal 站点的遗传图谱浏览
- [x] [vega](http://vega.archive.ensembl.org/Homo_sapiens/Location/Chromosome?r=6-QBL)（2004；[文献](https://doi.org/10.1093/nar/gki135)） `[重实现]`：Web；多物种 orthologue 的基因上下文（最经典的"基因页"版式）

#### 9) `Omics/PlasmidMap.qmd`（8 条）

缺口：Bizard 无质粒/载体图谱（合成生物学场景）。

- [ ] [ApE (a plasmid editor)](https://jorgensen.biology.utah.edu/wayned/ape/) `[重实现]`：桌面；质粒编辑器经典版式
- [x] [clonifier](https://dlesl.github.io/clonifier/) `[重实现]`：Web/Rust；克隆构建流程（多片段拼接）的分段图
- [x] [OpenVectorEditor](https://teselagen.github.io/tg-oss/ove/#/Editor) `[重实现]`：JS；线性/环状切换 + 特征箭头 + 引物对
- [x] [plascad](https://github.com/David-OConnor/plascad) `[重实现]`：Rust；质粒注释与图
- [x] [plasmapR](https://github.com/BradyAJohnston/plasmapR) `[包]`：R/ggplot2（GitHub，95★）；`read_gb()` + `ggplasmid()` 直接产 ggplot2 对象
- [x] [seqviz](https://github.com/Lattice-Automation/seqviz#viewer) `[重实现]`：Web；序列/质粒的交互浏览（可导静态图）
- [ ] [SnapGene](https://www.snapgene.com/) `[重实现]`：商业；仅作视觉规范参考（特征箭头、酶切位点刻度环），不引入
- [x] [VectorBee](https://www.vectorbee.com/en/) `[重实现]`：质粒/载体图的轻量工具

#### 10) `Omics/ChromatogramPlot.qmd`（2 条）

缺口：Bizard 无 Sanger 测序峰图（.ab1/.scf 直读）。

- [x] [Consed](http://bozeman.mbt.washington.edu/consed/consed.html)（1998；[文献](https://doi.org/10.1101/gr.8.3.195)） `[重实现]`：桌面；多读段装配视图的峰图堆叠
- [x] [Staden](https://sourceforge.net/projects/staden/)（2010；[文献](https://doi.org/10.1093/bioinformatics/btq268)） `[重实现]`：桌面；Sanger 装配与峰图的经典界面

#### 11) `Omics/MethylationTrack.qmd`（10 条）

缺口：Bizard 无甲基化/长读长修饰碱基轨道（含纳米孔原始信号）。

- [x] [AnnoJ](https://brainome.ucsd.edu/howto_annoj.html) `[重实现]`：Web/JS；甲基化与信号轨道的浏览器式布局
- [x] [CurrentView](https://github.com/genometechlab/currentview) `[重实现]`：Python；纳米孔离子电流信号与比对叠加
- [x] [Mahi-mahi](https://github.com/nanoporetech/mahi-mahi) `[重实现]`：Python；逐读段 motif 组成 + CpG 甲基化（分单倍型）
- [x] [Methplotlib](https://github.com/wdecoster/methplotlib)（2020；[文献](https://doi.org/10.1093/bioinformatics/btaa093)） `[包]`：Python（PyPI 1.0.0）；按甲基化频率分箱的瀑布图
- [x] [methylartist](https://github.com/adamewing/methylartist)（2021；[文献](https://doi.org/10.1101/2021.07.22.453313)） `[包]`：Python（PyPI 1.6.0）；单倍型分列的 motif 组成 + CpG 甲基化
- [x] [modbamtools](https://github.com/rrazaghi/modbamtools)（2022；[文献](https://doi.org/10.1101/2022.07.07.499188)） `[包]`：Python；逐读段（per-read）修饰碱基编码
- [x] [NanoMethViz](http://www.bioconductor.org/packages/release/bioc/html/NanoMethViz.html)（2021；[文献](https://doi.org/10.1371/journal.pcbi.1009524)） `[包]`：Bioconductor；甲基化比例热图 + 覆盖度轨 + 转录本轨
- [x] [Squigualiser](https://github.com/hiruna72/squigualiser)（2024；[文献](https://doi.org/10.1093/bioinformatics/btae501)） `[包]`：Python（PyPI 0.7.0）；纳米孔原始信号比对的可交互 HTML 图
- [x] [TE-nest](https://github.com/cmdcolin/TEnest)（2007；[文献](https://doi.org/10.1104/pp.107.110353)） `[重实现]`：Web/Perl；转座子嵌套结构图（历史方案）
- [x] [TRVZ (TRGT tandem repeat genotyping visualizer)](https://github.com/PacificBiosciences/trgt)（2023；[文献](https://doi.org/10.1101/2023.05.12.540470)） `[重实现]`：Rust；按单倍型展开的串联重复基因型图

#### 12) `Omics/RepeatStructurePlot.qmd`（9 条）

缺口：Bizard 无重复序列/串联重复（TR）结构与自比对点阵图。

- [x] [GraphAlignmentViewer](https://github.com/Illumina/GraphAlignmentViewer) `[重实现]`：Python；重复区图比对的读段路径
- [x] [miropeats](http://www.littlest.co.uk/software/bioinf/old_packages/miropeats/)（1995；[文献](https://doi.org/10.1093/bioinformatics/11.6.615)） `[重实现]`：Perl/C；镜像重复（mirror repeat）的"镜像"布局（历史）
- [x] [ModDotPlot](https://github.com/marbl/ModDotPlot)（2024；[文献](https://doi.org/10.1101/2024.04.15.589623)） `[包]`：Python（PyPI 1.0.1，MIT）；直接用 k-mer 相似度算自比对热图（不依赖 MUMmer），最可在 Bizard 内纯代码复现
- [x] [PipMaker](http://pipmaker.bx.psu.edu/pipmaker/)（2000；[文献](https://doi.org/10.1101/gr.10.4.577)） `[重实现]`：Web；重复序列的两序列比对"百分比 identity 图"（历史经典）
- [x] [ProleTRact](https://github.com/Lionward/ProleTRact)（2026；[文献](https://doi.org/10.1093/nargab/lqag096)） `[重实现]`：Web/TS（2026）；重复位点的多等位比较
- [x] [REPAVER](https://gitlab.com/gringer/bioinfscripts/)（[文献](https://doi.org/10.13140/RG.2.2.15289.39522)） `[重实现]`：R/C++；重复区覆盖与堆叠
- [x] [REViewer](https://github.com/Illumina/REViewer)（2021；[文献](https://doi.org/10.1101/2021.10.20.465046)） `[重实现]`：C++；重复区比对的读段堆叠（STR 基因型）
- [x] [SODA](https://sodaviz.org/)（2022；[文献](https://doi.org/10.1093/nargab/lqac077)） `[重实现]`：JS/TS；短串联重复的 motif 组成图
- [x] [TRviz](https://github.com/Jong-hun-Park/trviz)（2023；[文献](https://doi.org/10.1093/bioadv/vbad058)） `[包]`：Python（PyPI 1.4.2）；串联重复 motif 分解 + 多等位比较

#### 13) `Omics/LocalAncestryPlot.qmd`（5 条）

缺口：Bizard 无局部祖源/渗入（local ancestry）沿染色体可视化。

- [x] [fineSTRUCTURE aka ChromoPainter](http://www.paintmychromosomes.com/)（2012；[文献](https://doi.org/10.1371/journal.pgen.1002453)） `[重实现]`：共祖（coancestry）热图
- [x] [GeMo](https://github.com/SouthGreenPlatform/GeMo)（2022；[文献](https://doi.org/10.1093/database/baac057)） `[重实现]`：JS；染色体内祖源转换点标注
- [x] [Introgression browser](https://git.wageningenur.nl/aflit001/ibrowser)（2015；[文献](https://doi.org/10.1111/tpj.12800)） `[重实现]`：Web/Python；渗入片段沿基因组的带状图
- [x] [MOSAIC](https://maths.ucd.ie/~mst/MOSAIC/)（2019；[文献](https://doi.org/10.1534/genetics.119.302139)） `[重实现]`：R；堆叠面积（ancestry painting）沿染色体的祖源占比——本类唯一原生 R 的高质量参考
- [x] [tagore](https://github.com/jordanlab/tagore) `[包]`：Python（PyPI）；局部祖源的简化版（便于教学）

#### 14) `Omics/PhylogenyGenomePlot.qmd`（5 条）

缺口：Bizard 无"系统发生 + 基因组"联合视图（病原体基因组流行病学常用）。

- [x] [Gingr](https://harvest.readthedocs.io/en/latest/content/gingr.html)（2014；[文献](https://doi.org/10.1186/s13059-014-0524-x)） `[重实现]`：C++；基于系统发生树的同源块沿树堆叠
- [x] [Nextclade](https://clades.nextstrain.org/)（2021；[文献](https://doi.org/10.21105/joss.03773)） `[重实现]`：JS/React/D3；树 + 突变矩阵 + 基因注释三轨对齐（病原体基因组监测标准图）
- [x] [Panoptes](https://www.malariagen.net/apps/ag1000g/phase1-AR3/index.html)（2017；[文献](https://doi.org/10.1093/bioinformatics/btx410)） `[重实现]`：Python/JS；树 + 变异密度热图联合
- [x] [Taxonium](https://taxonium.org)（2022；[文献](https://doi.org/10.7554/eLife.82392)） `[重实现]`：Web/Python；大规模树 + 元数据色环
- [x] [TreeBrowser (MicrobesOnline)](http://www.microbesonline.org/cgi-bin/treeBrowse.cgi?locus=17761)（2009；[文献](https://doi.org/10.1093/nar/gkp919)） `[重实现]`：Web/Portal；基因组上下文沿树展开

#### 15) `Omics/BlastAlignmentPlot.qmd`（15 条）

缺口：Bizard 无比对/读段堆叠（read pileup / pairwise alignment）的出版级排版，共 15 条。

- [x] [alignoth](http://htmlpreview.github.io/?https://github.com/koesterlab/alignoth/blob/main/examples/plot.html)（2025；[文献](https://doi.org/10.1093/bioinformatics/btaf663)） `[重实现]`：Rust/Vega；Vega 规范描述多读段堆叠
- [x] [bamscope](https://github.com/jslfree080/bamscope) `[重实现]`：Kotlin；BAM 的终端/图形核对视图
- [x] [BAMSnap](https://github.com/parklab/bamsnap)（2021；[文献](https://doi.org/10.1093/bioinformatics/btaa1101)） `[包]`：Python；读段比对的静态快照（论文插图常用）
- [x] [Bamsnap-LRS](https://github.com/comery/Bamsnap-LRS)（2026；[文献](https://doi.org/10.64898/2026.06.21.733121)） `[重实现]`：Python；长读长比对的批量快照
- [x] [BLAST+ (reading -outfmt 0 pairwise output is the original CLI bioinformatics!)](ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/LATEST/)（2009；[文献](https://doi.org/10.1186/1471-2105-10-421)） `[重实现]`：成对比对的 match/mismatch 字符图谱（+/-/空白），仍在命令行与论文中使用
- [x] [DrukBam](https://github.com/StephanHolgerD/DrukBam) `[包]`：Python；BAM 覆盖度的快速折线
- [x] [GSSPlayground](https://github.com/orangeSi/GSSplayground) `[重实现]`：Perl；多读段比对的"序列图谱"
- [x] [Hapviz](https://github.com/ekg/hapviz) `[重实现]`：C；单倍型/读段的紧凑可视化
- [x] [JVarKit BamToSVG](http://lindenb.github.io/jvarkit/BamToSVG.html)（2015；[文献](https://doi.org/10.6084/m9.figshare.1425030)） `[重实现]`：Java；BAM 到 SVG 的读段堆叠
- [x] [plotReads](http://campuspress.yale.edu/knightlab/ruddle/plotreads/) `[重实现]`：静态/CLI；读段堆叠的极简绘图
- [x] [pyBamView](https://mgymrek.github.io/pybamview/)（2014；[文献](https://doi.org/10.1093/bioinformatics/btu565)） `[包]`：Python；浏览器中的 BAM 覆盖度与读段
- [x] [Sam2pairwise](https://github.com/mlafave/sam2pairwise)（2014；[文献](https://doi.org/10.5281/zenodo.11377)） `[重实现]`：C++；SAM 到成对比对字符图
- [x] [SequenceServer](https://sequenceserver.com/)（2019；[文献](https://doi.org/10.1093/molbev/msz185)） `[重实现]`：Web；BLAST 结果的图形化摘要
- [x] [shabam](https://github.com/dlrice/shabam) `[重实现]`：Python；读段 + 变异 + 覆盖度联合视图
- [x] [VizAln (from HipSTR)](https://github.com/tfwillems/HipSTR)（2017；[文献](https://doi.org/10.1038/nmeth.4267)） `[重实现]`：终端；STR 区读段比对的可视化

#### 16) `Omics/VariantAnnotationPlot.qmd`（7 条）

缺口：Bizard 无变异注释/等位基因频率沿坐标的紧凑图（含单倍型相位图）。

- [x] [NucFreq](https://github.com/mrvollger/NucFreq) `[重实现]`：Python/R；核苷酸频率沿基因组的堆叠
- [x] [phasius](https://github.com/wdecoster/phasius) `[重实现]`：Python；单倍型相位图
- [x] [snpit](https://github.com/aineniamh/snipit) `[重实现]`：Python；SNP 的等位基因频率图
- [x] [SNPitty](https://bitbucket.org/ccbc/snpitty/src/master/)（2018；[文献](https://doi.org/10.1016/j.jmoldx.2017.11.011)） `[重实现]`：Web/R；SNP 的等位基因频率与 CNV 联合图
- [x] [vcfPlotein](https://vcfplotein.liigh.unam.mx/#/) `[重实现]`：JS/D3/Vue；VCF 的染色体概览与密度
- [x] [VIVA](https://www.biorxiv.org/content/10.1101/589879v1.full.pdf)（2019；[文献](https://doi.org/10.1038/s41598-019-49114-z)） `[重实现]`：Web；变异位点的读段证据图（Variant Visualizer）
- [x] [XVCF](https://github.com/rashidma/XVCF)（2026；[文献](https://doi.org/10.1016/j.slast.2026.100447)） `[重实现]`：R/Shiny（2026）；VCF/体细胞变异的质控图

#### 17) `Omics/ProteinDomainPlot.qmd`（5 条）

缺口：Bizard 无蛋白结构域/序列特征图解（与 `Ranking/Lollipop` 的 domain 小节需明确分工）。

- [x] [DnaFeaturesViewer](https://github.com/Edinburgh-Genome-Foundry/DnaFeaturesViewer)（2020；[文献](https://doi.org/10.1093/bioinformatics/btaa213)） `[包]`：Python（PyPI 3.1.5）；质粒/核酸特征的紧凑线性图
- [x] [Genomics 2 Proteins Portal](https://g2p.broadinstitute.org/)（2024；[文献](https://doi.org/10.1038/s41592-024-02409-0)） `[重实现]`：Web；基因组变异到蛋白结构域的映射图
- [x] [IBS (Illustrator for Biological Sequences)](http://ibs.biocuckoo.org/)（2015；[文献](https://doi.org/10.1093/bioinformatics/btv362)） `[重实现]`：Java；序列特征的"图解"式排版（结构域/变异/引物）
- [x] [Mason](http://www.yeastrc.org/mason/) `[重实现]`：Web；蛋白特征的快速示意
- [x] [TogoStanza Protein Browser](https://db-dev.jpostdb.org/ts/stanza/protein_browser/help.html) `[重实现]`：JS；蛋白特征的 RDF 驱动视图

#### 18) `Python/GenomeTrackPlot.qmd`（9 条）

缺口：Bizard 的 Python 侧无"多轨道基因组轨道图"（pyGenomeTracks 系），这是 Python 侧最值得补的一篇。

- [x] [CoolBox](https://github.com/GangCaoLab/CoolBox)（2021；[文献](https://doi.org/10.1186/s12859-021-04408-w)） `[包]`：Python（PyPI 0.4.0）；Jupyter 内联的 Hi-C 浏览器式布局
- [x] [ezcharts](https://github.com/epi2me-labs/ezcharts) `[包]`：Python；ONT 生态的标准化图表（含基因组轨道）
- [x] [figeno](https://github.com/CompEpigen/figeno)（2024；[文献](https://doi.org/10.1093/bioinformatics/btae354)） `[包]`：Python（PyPI 1.9.0）；Hi-C + 甲基化 + 读段三合一（新一代替代）
- [x] [gcMapExplorer](https://github.com/rjdkmr/gcMapExplorer)（2017；[文献](https://doi.org/10.1093/nar/gkx644)） `[包]`：Python；Hi-C 矩阵的桌面浏览与导出
- [x] [gtracks](https://gitlab.com/salk-tm/gtracks) `[包]`：Python；pyGenomeTracks 的封装（最小示例）
- [x] [pyGenomeTracks](https://github.com/deeptools/pyGenomeTracks)（2020；[文献](https://doi.org/10.1093/bioinformatics/btaa692)） `[包]`：Python（PyPI 3.9）；命令行/INI 配置直出出版级多轨道图，跨语言可复现性最好
- [x] [python-genome-browser aka pygbrowse](https://github.com/phageghost/python-genome-browser/blob/master/pygbrowse_demonstration.ipynb) `[重实现]`：Python；Hi-C/覆盖度/基因结构的笔记本式浏览
- [x] [SparK](https://github.com/harbourlab/SparK)（2019；[文献](https://doi.org/10.1101/845529)） `[重实现]`：Python；Hi-C 的稀疏矩阵浏览
- [x] [trackplot (python)](https://github.com/ygidtu/trackplot)（2023；[文献](https://doi.org/10.1371/journal.pcbi.1011477)） `[包]`：Python（PyPI 0.7.1）；覆盖度 + 基因结构的命令行出图

#### 19) `Python/GenomeComparisonCircos.qmd`（1 条）

缺口：Python 侧无多基因组环形比较（pyGenomeViz），与 `Python/CircosPlot.qmd` 的"扇区/弦图"口径不同，单独成文更清晰。

- [x] [pyGenomeViz](https://github.com/moshi4/pyGenomeViz) `[包]`：Python（PyPI 1.7.0，MIT）；多基因组环形比较，补充"多基因组"版式

#### 20) `Python/MicrobiomeGenomePlot.qmd`（2 条）

缺口：Python 侧无微生物/比较基因组交互式视图的静态重画（Anvi'o 的静态图版式）。

- [x] [Anvio](https://peerj.com/articles/1319/)（2015；[文献](https://doi.org/10.7717/peerj.1319)） `[包]`：Python；宏基因组/比较基因组的交互界面（其静态图可参考）
- [x] [Chromatiblock](https://github.com/mjsull/chromatiblock)（2020；[文献](https://doi.org/10.21105/joss.02451)） `[重实现]`：Python；泛基因组同源块的矩阵图

### 四、⏸️ 暂缓（206 条）

暂缓 ≠ 无价值：这些条目的图型多数已被第二、三节吸收，只是"以平台/工具形态存在"，无法作为静态可复现的教程选题。

#### 4.1 交互式基因组浏览器（browser，41 条）

交互式基因组浏览器（IGV/JBrowse/UCSC/Ensembl/HiGlass/Gosling 等）。它们是"平台"而非"图型"，静态截图需依赖真实数据加载与交互状态；其中可静态化的矩阵轨道与多轨道布局已在 `Python/GenomeTrackPlot.qmd` 吸收。

- [ ] [ABrowse (genome browser)](https://doi.org/10.1186/1471-2105-13-2)（2012；[文献](https://doi.org/10.1186/1471-2105-13-2)）
- [ ] [Argo](https://web.archive.org/web/20070924141423/http://www.broad.mit.edu/annotation/argo/)
- [ ] [BasePlayer](https://baseplayer.fi/)（2018；[文献](https://doi.org/10.1038/s41596-018-0052-3)）
- [ ] [Biodalliance](http://www.biodalliance.org/)（2011；[文献](https://doi.org/10.1093/bioinformatics/btr020)）
- [ ] [Biodalliance comparative demo](http://biodalliance.org/dev/test-comparative.html)（2011；[文献](https://doi.org/10.1093/bioinformatics/btr020)）
- [ ] [Celera genome browser](https://www.csee.umbc.edu/~turner/presentations/bvw2002/sld009.htm)
- [ ] [cisGenome Browser](https://jhui2014.github.io/browser/screenshots.html)（2010；[文献](https://doi.org/10.1093/bioinformatics/btq286)）
- [ ] [CodeXomics](https://scilence2022.github.io/CodeXomics/)
- [ ] [CrossBrowse](https://github.com/shenkers/CrossBrowse)（2018；[文献](https://doi.org/10.1101/272880)）
- [ ] [EaSeq](https://easeq.net/screenshots/)（2016；[文献](https://doi.org/10.1038/nsmb.3180)）
- [ ] [Ensembl genome browser](https://useast.ensembl.org/Homo_sapiens/Location/View?r=17:63992802-64038237)（2002；[文献](https://doi.org/10.1093/nar/30.1.38)）
- [ ] [Ensembl genome browser 2020 edition](http://2020.ensembl.org/)
- [ ] [Epiviz](https://epiviz.github.io/)（2020；[文献](https://doi.org/10.1093/bioinformatics/btaa591)）
- [ ] [GBrowse 2](http://gmod.org/wiki/GBrowse)（2013；[文献](https://doi.org/10.1093/bib/bbt001)）
- [ ] [GenomeMaps](http://www.genomemaps.org/)（2013；[文献](https://doi.org/10.1093/nar/gkt530)）
- [ ] [Genoverse](https://github.com/wtsi-web/Genoverse)
- [ ] [Gnomad browser](https://github.com/macarthur-lab/gnomadjs)（2020；[文献](https://doi.org/10.1038/s41586-020-2308-7)）
- [ ] [GTEX Locus browser](https://gtexportal.org/home/locusBrowserPage/ACTN3)（2020；[文献](https://doi.org/10.1126/science.aaz1776)）
- [ ] [GWAS catalog browser](https://www.ebi.ac.uk/gwas/variants/rs1558902)（2018；[文献](https://doi.org/10.1093/nar/gky1120)）
- [ ] [HiGlass](https://higlass.io)（2018；[文献](https://doi.org/10.1186/s13059-018-1486-1)）
- [ ] [IGB](https://bioviz.org/)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw069)）
- [ ] [IGV](https://igv.org/)（2011；[文献](https://doi.org/10.1038/nbt.1754)）
- [ ] [IGV.js](https://github.com/igvteam/igv.js/)（2020；[文献](https://doi.org/10.1101/2020.05.03.075499)）
- [ ] [JBR](https://artyomovlab.wustl.edu/jbr/2018_h3k27ac_aging/)（2021；[文献](https://doi.org/10.1093/bioinformatics/btab376)）
- [ ] [JBrowse](http://jbrowse.org/jbrowse1.html)（2016；[文献](https://doi.org/10.1186/s13059-016-0924-1)）
- [ ] [JBrowse 2](http://jbrowse.org/jb2)（2022；[文献](https://doi.org/10.1101/2022.07.28.501447)）
- [ ] [MapView](https://web.archive.org/web/20201023093840/https://sites.google.com/site/wjwdavymapview/mapview/MapView.zip?attredirects=0)（2009；[文献](https://doi.org/10.1093/bioinformatics/btp255)）
- [ ] [NCBI Genome Data Viewer](https://www.ncbi.nlm.nih.gov/genome/gdv/)（2020；[文献](https://doi.org/10.1101/gr.266932.120)）
- [ ] [Nucleome browser](https://vis.nucleome.org/v1/main.html)（2022；[文献](https://doi.org/10.1101/2022.02.21.481225)）
- [ ] [Pileup.js](https://github.com/hammerlab/pileup.js)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw167)）
- [ ] [Short read assembly browser](https://rapdb.dna.affrc.go.jp/AssemblyBrowser2/index.html?ref=%22chr01%22&run_id=%22G4toI1%22&start=%22234780%22&end=%22236631%22)（2013；[文献](https://doi.org/https://doi.org/10.1093/pcp/pcs183)）
- [ ] [SWAV](http://swav.popgenetics.net/)（2020；[文献](https://doi.org/10.1038/s41598-019-57038-x)）
- [ ] [Tablet](https://ics.hutton.ac.uk/tablet/)（2012；[文献](https://doi.org/10.1093/bib/bbs012)）
- [ ] [theBIGbam](https://github.com/bhagavadgitadu22/theBIGbam)（2026；[文献](https://doi.org/10.1093/bioinformatics/btag459)）
- [ ] [Trackster](https://galaxyproject.org/learn/visualization/)（2012；[文献](https://doi.org/10.1038/nbt.2404)）
- [ ] [UCSC genome browser](https://genome.ucsc.edu/)（2002；[文献](https://doi.org/10.1101/gr.229102)）
- [ ] [UTGB (University of Tokyo Genome Browser)](http://utgenome.org/)（2009；[文献](https://doi.org/10.1093/bioinformatics/btp350)）
- [ ] [Valis browser](https://valis.bio/)
- [ ] [VISTA browser](http://pipeline.lbl.gov/cgi-bin/gateway2)（2004；[文献](https://doi.org/10.1093/nar/gkh458)）
- [ ] [WashU epigenomics browser](https://epigenomegateway.wustl.edu/)（2019；[文献](https://doi.org/10.1093/nar/gkz348)）
- [ ] [Zenbu](http://fantom.gsc.riken.jp/zenbu/)（2014；[文献](https://doi.org/10.1038/nbt.2840)）

#### 4.2 桌面 / Web GUI 应用（gui，92 条）

桌面/Web GUI 应用（Jalview、Artemis、Phandango、CoGe、GIVE、GenomeSpy 等）。输出依赖交互状态，不能确定性复现；其比对查看、点阵图、共线性浏览等图型已分别由第二、三节覆盖。

- [ ] [AA (alignment annotator)](http://www.bioinformatics.org/strap/aa)（2014；[文献](https://doi.org/10.1093/nar/gku400)）
- [ ] [abrowse (MSA viewer)](https://github.com/ihh/abrowse)
- [ ] [alignment.js](https://github.com/veg/alignment.js)
- [ ] [AlignmentComparator](http://bioinfweb.info/AlignmentComparator/)
- [ ] [AlignmentViewer](https://alignmentviewer.org/)（2020；[文献](https://doi.org/10.12688/f1000research.22242.2)）
- [ ] [AliView](http://www.ormbunkar.se/aliview/)（2014；[文献](https://doi.org/10.1093/bioinformatics/btu531)）
- [ ] [ALVIS (chimeric alignment viewer)](https://github.com/SR-Martin/alvis)（2021；[文献](https://doi.org/10.1186/s12859-021-04056-0)）
- [ ] [ALVIS (MSA viewer)](https://www.ebi.ac.uk/research/goldman/software/alvis)（2016；[文献](https://doi.org/10.1093/nar/gkw022)）
- [ ] [Apollo](https://web.archive.org/web/20060927154956/http://www.fruitfly.org/annot/apollo/)（2002；[文献](https://doi.org/10.1186/gb-2002-3-12-research0082)）
- [ ] [Apollo aka WebApollo](https://genomearchitect.readthedocs.io/en/latest/)（2013；[文献](https://doi.org/10.1186/gb-2013-14-8-r93)）
- [ ] [Aquaria](http://aquaria.ws/Q9HD67/5i0i/A)（2015；[文献](https://doi.org/10.1038/nmeth.3258)）
- [ ] [Artemis comparison tool (ACT)](https://www.sanger.ac.uk/science/tools/artemis-comparison-tool-act)（2005；[文献](https://doi.org/10.1093/bioinformatics/bti553)）
- [ ] [BioJS MSA](https://msa.biojs.net/)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw474)）
- [ ] [Chip Monk](http://www.bioinformatics.babraham.ac.uk/projects/chipmonk/)（2007；[文献](https://doi.org/10.1186/1752-0509-1-S1-P80)）
- [ ] [Chipster](https://chipster.csc.fi/)（2011；[文献](https://doi.org/10.1186/1471-2164-12-507)）
- [ ] [Chromatic](https://chromatic.nci.nih.gov/)（2018；[文献](https://doi.org/10.1177/1176935118771972)）
- [ ] [clinker](https://github.com/gamcil/clinker)（2021；[文献](https://doi.org/10.1093/bioinformatics/btab007)）
- [ ] [CoGe](https://genomevolution.org/coge/)（2017；[文献](https://doi.org/10.1093/bioinformatics/btx144)）
- [ ] [CView](https://sourceforge.net/projects/cview/)（2022；[文献](https://doi.org/10.1371/journal.pone.0259726)）
- [ ] [Cylindrical alignment app](https://sourceforge.net/projects/cylindrical-alignment-app/)
- [ ] [D3GB](http://d3gb.usal.es/)（2017；[文献](https://doi.org/10.1089/cmb.2016.0213)）
- [ ] [Dot](https://dot.sandbox.bio/)
- [ ] [Dotlet](https://dotlet.vital-it.ch/)（2000；[文献](https://doi.org/10.1093/bioinformatics/16.2.178)）
- [ ] [Epilogos](https://epilogos.altius.org/)
- [ ] [Gambit](http://labsergen.langebio.cinvestav.mx/bioinformatics/jacob/?p=473)
- [ ] [GBrowse_syn](http://gmod.org/wiki/GBrowse_syn)（2010；[文献](https://doi.org/10.1002/0471250953.bi0912s31)）
- [ ] [GeCoViz](https://gecoviz.cgmlab.org/)（2022；[文献](https://doi.org/10.1093/nar/gkac367)）
- [ ] [Genome Context Viewer](https://www.legumefederation.org/gcv/phytozome_10_2/)（2017；[文献](https://doi.org/10.1093/bioinformatics/btx757)）
- [ ] [GenomeMatcher](http://www.ige.tohoku.ac.jp/joho/gmProject/gmmanual.html)（2008；[文献](https://doi.org/10.1186/1471-2105-9-376)）
- [ ] [GenomePaint](https://genomepaint.stjude.cloud/)（2021；[文献](https://doi.org/10.1016/j.ccell.2020.12.011)）
- [ ] [GenomeSpy](https://genomespy.app/)（2024；[文献](https://doi.org/10.1093/gigascience/giae040)）
- [ ] [GenomeView (java app)](https://genomeview.org/content/quick-start-guide)（2011；[文献](https://doi.org/10.1093/nar/gkr995)）
- [ ] [Genomicus](https://www.genomicus.biologie.ens.fr/genomicus/)（2021；[文献](https://doi.org/10.1093/nar/gkab1091)）
- [ ] [Genovar](http://genovar.sourceforge.net/)（2012；[文献](https://doi.org/10.1186/1471-2105-13-S7-S12)）
- [ ] [Gepard](http://cube.univie.ac.at/gepard)（2007；[文献](https://doi.org/10.1093/bioinformatics/btm039)）
- [ ] [gingko](https://github.com/robertaboukhalil/ginkgo)（2015；[文献](https://doi.org/10.1038/nmeth.3578)）
- [ ] [GIVE](https://zhong-lab-ucsd.github.io/GIVE_homepage/)（2018；[文献](https://doi.org/10.1186/s13059-018-1465-6)）
- [ ] [Gmaj](https://www.bx.psu.edu/miller_lab/dist/CHAP/docs/gmaj_geneconv.html)（2011；[文献](https://doi.org/10.1186/1471-2148-11-226)）
- [ ] [Gosling](https://gosling.js.org/)（2022；[文献](https://doi.org/10.1109/TVCG.2021.3114876)）
- [ ] [gw](https://github.com/kcleal/gw)（2025；[文献](https://doi.org/10.1038/s41592-025-02749-5)）
- [ ] [Hawkeye](https://github.com/peterhj/amos)（2007；[文献](https://doi.org/10.1186/gb-2007-8-3-r34)）
- [ ] [HiPiler](http://hipiler.higlass.io)（2018；[文献](https://doi.org/10.1109/TVCG.2017.2745978)）
- [ ] [iGenomics](https://github.com/stuckinaboot/iGenomics)（2020；[文献](https://doi.org/10.1093/gigascience/giaa138)）
- [ ] [iLambda Dotplot](https://github.com/iLambda/dotplot)
- [ ] [InGAP-SV](http://ingap.sourceforge.net/)（2011；[文献](https://doi.org/10.1093/nar/gkr506)）
- [ ] [Jalview](https://www.jalview.org/)（2009；[文献](https://doi.org/10.1093/bioinformatics/btp033)）
- [ ] [JAX Synteny browser](https://github.com/TheJacksonLaboratory/syntenybrowser)（2019；[文献](https://doi.org/10.1007/s00335-019-09821-4)）
- [ ] [Jdot](https://github.com/LyonsLab/jdot)
- [ ] [JGI IMG](https://img.jgi.doe.gov/)
- [ ] [JSAV](http://www.bioinf.org.uk/software/jsav)（2014；[文献](https://doi.org/10.12688/f1000research.5486.1)）
- [ ] [Juicebox.js](http://www.igv.org/doc/juiceboxjs.html)（2018；[文献](https://doi.org/10.1016/j.cels.2018.01.001)）
- [ ] [LookSeq](https://www.sanger.ac.uk/tool/lookseq/)（2009；[文献](https://doi.org/10.1101/gr.093443.109)）
- [ ] [MagicViewer](http://bioinformatics.zj.cn/magicviewer/)（2010；[文献](https://doi.org/10.1093/nar/gkq302)）
- [ ] [MCGV](https://www.ncbi.nlm.nih.gov/mcgv/cm/mcgv/help)
- [ ] [MEGA-X](https://www.megasoftware.net/)（2021；[文献](https://doi.org/10.1093/molbev/msab120)）
- [ ] [MSABrowser](https://thekaplanlab.github.io/)（2021；[文献](https://doi.org/10.1093/bioadv/vbab009)）
- [ ] [Multiple genome viewer](http://www.informatics.jax.org/mgv/)（2021；[文献](https://doi.org/10.1007/s00335-021-09904-1)）
- [ ] [MView](https://www.ebi.ac.uk/Tools/msa/mview/)（1998；[文献](https://doi.org/10.1093/bioinformatics/14.4.380)）
- [ ] [NCBI CGV](https://ncbiinsights.ncbi.nlm.nih.gov/2022/07/05/cgv-beta-release/)
- [ ] [NCBI MSA Viewer](https://www.ncbi.nlm.nih.gov/projects/msaviewer/)
- [ ] [NGB](https://github.com/epam/NGB)
- [ ] [OMA local synteny browser](https://omabrowser.org/oma/synteny/ADH1A_HUMAN/)（2020；[文献](https://doi.org/10.1093/nar/gkaa1007)）
- [ ] [Pan-Tetris](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4547177/)（2015；[文献](https://doi.org/10.1186/1471-2105-16-S11-S3)）
- [ ] [Panache](https://github.com/SouthGreenPlatform/panache)（2021；[文献](https://doi.org/10.1093/bioinformatics/btab688)）
- [ ] [Peax](https://github.com/Novartis/peax)（2020；[文献](https://doi.org/10.1111/cgf.13971)）
- [ ] [PGAP-X](https://pgapx.zhaopage.com/)（2018；[文献](https://doi.org/10.1186/s12864-017-4337-7)）
- [ ] [PGV (pangenome tool)](https://github.com/ucrbioinfo/PGV)（2021；[文献](https://doi.org/10.1186/s12859-021-04424-w)）
- [ ] [Phandango](https://jameshadfield.github.io/phandango/#/examples)（2017；[文献](https://doi.org/10.1093/bioinformatics/btx610)）
- [ ] [PretextView](https://github.com/wtsi-hpag/PretextView)
- [ ] [ProViz](http://slim.icr.ac.uk/proviz/index.php)（2016；[文献](https://doi.org/10.1093/nar/gkw265)）
- [ ] [PureScript genetics browser](https://github.com/chfi/purescript-genetics-browser)
- [ ] [Redotable](https://github.com/s-andrews/redotable)
- [ ] [RegulomeExplorer](http://explorer.cancerregulome.org/all_pairs/?dataset=TCGA_ACC)
- [ ] [RPAN (3kricedb)](http://cgm.sjtu.edu.cn/3kricedb/visualization/?tracks=DNA%2Cgene%2CPF)（2016；[文献](https://doi.org/10.1093/nar/gkw958)）
- [ ] [Seaview](http://doua.prabi.fr/software/seaview)（2009；[文献](https://doi.org/10.1093/molbev/msp259)）
- [ ] [SeqCover](https://brentp.github.io/seqcover/#gene=AIFM1)
- [ ] [SEQing](https://github.com/malewins/SEQing)（2020；[文献](https://doi.org/10.1186/s12859-020-3434-9)）
- [ ] [seqotron](https://github.com/4ment/seqotron)（2016；[文献](https://doi.org/10.1186/s13104-016-1927-4)）
- [ ] [SFARI](https://gene.sfari.org/database/human-gene/)（2010；[文献](https://doi.org/10.1242/dmm.005439)）
- [ ] [SnoopCGH](http://snoopcgh.sourceforge.net/)（2009；[文献](https://doi.org/10.1093/bioinformatics/btp488)）
- [ ] [SplitThreader](http://splitthreader.com/)（2016；[文献](https://doi.org/10.1101/087981)）
- [ ] [STRAP](http://www.bioinformatics.org/strap/)（2014；[文献](https://doi.org/10.1093/nar/gku400)）
- [ ] [TASUKE](https://tasuke-wiki.dna.naro.go.jp/)（2019；[文献](https://doi.org/10.1093/dnares/dsz022)）
- [ ] [Transposcope](https://github.com/FenyoLab/transposcope)（2020；[文献](https://doi.org/10.1093/bioinformatics/btaa244)）
- [ ] [UCSC Xena](https://xena.ucsc.edu/)（2020；[文献](https://doi.org/10.1038/s41587-020-0546-8)）
- [ ] [udon](https://github.com/ocxtal/udon)
- [ ] [UGENE](http://ugene.net/)（2012；[文献](https://doi.org/10.1093/bioinformatics/bts091)）
- [ ] [Varsome](https://varsome.com/security-validation/?next=/variant/hg19/NM_000088.3(COL1A1):c.658C%3ET)（2018；[文献](https://doi.org/10.1093/bioinformatics/bty897)）
- [ ] [vitessce](https://vitessce.io/)（2024；[文献](https://doi.org/10.1038/s41592-024-02436-x)）
- [ ] [wally](https://github.com/tobiasrausch/wally)
- [ ] [waragraph](https://chfi.github.io/waragraph/)
- [ ] [wasm bigwig demo browser](https://shk656461.github.io/index.html)

#### 4.3 终端 / ASCII 工具（cli，25 条）

终端/ASCII 工具（`samtools tview`/`depth`、ASCIIGenome、SeqTUI 等）。字符画不适合出版级排版，但其"输出图型"（覆盖度、比对堆叠）已由 `Omics/BlastAlignmentPlot.qmd` 等覆盖。

- [ ] [Alan](https://github.com/mpdunne/alan)
- [ ] [alen](https://github.com/jakobnissen/alen)
- [ ] [Alv](https://github.com/arvestad/alv)（2018；[文献](https://doi.org/10.21105/joss.00955)）
- [ ] [ASCIIGenome](https://github.com/dariober/ASCIIGenome)（2017；[文献](https://doi.org/10.1093/bioinformatics/btx007)）
- [ ] [bacircos](https://github.com/sablokg/bacircos)（2026；[文献](https://doi.org/10.20944/preprints202609.2579.v1)）
- [ ] [dep](https://github.com/slw287r/dep)
- [ ] [Dotter](https://sonnhammer.sbc.su.se/Dotter.html)（1995；[文献](https://doi.org/10.1016/0378-1119(95)00714-8)）
- [ ] [Dottup](https://www.bioinformatics.nl/cgi-bin/emboss/help/dottup)
- [ ] [GenomeTools](http://genometools.org/)（2013；[文献](https://doi.org/10.1109/TCBB.2013.68)）
- [ ] [MashMap](https://github.com/marbl/MashMap/blob/master/scripts/generateDotPlot)（2018；[文献](https://doi.org/10.1093/bioinformatics/bty597)）
- [ ] [Panacaea](https://github.com/JCVenterInstitute/PanACEA)（2018；[文献](https://doi.org/10.1186/s12859-018-2250-y)）
- [ ] [pileuppy](https://gitlab.com/tprodanov/pileuppy)
- [ ] [QUAAK](https://github.com/YingZhou001/Quaak)
- [ ] [rich-msa](https://github.com/althonos/rich-msa)
- [ ] [Rna Alignment Viewers (Colorstock, SScolor, Ratón)](http://biowiki.org/wiki/index.php?title=Rna_Alignment_Viewers&redirect=no)（2008；[文献](https://doi.org/10.1093/bioinformatics/btm635)）
- [ ] [salti](https://github.com/Sam-Sims/salti)
- [ ] [Samtools coverage](http://www.htslib.org/)（2009；[文献](https://doi.org/10.1093/bioinformatics/btp352)）
- [ ] [Samtools depth visualization](https://twitter.com/yokofakun/status/1178686978541441025)
- [ ] [Samtools tview](http://www.htslib.org/)（2009；[文献](https://doi.org/10.1093/bioinformatics/btp352)）
- [ ] [SeqSizzle](https://github.com/ChangqingW/SeqSizzle)
- [ ] [SeqTUI](https://github.com/ranwez-search/SeqTUI)
- [ ] [SvABA](https://github.com/walaj/svaba#alignmentstxtgz)（2018；[文献](https://doi.org/10.1101/gr.221028.117)）
- [ ] [termal](https://github.com/sib-swiss/termal)（2025；[文献](https://doi.org/10.1093/bioadv/vbaf208)）
- [ ] [tgv (terminal genome viewer)](https://github.com/zeqianli/tgv)
- [ ] [wgatools](https://github.com/wjwei-handsome/wgatools)（2025；[文献](https://doi.org/10.1093/bioinformatics/btaf132)）

#### 4.4 商业软件（commercial，16 条）

商业软件（Geneious、Benchling、SnapGene、DNASTAR 等）。无开源实现可复用；对应图型（质粒图、比对图）已在第三节以开源方式给出重实现路径。

- [ ] [10x genomics - Loupe](https://support.10xgenomics.com/genome-exome/software/visualization/latest/structural-variants)
- [ ] [Alamut](https://www.interactive-biosoftware.com/alamut-visual/)
- [ ] [ATCC Genome Portal](https://docs.onecodex.com/en/articles/3996697-using-the-genome-browser)
- [ ] [Benchling](https://www.benchling.com/)
- [ ] [Bionano](https://bionanogenomics.com/technology/structural-variation/)
- [ ] [Circa](https://omgenomics.com/circa)
- [ ] [CLC Genomics workbench](https://digitalinsights.qiagen.com/news/blog/discovery/structural-variant-detection-using-clc-genomics-workbench/)
- [ ] [DNASTAR](https://www.dnastar.com/)
- [ ] [ERGO](https://www.igenbio.com/ergo)（2003；[文献](https://doi.org/10.1093/nar/gkg148)）
- [ ] [Geneious](https://www.geneious.com/)
- [ ] [Genestack](https://genestack.com/blog/2015/05/28/navigation-in-genestack-genome-browser/)
- [ ] [Golden Helix](https://www.goldenhelix.com/products/GenomeBrowse/)
- [ ] [Lucid viewer](https://lucidalign.com/)
- [ ] [MacVector](https://macvector.com/)
- [ ] [Persephone](https://persephonesoft.com/)
- [ ] [Strand NGS](https://www.strand-ngs.com/)

#### 4.5 已失效 / 不可用项目（dead，26 条）

已失效/不可用项目（`status:Deadlink`/`Historical`/`Unavailable`）。图型仍有参考价值，按需在相关 qmd 中吸收，不单独立项。

- [ ] [Aequatus](https://github.com/TGAC/Aequatus)（2018；[文献](https://doi.org/10.1093/gigascience/giy128)）
- [ ] [AutoGraph](http://autograph.genouest.org/)（2006；[文献](https://doi.org/10.1093/bioinformatics/btl618)）
- [ ] [BactoGENIE](https://doi.org/10.1186/1471-2105-16-S11-S6)（2015；[文献](https://doi.org/10.1186/1471-2105-16-S11-S6)）
- [ ] [CGH-Explorer](https://doi.org/10.1093/bioinformatics/bti113)（2004；[文献](https://doi.org/10.1093/bioinformatics/bti113)）
- [ ] [cnvCurator](http://www.acsu.buffalo.edu/~lm69/cnvCurator)（2015；[文献](https://doi.org/10.1186/s12859-015-0766-y)）
- [ ] [Combo](https://doi.org/10.1093/bioinformatics/btl193)（2006；[文献](https://doi.org/10.1093/bioinformatics/btl193)）
- [ ] [DNAVis](https://doi.org/10.1093/bioinformatics/bti807)（2005；[文献](https://doi.org/10.1093/bioinformatics/bti807)）
- [ ] [Dotplot (chirimoyo)](https://chirimoyo.ac.uma.es/bitlab/portfolio/dotplot/)
- [ ] [EvolutionHighway](http://eh-demo.ncsa.illinois.edu/)
- [ ] [Flash Gviewer](http://gmod.org/wiki/Flashgviewer/)
- [ ] [GECO](http://bioinfo.mikrobio.med.uni-giessen.de/geco2/GecoMainServlet)（2006；[文献](https://doi.org/10.1093/bioinformatics/btl556)）
- [ ] [gff2ps](https://genome.crg.es/software/gfftools/GFF2PS.html)（2000；[文献](https://doi.org/10.1093/bioinformatics/16.8.743)）
- [ ] [Gremlin](http://compbio.cs.brown.edu/projects/gremlin/)（2010；[文献](https://doi.org/10.1109/tvcg.2010.163)）
- [ ] [Internet Contig Explorer](https://www.bcgsc.ca/resources/software/ice)（2003；[文献](https://doi.org/10.1101/gr.819303)）
- [ ] [mGSV (multiple genome synteny viewer)](https://github.com/kashmatic/mGSV)（2012；[文献](https://doi.org/10.1186/1471-2105-13-190)）
- [ ] [MIRA](http://mira-assembler.sourceforge.net/docs/DefinitiveGuideToMIRA.html)（2004；[文献](https://doi.org/10.1101/gr.1917404)）
- [ ] [Orchestral](https://doi.org/10.1109/BioVis.2013.6664351)（2013；[文献](https://doi.org/10.1109/BioVis.2013.6664351)）
- [ ] [Parasight](https://baileylab.brown.edu/parasight/galframe.html)（2005；[文献](https://doi.org/10.1038/ng1562)）
- [ ] [PSAT](http://www.nwrce.org/psat)（2008；[文献](https://doi.org/10.1186/1471-2105-9-170)）
- [ ] [r2cat](https://github.com/phuseman/r2cat)（2009；[文献](https://doi.org/10.1093/bioinformatics/btp690)）
- [ ] [React MSAViewer](https://github.com/plotly/react-msa-viewer)
- [ ] [SMRT View](http://files.pacb.com/software/smrtanalysis/2.3.0/doc/smrtview/help/Webhelp/App_View_Epipro.htm)
- [ ] [SNIPViz](http://www.yeastrc.org/snipviz/4.HTML_Config_retrieve_newick_and_fasta_from_server/snip_viz_HTML_config_with_newick_clustering.html)（2014；[文献](https://doi.org/10.1186/1756-0500-7-468)）
- [ ] [SNPduo](https://web.archive.org/web/20210518090714/http://pevsnerlab.kennedykrieger.org/SNPduo/)（2009；[文献](https://doi.org/10.1371/journal.pone.0006711)）
- [ ] [sockeye](https://www.bcgsc.ca/resources/software/sockeye)（2004；[文献](https://doi.org/10.1101/gr.1890304)）
- [ ] [VAMP](https://doi.org/10.1093/bioinformatics/btl359)（2006；[文献](https://doi.org/10.1093/bioinformatics/btl359)）

#### 4.6 分析流程（非绘图）（analysis，6 条）

分析流程而非绘图（ChIA-Pipe、FastANI、ChopChop、panX 等）。属"分析"范畴，归入后续专题。

- [ ] [ChIA-Pipe](https://github.com/TheJacksonLaboratory/ChIA-PIPE)（2020；[文献](https://doi.org/10.1126/sciadv.aay2078)）
- [ ] [Chopchop](http://chopchop.cbu.uib.no/)（2019；[文献](https://doi.org/10.1093/nar/gkz365)）
- [ ] [FastANI](https://github.com/ParBLiSS/FastANI)（2018；[文献](https://doi.org/10.1038/s41467-018-07641-9)）
- [ ] [Genome Puzzle Master (GPM)](https://jianwei-zhang.github.io/LIMS/GPM-Manual.html)（2016；[文献](https://doi.org/10.1093/bioinformatics/btw370)）
- [ ] [MosaicCatcher pipeline](https://github.com/friendsofstrandseq/mosaicatcher-pipeline)
- [ ] [panX](http://pangenome.tuebingen.mpg.de/)（2017；[文献](https://doi.org/10.1093/nar/gkx977)）

### 五、❓ 需调研（10 条）

以下条目信息不足以判定"图型能否静态重画"，暂不计入实施面；核实后并入第二/三/四节。

- [ ] [ab1lens](https://fishka.bio/ab1lens)：网页版 `.ab1` 测序峰图查看器（data:Chromatogram）；与拟新增的 `Omics/ChromatogramPlot.qmd` 直接相关，待确认是否有可复现的静态出图方式
- [ ] [MizBee](http://www.cs.utah.edu/~miriah/mizbee/Overview.html)（2009；[文献](https://doi.org/10.1109/TVCG.2009.167)）：多尺度共线性浏览（2009，Processing）；图型经典但实现语言已过时，待评估重画收益
- [ ] [Panagram](https://github.com/kjenike/panagram)：泛基因组 k-mer 图（Python）；待确认包发布与图型
- [ ] [PG2 (PanGenoGrapher)](https://github.com/iVis-at-Bilkent/pangenographer)（2026；[文献](https://doi.org/10.1186/s12859-026-06555-4)）：泛基因组图 Web 布局工具（2026）；待确认导出格式与是否可静态重画
- [ ] [PGR-TK](https://github.com/cschin/pgr-tk)（2023；[文献](https://doi.org/10.1038/s41592-023-01914-y)）：泛基因组图（Rust）；待确认其"图"的形态与可否用 R/Python 重画
- [ ] [SaVanache](https://savanache.ird.fr)（2026；[文献](https://doi.org/10.64898/2026.05.05.722901)）：结构变异/共线性 Web 可视化（2026，AGV 已标记 Deadlink）；待确认可达性与图型
- [ ] [SQANTI-browser](https://github.com/ConesaLab/SQANTI-browser)（2026；[文献](https://doi.org/10.64898/2026.05.25.727625)）：长读长转录本浏览器（依赖 UCSC 生态）；待确认是否只有交互形态
- [ ] [SynFlow](https://github.com/SouthGreenPlatform/SynFlow)（2026；[文献](https://doi.org/10.1093/bioinformatics/btag567)）：SV + 共线性 + JBrowse 集成；过滤掉 JBrowse 依赖后是否仍有独立的 SV 图型待确认
- [ ] [VIJB](https://github.com/NiBuMNHN/VIJB)（2026；[文献](https://doi.org/10.1093/bioinformatics/btag396)）：JBrowse 可访问性（accessibility）插件；待确认是否产生静态图
- [ ] [winpca](https://github.com/MoritzBlumer/winpca)（2025；[文献](https://doi.org/10.48550/arXiv.2501.11982)）：局部祖源 PCA（Python，arXiv 2025）；待确认包发布情况与图型

### 六、实施约定与依赖风险

#### 6.1 依赖可用性（核对日期 2026-11-07）

Bizard 文档采用"文档内按需安装"模式（`if (!requireNamespace(...)) install.packages(...)`），`renv.lock` 是工作流生成的锁定快照（当前 708 包，R 4.6.2）。下列状态以此为依据逐包核对：

| 状态 | 包 |
|---|---|
| ✅ 已在 `renv.lock` 锁定 | `circlize`、`RIdeogram`、`qqman`、`ComplexHeatmap`、`gggenes`、`ggtree`、`ggmsa`、`ggseqlogo`、`aplot`、`syntR`（GitHub 源） |
| ✅ CRAN 可直接装 | `CMPlot`、`chromoMap`、`gggenomes`、`locuszoomr`、`syntenyPlotteR`、`genoPlotR`、`vcfR`、`topr`、`msaR` |
| ✅ Bioconductor 可直接装 | `Gviz`、`ggbio`、`gtrellis`、`trackViewer`、`OmicCircos`、`karyoploteR`、`plotgardener`、`NanoMethViz`、`chimeraviz`、`drawProteins`、`sangerseqR`、`chromPlot`、`wiggleplotr`、`EnrichedHeatmap`、`genomation`、`GenVisR`、`tracktables` |
| ✅ PyPI 可直接装 | `pyGenomeTracks`、`figeno`、`pyGenomeViz`、`pyCirclize`、`ModDotPlot`、`TRviz`、`methylartist`、`methplotlib`、`modbamtools`、`CoolBox`、`dna_features_viewer`、`cnvkit`、`samplot`、`trackplot`、`rmats2sashimiplot`、`gbdraw`、`squigualiser`、`tagore`、`ete3` |
| ⚠️ 仅 GitHub 源（需 `Remotes:` 或 pip from git） | `plasmapR`、`ggbandage`、`ggtubemap`、`pafr`（CRAN 已归档，用 GitHub 源）、`ReConPlot`、`SVbyEye`、`seqvisr`、`rMSA`、`ggtranscript` |
| ❌ 已归档 / 已移除（不可直接装） | `ggcoverage`（CRAN 2023-11-16 归档）、`LDheatmap`（CRAN 2023-01-22 归档）、`Sushi`（Bioconductor 3.16 起移除）、`Millefy`、`GenomicBreaks`、`plotsr`、`ggsashimi`（无 PyPI 包） |

> 依赖策略：凡标 `[包]` 的条目优先用现成包；凡标 `[重实现]` 的条目**不要**为了省事去装已归档包（如 `pafr`），直接用 ggplot2/matplotlib 按图型重画，并把"图型要素"写进教程说明。

#### 6.2 图型科学准确性红线（重实现时必须遵守）

1. **Sashimi 图**：正负链必须分 `-1/+1` 两层画弧，否则会掩盖链特异剪接；纵轴须标明是 read count 还是剪接比率（PSI）。
2. **连锁不平衡图**：D′ 与 r² 的取值区间与含义不同，**不可共用同一色标**；必须标注所用统计量。
3. **CNV 图**：分段线（segmented）与原始深度点必须同现，只画分段会被误读为精确断点；须标注纯度/倍体校正方式。
4. **共线性/同源块图**：必须编码方向（同向/倒位用颜色或箭头区分），否则会掩盖倒位与易位。
5. **环形（Circos）图**：扇区尺寸须与基因组长度等比，扇区起点与中心角对齐规则需在文档中写明。
6. **"树 + 下游矩阵"联合图**：叶序必须与树一致（顺序不一致是最常见的科学错误）。
7. **甲基化/修饰碱基轨道**：必须标注最小覆盖度阈值，并说明被过滤的位点数量。
8. **基因组坐标轴**：必须标明参考基因组版本（如 GRCh38 / T2T-CHM13），跨物种图须说明坐标是否为线性映射。
9. **点阵图（dotplot）**：须说明是全长比对还是局部比对、所用相似度阈值与最小块长。
10. **多基因组比较**：同源块会压缩/扩张坐标轴，须声明坐标是否已归一化，避免读者按绝对长度比较。

#### 6.3 建议实施批次（按依赖确定性排序）

- **第 1 批｜零新增重依赖**（只用 `renv.lock` 已有包）：第二节的 `ChromosomePlot`、`GeneStructurePlot`、`MultiSeqsAlignment`、`MotifPlot`、`ManhattanPlot`、`ScCnvPlot`、`Lollipop`、`SyntenyBlocksPlot`。
- **第 2 批｜依赖可从 CRAN/Bioconductor 自装**：`GwasSnpPlot`、`Python/CircosPlot`、`CollinearityPlot`、`Hiplot/087-hic-heatmap`，以及新增 `StructuralVariantPlot`、`GenomeGraphPlot`、`BlastAlignmentPlot`、`MultiGenomeComparisonPlot`、`MethylationTrack`、`RepeatStructurePlot`、`WholeGenomeAlignmentPlot`、`ProteinDomainPlot`。
- **第 3 批｜依赖需重实现或仅 GitHub/PyPI**：`PlasmidMap`、`GeneFusionPlot`、`SashimiPlot`、`VariantAnnotationPlot`、`GeneticMapPlot`、`LocalAncestryPlot`、`PhylogenyGenomePlot`、`ChromatogramPlot`、`LinkageDisequilibriumPlot`、`Python/GenomeTrackPlot`、`Python/GenomeComparisonCircos`、`Python/MicrobiomeGenomePlot`。

#### 6.4 口径与方法声明

- 数据源：`awesome-genome-visualization` 的 `TOOLS.json`（574 条，快照 2026-11-07）；每条**一对一**归属某一节，已脚本校验：无遗漏、无重复，574 = 186 + 172 + 206 + 10。
- 包可用性核对日期 2026-11-07（CRAN / Bioconductor 3.22 / PyPI 索引）；URL 与 DOI 均取自 `TOOLS.json` 原始字段，未作推测补全。
- 未被核实的条目一律归入第四/五节，不进入实施清单；本清单不追求"全部收录"，只保证"可静态复现"。`[重实现]` 若在实施时发现可用包，直接改为 `[包]` 并记录版本即可，不影响清单结构。

---

进度将通过勾选本清单条目跟踪。如需先做某一批，直接在本帖回复批次号即可。
