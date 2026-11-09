# Awesome Genome Visualization integration audit (#435)

Snapshot: upstream `cmdcolin/awesome-genome-visualization` `TOOLS.json` (574 entries, no duplicates), cross-checked on 2026-11-07 with the GitHub REST API for last-push date and archive status.

Every upstream entry is recorded exactly once. Entries are never dropped silently: each one ends in either an integrated page or an explicit, reasoned skip. Two skip rules come straight from the issue brief:

- **>5 years without maintenance** — last `pushed_at` strictly earlier than `2021-11-07` (the snapshot date, 2026-11-07, minus five years) or repository archived → `stale-skip`.
- **Dependency too heavy** — framework-scale dependency closures (ArchR, signac, Gviz, ggbio, pyGenomeTracks, CNVkit, SVbyEye, …) → `heavy-skip`, redrawn with ggplot2 / matplotlib instead of installed.

## Decisions

| Decision | Entries | Meaning |
|---|---:|---|
| `package` | 68 | `[包]` 现成实现（作为设计参考，不新增依赖） |
| `reimplemented` | 220 | 本页用 ggplot2 / matplotlib 重画 |
| `stale-skip` | 39 | ⏭️ 跳过：>5 年无运维更新 |
| `heavy-skip` | 27 | ⏭️ 跳过：依赖过重 |
| `nonstatic-skip` | 2 | ⏭️ 跳过：无法静态复现 |
| `nonstatic` | 158 | 暂缓：交互式浏览器 / GUI / 终端 |
| `commercial` | 18 | 暂缓：商业软件 |
| `dead` | 26 | 暂缓：已失效 |
| `analysis` | 6 | 暂缓：分析流程（不产出图形） |
| `research` | 10 | 待调研：信息不足 |
| **total** | **574** | |

356 of 574 entries map onto a Bizard page; 68 are explicitly skipped on the two issue criteria plus the static-render criterion, and 218 stay out of the implementation surface as interactive/browser, GUI, terminal, commercial, dead, non-drawing or under-specified.

## Per-page coverage

Each row lists how many upstream entries target that page and how they were handled. The `Skipped` column counts the explicit skips.

| Page | Entries | Integrated | Skipped | Skip detail |
|---|---:|---:|---:|---|
| `Hiplot/087-hic-heatmap.qmd` | 2 | 0 | 2 | heavy-skip 1, stale-skip 1 |
| `Omics/BlastAlignmentPlot.qmd` | 15 | 11 | 4 | stale-skip 4 |
| `Omics/ChromatogramPlot.qmd` | 2 | 2 | 0 | — |
| `Omics/ChromosomePlot.qmd` | 14 | 10 | 4 | heavy-skip 1, stale-skip 3 |
| `Omics/CollinearityPlot.qmd` | 29 | 20 | 9 | heavy-skip 3, stale-skip 6 |
| `Omics/GeneFusionPlot.qmd` | 8 | 7 | 1 | heavy-skip 1 |
| `Omics/GeneStructurePlot.qmd` | 25 | 20 | 5 | heavy-skip 4, stale-skip 1 |
| `Omics/GeneticMapPlot.qmd` | 6 | 6 | 0 | — |
| `Omics/GenomeGraphPlot.qmd` | 25 | 22 | 3 | nonstatic-skip 2, stale-skip 1 |
| `Omics/GwasSnpPlot.qmd` | 3 | 3 | 0 | — |
| `Omics/LinkageDisequilibriumPlot.qmd` | 2 | 2 | 0 | — |
| `Omics/LocalAncestryPlot.qmd` | 5 | 5 | 0 | — |
| `Omics/ManhattanPlot.qmd` | 12 | 12 | 0 | — |
| `Omics/MethylationTrack.qmd` | 10 | 10 | 0 | — |
| `Omics/MotifPlot.qmd` | 1 | 1 | 0 | — |
| `Omics/MultiGenomeComparisonPlot.qmd` | 11 | 11 | 0 | — |
| `Omics/MultiSeqsAlignment.qmd` | 16 | 15 | 1 | stale-skip 1 |
| `Omics/PhylogenyGenomePlot.qmd` | 5 | 5 | 0 | — |
| `Omics/PlasmidMap.qmd` | 8 | 8 | 0 | — |
| `Omics/ProteinDomainPlot.qmd` | 5 | 5 | 0 | — |
| `Omics/RepeatStructurePlot.qmd` | 9 | 8 | 1 | stale-skip 1 |
| `Omics/SashimiPlot.qmd` | 7 | 6 | 1 | stale-skip 1 |
| `Omics/ScCnvPlot.qmd` | 24 | 13 | 11 | heavy-skip 4, stale-skip 7 |
| `Omics/StructuralVariantPlot.qmd` | 27 | 19 | 8 | heavy-skip 1, stale-skip 7 |
| `Omics/SyntenyBlocksPlot.qmd` | 22 | 21 | 1 | stale-skip 1 |
| `Omics/VariantAnnotationPlot.qmd` | 7 | 5 | 2 | heavy-skip 2 |
| `Omics/WholeGenomeAlignmentPlot.qmd` | 8 | 6 | 2 | heavy-skip 1, stale-skip 1 |
| `Python/CircosPlot.qmd` | 29 | 23 | 6 | heavy-skip 3, stale-skip 3 |
| `Python/GenomeComparisonCircos.qmd` | 1 | 0 | 1 | heavy-skip 1 |
| `Python/GenomeTrackPlot.qmd` | 9 | 6 | 3 | heavy-skip 3 |
| `Python/MicrobiomeGenomePlot.qmd` | 2 | 1 | 1 | heavy-skip 1 |
| `Ranking/Lollipop.qmd` | 9 | 7 | 2 | heavy-skip 1, stale-skip 1 |

## Explicit skips on the maintenance / dependency criteria

Entries that were *not* implemented on purpose, with the evidence used to decide. An entry with no GitHub repository (a paper, web service or vendor product) is judged from the checklist annotation instead.

| Entry | Page | Reason | Last push | Archived |
|---|---|---|---|---|
| 10x genomics - Loupe | `` | 暂缓：商业软件 | — | — |
| ATCC Genome Portal | `` | 暂缓：商业软件 | — | — |
| Alamut | `` | 暂缓：商业软件 | — | — |
| Benchling | `` | 暂缓：商业软件 | — | — |
| Bionano | `` | 暂缓：商业软件 | — | — |
| CLC Genomics workbench | `` | 暂缓：商业软件 | — | — |
| Circa | `` | 暂缓：商业软件 | — | — |
| DNASTAR | `` | 暂缓：商业软件 | — | — |
| ERGO | `` | 暂缓：商业软件 | — | — |
| Geneious | `` | 暂缓：商业软件 | — | — |
| Genestack | `` | 暂缓：商业软件 | — | — |
| Golden Helix | `` | 暂缓：商业软件 | — | — |
| Lucid viewer | `` | 暂缓：商业软件 | — | — |
| MacVector | `` | 暂缓：商业软件 | — | — |
| Persephone | `` | 暂缓：商业软件 | — | — |
| Strand NGS | `` | 暂缓：商业软件 | — | — |
| Aequatus | `` | 暂缓：已失效 | 2025-10-26 | no |
| AutoGraph | `` | 暂缓：已失效 | — | — |
| BactoGENIE | `` | 暂缓：已失效 | — | — |
| CGH-Explorer | `` | 暂缓：已失效 | — | — |
| Combo | `` | 暂缓：已失效 | — | — |
| DNAVis | `` | 暂缓：已失效 | — | — |
| Dotplot (chirimoyo) | `` | 暂缓：已失效 | — | — |
| EvolutionHighway | `` | 暂缓：已失效 | — | — |
| Flash Gviewer | `` | 暂缓：已失效 | — | — |
| GECO | `` | 暂缓：已失效 | — | — |
| Gremlin | `` | 暂缓：已失效 | — | — |
| Internet Contig Explorer | `` | 暂缓：已失效 | — | — |
| MIRA | `` | 暂缓：已失效 | — | — |
| Orchestral | `` | 暂缓：已失效 | — | — |
| PSAT | `` | 暂缓：已失效 | — | — |
| Parasight | `` | 暂缓：已失效 | — | — |
| React MSAViewer | `` | 暂缓：已失效 | — | no |
| SMRT View | `` | 暂缓：已失效 | — | — |
| SNIPViz | `` | 暂缓：已失效 | — | — |
| SNPduo | `` | 暂缓：已失效 | — | — |
| VAMP | `` | 暂缓：已失效 | — | — |
| cnvCurator | `` | 暂缓：已失效 | — | — |
| gff2ps | `` | 暂缓：已失效 | — | — |
| mGSV (multiple genome synteny viewer) | `` | 暂缓：已失效 | 2013-04-15 | no |
| r2cat | `` | 暂缓：已失效 | — | — |
| sockeye | `` | 暂缓：已失效 | — | — |
| Juicebox | `Hiplot/087-hic-heatmap.qmd` | ⏭️ 跳过：依赖过重 | 2026-10-15 | no |
| HiCPlotter | `Hiplot/087-hic-heatmap.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2020-07-13 | no |
| GSSPlayground | `Omics/BlastAlignmentPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2020-07-12 | yes |
| Hapviz | `Omics/BlastAlignmentPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2014-03-12 | no |
| Sam2pairwise | `Omics/BlastAlignmentPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2015-01-29 | no |
| shabam | `Omics/BlastAlignmentPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2017-05-08 | no |
| ggbio | `Omics/ChromosomePlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| Ideoplot | `Omics/ChromosomePlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2015-09-21 | no |
| KaryotypeSVG | `Omics/ChromosomePlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2017-07-17 | no |
| RIdeogram | `Omics/ChromosomePlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2020-09-22 | no |
| GENESPACE | `Omics/CollinearityPlot.qmd` | ⏭️ 跳过：依赖过重 | 2025-01-24 | no |
| SyMap | `Omics/CollinearityPlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| plotsr | `Omics/CollinearityPlot.qmd` | ⏭️ 跳过：依赖过重 | 2026-07-27 | no |
| Minidot | `Omics/CollinearityPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2016-08-30 | no |
| Mummer-idotplot | `Omics/CollinearityPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2020-02-29 | no |
| local-rearrangements | `Omics/CollinearityPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2017-11-15 | no |
| oxford-plots | `Omics/CollinearityPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2019-08-11 | no |
| paf2dot | `Omics/CollinearityPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2021-05-03 | no |
| pafr | `Omics/CollinearityPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2021-04-30 | no |
| FusionInspector | `Omics/GeneFusionPlot.qmd` | ⏭️ 跳过：依赖过重 | 2026-10-12 | no |
| ArchR | `Omics/GeneStructurePlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| Gviz | `Omics/GeneStructurePlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| signac | `Omics/GeneStructurePlot.qmd` | ⏭️ 跳过：依赖过重 | 2026-09-11 | no |
| trackViewer | `Omics/GeneStructurePlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| Millefy | `Omics/GeneStructurePlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2019-12-23 | no |
| Bandage | `Omics/GenomeGraphPlot.qmd` | ⏭️ 跳过：无法静态复现 | 2026-07-07 | no |
| BandageJS | `Omics/GenomeGraphPlot.qmd` | ⏭️ 跳过：无法静态复现 | 2026-11-05 | no |
| IGGE | `Omics/GenomeGraphPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2021-04-11 | no |
| msaR | `Omics/MultiSeqsAlignment.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2021-09-22 | no |
| ApE (a plasmid editor) | `Omics/PlasmidMap.qmd` | 暂缓：商业软件 | — | — |
| SnapGene | `Omics/PlasmidMap.qmd` | 暂缓：商业软件 | — | — |
| REViewer | `Omics/RepeatStructurePlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2026-04-20 | yes |
| Spliceclust | `Omics/SashimiPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2019-12-30 | no |
| CNVkit | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| CNVpytor | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：依赖过重 | 2026-06-23 | no |
| mocha | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：依赖过重 | 2025-08-22 | no |
| numbat | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| CNSpector | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2019-06-01 | no |
| CNVPlot | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2017-04-05 | no |
| CNView | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2018-01-08 | no |
| Hagfish | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2012-08-09 | no |
| aCNVViewer | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2019-04-10 | no |
| casper | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2021-04-05 | no |
| scCNV_heatmap | `Omics/ScCnvPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2019-10-08 | no |
| SVbyEye | `Omics/StructuralVariantPlot.qmd` | ⏭️ 跳过：依赖过重 | 2026-04-13 | no |
| Delly-maze | `Omics/StructuralVariantPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2021-10-11 | no |
| Discoplot | `Omics/StructuralVariantPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2015-09-02 | no |
| SVPV | `Omics/StructuralVariantPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2017-07-19 | no |
| bigly | `Omics/StructuralVariantPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2020-10-02 | no |
| spinteny | `Omics/StructuralVariantPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2013-08-29 | no |
| svv | `Omics/StructuralVariantPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2016-11-21 | no |
| svviz | `Omics/StructuralVariantPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2018-08-18 | no |
| syntenyPlotByR | `Omics/SyntenyBlocksPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2020-05-25 | no |
| VIVA | `Omics/VariantAnnotationPlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| XVCF | `Omics/VariantAnnotationPlot.qmd` | ⏭️ 跳过：依赖过重 | 2026-04-21 | no |
| progressiveMauve viewer | `Omics/WholeGenomeAlignmentPlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| Genome-plots-processing | `Omics/WholeGenomeAlignmentPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2020-02-05 | no |
| DNAPlotLib | `Python/CircosPlot.qmd` | ⏭️ 跳过：依赖过重 | 2024-03-20 | no |
| circularMT | `Python/CircosPlot.qmd` | ⏭️ 跳过：依赖过重 | 2026-07-15 | no |
| pyCirclize | `Python/CircosPlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| Cvit.js | `Python/CircosPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2021-01-15 | no |
| GenomeProjector | `Python/CircosPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2020-09-28 | no |
| cgplot | `Python/CircosPlot.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2019-05-19 | no |
| pyGenomeViz | `Python/GenomeComparisonCircos.qmd` | ⏭️ 跳过：依赖过重 | 2026-06-14 | no |
| CoolBox | `Python/GenomeTrackPlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| figeno | `Python/GenomeTrackPlot.qmd` | ⏭️ 跳过：依赖过重 | 2025-12-12 | no |
| pyGenomeTracks | `Python/GenomeTrackPlot.qmd` | ⏭️ 跳过：依赖过重 | 2026-10-03 | no |
| Anvio | `Python/MicrobiomeGenomePlot.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| drawProteins | `Ranking/Lollipop.qmd` | ⏭️ 跳过：依赖过重 | — | — |
| pViz | `Ranking/Lollipop.qmd` | ⏭️ 跳过：>5 年无运维更新 | 2016-09-13 | no |

## Reproducing

```bash
python3 docs/awesome-genome-visualization/build_audit.py
```

The script rewrites `audit.csv` and this report from the three checked-in inputs. `repo_meta.json` is a point-in-time GitHub snapshot; refresh it with `gh api repos/<owner>/<repo>` (fields `pushed_at`, `archived`, `stargazers_count`, `license`) before re-running if the audit needs to be re-dated.
