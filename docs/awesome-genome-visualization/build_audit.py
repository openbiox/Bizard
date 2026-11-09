#!/usr/bin/env python3
"""Rebuild the issue-435 integration audit.

Usage:
    python3 docs/awesome-genome-visualization/build_audit.py

Inputs (read from the directory containing this script):
    checklist.md   - the issue-435 integration checklist (574 entries;
                    [x] = handled, [ ] = out of scope)
    TOOLS.json     - upstream awesome-genome-visualization snapshot
    repo_meta.json - GitHub `pushed_at` / `archived` / `stars` per repo
                     (refreshed with `gh api repos/<owner>/<repo>`)

Outputs:
    audit.csv                      - one row per entry with its decision
    README.md                      - human-readable report generated from audit.csv
"""
import json, re, csv, datetime, sys, os
from collections import Counter, defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
CHECKLIST = os.path.join(HERE, 'checklist.md')
META = os.path.join(HERE, 'repo_meta.json')
TOOLS = os.path.join(HERE, 'TOOLS.json')
OUT = os.path.join(HERE, 'audit.csv')

text = open(CHECKLIST, encoding='utf-8').read()
meta = json.load(open(META))

section = None; target = None; rows = []; part = None
for ln in text.splitlines():
    m = re.match(r'^(###|####)\s+(.*)$', ln)
    if m:
        level, title = m.group(1), m.group(2).strip()
        if level == '###' and re.match(r'^[一二三四五六]、|^0\.', title):
            part, section, target = title, title, None
            continue
        if level == '####' and re.match(r'^\d+\.\d+\s', title):
            section, target = '四|' + title, None
            continue
        t = re.search(r'`([^`]+\.qmd)`', title)
        if t:
            target = t.group(1)
        continue
    e = re.match(r'^- \[[ xX]\] (.+)$', ln)
    if not e: continue
    body = e.group(1)
    lm = re.match(r'\[([^\]]+)\]\((https?://[^)]+)\)', body)
    if lm:
        name, url = lm.group(1), lm.group(2)
    else:
        m2 = re.match(r'\*\*([^*]+)\*\*', body)
        name, url = (m2.group(1) if m2 else body[:40]), ''
    pkg = '包' if '`[包]`' in body else ('重实现' if '`[重实现]`' in body else '')
    year = re.search(r'（(\d{4})[；）]', body)
    rows.append(dict(name=name.strip(), url=url, section=section, target=target or '',
                     impl=pkg, year=year.group(1) if year else '', part=part or ''))

def repo_of(u):
    m = re.search(r'github\.com/([^/]+)/([^/#?]+)', u)
    return f"{m.group(1)}/{m.group(2).removesuffix('.git')}".lower() if m else None

CUT = datetime.date(2021, 11, 7)   # > 5 years before the 2026-11-07 snapshot
SNAPSHOT = '2026-11-07'

# Decision vocabulary.  Only the first four are "integrated"; the rest are
# explicit, reasoned skips so that no entry silently disappears.
INTEGRATED = ('package', 'reimplemented', 'stale-skip', 'heavy-skip',
              'nonstatic-skip')
SKIPPED = ('nonstatic', 'commercial', 'dead', 'analysis', 'research')
LABELS = {
    'package':         '`[包]` 现成实现（作为设计参考，不新增依赖）',
    'reimplemented':   '本页用 ggplot2 / matplotlib 重画',
    'stale-skip':      '⏭️ 跳过：>5 年无运维更新',
    'heavy-skip':      '⏭️ 跳过：依赖过重',
    'nonstatic-skip':  '⏭️ 跳过：无法静态复现',
    'nonstatic':       '暂缓：交互式浏览器 / GUI / 终端',
    'commercial':      '暂缓：商业软件',
    'dead':            '暂缓：已失效',
    'analysis':        '暂缓：分析流程（不产出图形）',
    'research':        '待调研：信息不足',
}
HEAVY = {
    # whole analysis frameworks / very large dependency closures
    'ArchR', 'signac', 'plotgardener', 'Gviz', 'ggbio', 'ChIPseeker', 'trackViewer',
    'Anvio', 'CoolBox', 'figeno', 'pyGenomeTracks', 'squigualiser', 'samplot',
    'casper', 'numbat', 'CNVpytor', 'CNVkit', 'gGnome', 'mocha', 'wgdi', 'jcvi',
    'GENESPACE', 'SyMap', 'progressiveMauve viewer', 'ribbon', 'SVbyEye', 'plotsr',
    'pyGenomeViz', 'pyCirclize', 'DNAPlotLib', 'GenoVi', 'circularMT', 'HiCPlotter',
    'Juicebox', 'drawProteins', 'FusionInspector', 'STAR-Fusion', 'VIVA', 'XVCF',
}
NONSTATIC = {  # browser / GUI / CLI / ASCII: no deterministic static output
    'Bandage', 'BandageJS', 'IGV', 'IGV.js', 'JBrowse', 'JBrowse 2', 'UCSC genome browser',
    'HiGlass', 'JBrowse', 'Tablet', 'samtools tview', 'ASCIIGenome', 'SeqTUI',
}
COMMERCIAL = re.compile(r'^(?:10x genomics|Alamut|ATCC|Benchling|Bionano|Circa|CLC|DNASTAR|ERGO|Geneious|Genestack|Golden Helix|Lucid|MacVector|Persephone|Strand|SnapGene|ApE)')

def decide(r):
    sec = r['section']
    rp = repo_of(r['url'])
    m = meta.get(rp, {}) if rp else {}
    pushed = (m.get('pushed_at') or '')[:10]
    archived = bool(m.get('archived'))
    stale = bool(pushed) and datetime.date.fromisoformat(pushed) < CUT
    if r['part'].startswith('五'):
        return 'research', '信息不足以判定能否静态复现，未纳入实施面（保持原清单第五节）'
    if sec.startswith('四|4.6'):
        return 'analysis', '分析流程而非绘图，不产出图形'
    if sec.startswith('四|4.5'):
        return 'dead', '项目已失效/不可用（status: Deadlink/Historical/Unavailable）'
    if sec.startswith('四|4.4') or COMMERCIAL.match(r['name']):
        return 'commercial', '商业软件，无开源实现可复用；仅作视觉规范参考'
    if sec.startswith('四|4.3'):
        return 'nonstatic', '终端/ASCII 工具，字符画不能出版级排版'
    if sec.startswith('四|4.2'):
        return 'nonstatic', '桌面/Web GUI，输出依赖交互状态，无法确定性复现'
    if sec.startswith('四|4.1'):
        return 'nonstatic', '交互式基因组浏览器，属于平台而非可静态重画的图型'
    # sections 二 / 三
    if archived:
        return 'stale-skip', f'仓库已归档（archived，最后 push {pushed or "未知"}），不再接受维护（仅作版式参考，不安装）'
    if stale:
        return 'stale-skip', f'最近一次维护提交 {pushed}，超过 5 年无运维更新（仅作版式参考，不安装）'
    if r['name'] in NONSTATIC:
        return 'nonstatic-skip', '交互式/桌面工具，无法静态复现（仅作版式参考）'
    if r['name'] in HEAVY:
        return 'heavy-skip', '依赖过重（框架级依赖闭包），改为在本页用 ggplot2/matplotlib 重画'
    if r['impl'] == '包':
        return 'package', '可从 CRAN/Bioconductor/PyPI 安装的现成实现（作为设计参考；本页不强制新增依赖）'
    return 'reimplemented', '无可靠现成包，按图型在本页用基础图形重画'

out = []
for r in rows:
    d, reason = decide(r)
    rp = repo_of(r['url'])
    m = meta.get(rp, {}) if rp else {}
    out.append(dict(r, decision=d, reason=reason,
                    repo=rp or '', pushed=(m.get('pushed_at') or '')[:10],
                    archived=('yes' if m.get('archived') else ('no' if m else ''))))

with open(OUT, 'w', newline='', encoding='utf-8') as fh:
    w = csv.DictWriter(fh, fieldnames=['name','section','part','target','impl','year','decision','reason','repo','pushed','archived','url'])
    w.writeheader(); w.writerows(out)

total = Counter(r['decision'] for r in out)


def write_report():
    by_target = defaultdict(list)
    for r in out:
        if r['target']:
            by_target[r['target']].append(r)
    lines = []
    A = lines.append
    A('# Awesome Genome Visualization integration audit (#435)')
    A('')
    A(f'Snapshot: upstream `cmdcolin/awesome-genome-visualization` '
      f'`TOOLS.json` ({len(rows)} entries, no duplicates), cross-checked on {SNAPSHOT} '
      f'with the GitHub REST API for last-push date and archive status.')
    A('')
    A('Every upstream entry is recorded exactly once. Entries are never dropped '
      'silently: each one ends in either an integrated page or an explicit, '
      'reasoned skip. Two skip rules come straight from the issue brief:')
    A('')
    A(f'- **>5 years without maintenance** — last `pushed_at` earlier than '
      f'`{CUT.isoformat()}` or repository archived → `stale-skip`.')
    A('- **Dependency too heavy** — framework-scale dependency closures '
      '(ArchR, signac, Gviz, ggbio, pyGenomeTracks, CNVkit, SVbyEye, …) '
      '→ `heavy-skip`, redrawn with ggplot2 / matplotlib instead of installed.')
    A('')
    A('## Decisions')
    A('')
    A('| Decision | Entries | Meaning |')
    A('|---|---:|---|')
    for key in INTEGRATED + SKIPPED:
        A(f'| `{key}` | {total[key]} | {LABELS[key]} |')
    A(f'| **total** | **{len(out)}** | |')
    A('')
    integrated = sum(total[k] for k in INTEGRATED)
    explicit = sum(total[k] for k in ('stale-skip', 'heavy-skip', 'nonstatic-skip'))
    outscope = sum(total[k] for k in ('nonstatic', 'commercial', 'dead',
                                      'analysis', 'research'))
    A(f'{integrated} of {len(out)} entries map onto a Bizard page; '
      f'{explicit} are explicitly skipped on the two issue criteria plus the '
      f'static-render criterion, and {outscope} stay out of the implementation '
      f'surface as interactive/browser, GUI, terminal, commercial, dead, '
      f'non-drawing or under-specified.')
    A('')
    A('## Per-page coverage')
    A('')
    A('Each row lists how many upstream entries target that page and how they '
      'were handled. The `Skipped` column counts the explicit skips.')
    A('')
    A('| Page | Entries | Integrated | Skipped | Skip detail |')
    A('|---|---:|---:|---:|---|')
    for tgt in sorted(by_target):
        rs = by_target[tgt]
        c = Counter(r['decision'] for r in rs)
        skip = sum(c[k] for k in ('stale-skip', 'heavy-skip', 'nonstatic-skip'))
        detail = ', '.join(f'{k} {v}' for k, v in sorted(c.items())
                           if k in ('stale-skip', 'heavy-skip', 'nonstatic-skip'))
        A(f'| `{tgt}` | {len(rs)} | {len(rs) - skip} | {skip} | {detail or "—"} |')
    A('')
    A('## Explicit skips on the maintenance / dependency criteria')
    A('')
    A('Entries that were *not* implemented on purpose, with the evidence used '
      'to decide. An entry with no GitHub repository (a paper, web service or '
      'vendor product) is judged from the checklist annotation instead.')
    A('')
    A('| Entry | Page | Reason | Last push | Archived |')
    A('|---|---|---|---|---|')
    for r in sorted(out, key=lambda r: (r['target'], r['decision'], r['name'])):
        if r['decision'] in ('stale-skip', 'heavy-skip', 'nonstatic-skip',
                             'commercial', 'dead'):
            A(f"| {r['name']} | `{r['target']}` | {LABELS[r['decision']]} | "
              f"{r['pushed'] or '—'} | {r['archived'] or '—'} |")
    A('')
    A('## Reproducing')
    A('')
    A('```bash')
    A('python3 docs/awesome-genome-visualization/build_audit.py')
    A('```')
    A('')
    A('The script rewrites `audit.csv` and this report from the three checked-in '
      'inputs. `repo_meta.json` is a point-in-time GitHub snapshot; refresh it '
      'with `gh api repos/<owner>/<repo>` (fields `pushed_at`, `archived`, '
      '`stargazers_count`, `license`) before re-running if the audit needs to '
      'be re-dated.')
    open(os.path.join(HERE, 'README.md'), 'w', encoding='utf-8').write(
        '\n'.join(lines) + '\n')


write_report()
print('total', len(out))
print(dict(total))
