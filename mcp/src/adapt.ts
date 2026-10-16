import type { Bundle } from "./types.js";
import { findChart } from "./tutorial.js";

export interface GuidanceSection {
  title: string;
  items: string[];
}

export interface AdaptGuidance {
  chartId: string;
  language: string;
  userDataContext: string | null;
  sections: GuidanceSection[];
}

const FAILURE_MODES: Record<string, string[]> = {
  Distribution: [
    "Too few data points: violin/box plots need >= 5 values per group; check with nrow()/len() before plotting.",
    "Factor levels missing: a group absent from the data drops silently — verify levels(factor_col) / unique(df.group).",
    "Distribution summary checks: confirm the plotted variable's range and spread make sense for a distribution chart.",
  ],
  Correlation: [
    "Non-numeric columns passed to correlation: coerce or drop them first (types(df) / str(df)).",
    "Perfect collinearity produces NaN coefficients — inspect the variance of each variable.",
  ],
  default: [
    "Wrong data shape (long vs wide): reshape (pivot_longer / melt / pd.melt) before plotting.",
    "Package version drift: reinstall with the pinned versions from the Setup section.",
  ],
};

const PARAM_HINTS: Record<string, string[]> = {
  r: [
    "Map user columns onto aes(): replace example column names (e.g. aes(x = wt) → aes(x = your_col)).",
    "Adjust scale_/theme_ options only after the base plot renders — change one layer at a time.",
  ],
  python: [
    "Replace example column names in the plotting call with your DataFrame columns.",
    "Keep seaborn/matplotlib layer order: data → axes → styling.",
  ],
  julia: [
    "Replace example column symbols in the plotting macro with your table columns.",
    "Keep Makie/Gadfly attribute names consistent with the loaded backend.",
  ],
};

const MAX_ALTERNATIVES = 3;
const DESCRIPTION_PREVIEW_CHARS = 80;

export function adaptGuidance(
  bundle: Bundle, chartId: string, language: "r" | "python" | "julia",
  userDataContext?: string,
): AdaptGuidance {
  const chart = findChart(bundle, chartId);
  if (!chart) throw new Error(`Unknown chartId: ${chartId}`);
  const failureModes = FAILURE_MODES[chart.level2] ?? FAILURE_MODES.default;
  const alternatives = bundle.charts
    .filter((c) => c.level1 === chart.level1 && c.id !== chart.id)
    .slice(0, MAX_ALTERNATIVES)
    .map((c) => `${c.name} (${c.tutorialUrl}) — ${c.description.slice(0, DESCRIPTION_PREVIEW_CHARS)}`);

  return {
    chartId: chart.id,
    language,
    userDataContext: userDataContext ?? null,
    sections: [
      {
        title: "Data shape check",
        items: [
          `Before plotting, verify your data has the columns this ${chart.type} expects (long format unless stated otherwise).`,
          "Run a structural self-check first: str(df) / df.info() / show(schema).",
        ],
      },
      {
        title: "Dependency preparation",
        items: [
          `Install exactly the packages listed in the tutorial Setup (${chart.codeRefs.join(", ") || "see tutorial"}).`,
          "Use the tutorial's install-guard pattern: check-installed → install → library(), never bare install.packages at top level.",
        ],
      },
      {
        title: "Parameter mapping",
        items: PARAM_HINTS[language],
      },
      {
        title: "Common failure modes",
        items: failureModes,
      },
      {
        title: "Alternatives",
        items: alternatives.length > 0
          ? alternatives
          : ["No same-category alternatives in the bundle; browse bizard_list_categories."],
      },
    ],
  };
}
