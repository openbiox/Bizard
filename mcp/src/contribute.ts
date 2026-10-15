import { execFile as execFileCb } from "node:child_process";
import { promisify } from "node:util";

const execFile = promisify(execFileCb);

export const BODY_MAX_CHARS = 4000;
const TITLE_MAX_CHARS = 120;
const ERROR_SNIPPET_CHARS = 200;
const MAX_BUFFER_BYTES = 10 * 1024 * 1024;

export interface ExecResult { code: number; stdout: string; stderr: string; }
export type ExecFn = (cmd: string, args: string[]) => Promise<ExecResult>;

export interface CredentialEnv {
  exec?: ExecFn;
  githubToken?: string;
  /** Overridable fetch for tests; defaults to globalThis.fetch. */
  fetch?: typeof fetch;
  ghOwner?: string; // default "openbiox"
  ghRepo?: string;  // default "Bizard"
}

export interface CheckInput {
  contributionType: "new-tutorial" | "fix" | "translation" | "gallery-image";
  content: string;
  language: "en" | "zh";
}

export interface CheckResult {
  pass: boolean;
  issues: string[];
  checklist: string[];
  pointsPreview: string;
}

export interface SubmitInput {
  contributionType: CheckInput["contributionType"];
  title: string;
  body: string;
  asPR: boolean;
}

export interface SubmitResult {
  level: 1 | 2 | 3;
  submitted: boolean;
  url: string | null;
  draft: string | null;
  deepLink: string | null;
  bodyTruncated: boolean;
  note: string;
}

const POINTS: Record<CheckInput["contributionType"], string> = {
  "new-tutorial": "+10",
  "fix": "+3",
  "translation": "+2",
  "gallery-image": "+3",
};

/** Only labels that exist in openbiox/Bizard; other types submit unlabeled for maintainer triage. */
const ISSUE_LABELS: Partial<Record<CheckInput["contributionType"], string>> = {
  "new-tutorial": "新增图表-新教程",
};

export function checkContribution(input: CheckInput): CheckResult {
  const issues: string[] = [];
  const c = input.content;
  if (!/^---\n[\s\S]*?\n---/.test(c)) issues.push("Missing YAML front matter (--- … ---).");
  if (!/^author:/m.test(c)) issues.push("YAML front matter lacks author field.");
  if (input.contributionType === "new-tutorial" && !/^## Setup/m.test(c)) {
    issues.push("New tutorials must include the fixed '## Setup' section (see Template/).");
  }
  if (input.contributionType === "new-tutorial" && !/```\{(r|python|julia)/.test(c)) {
    issues.push("New tutorials must contain at least one executable code block.");
  }
  return {
    pass: issues.length === 0,
    issues,
    checklist: [
      "Followed Template/contribution_0323.md structure",
      "Figures carry fig-alt text",
      `Contribution points reference: ${POINTS[input.contributionType]} (issue #4 rules)`,
    ],
    pointsPreview: POINTS[input.contributionType],
  };
}

export async function detectCredentialLevel(env: CredentialEnv): Promise<1 | 2 | 3> {
  const exec = execOf(env);
  const gh = await exec("gh", ["auth", "status"]);
  if (gh.code === 0) return 1;
  if (env.githubToken) return 2;
  return 3;
}

export async function submitContribution(input: SubmitInput, env: CredentialEnv): Promise<SubmitResult> {
  const owner = env.ghOwner ?? "openbiox";
  const repo = env.ghRepo ?? "Bizard";
  const truncated = input.body.length > BODY_MAX_CHARS;
  const body = truncated ? input.body.slice(0, BODY_MAX_CHARS) + "\n\n[Truncated]" : input.body;

  const level = await detectCredentialLevel(env);
  if (level === 1) return submitViaGh(input, env, owner, repo, body, truncated);
  if (env.githubToken) return submitViaApi(input, env, owner, repo, body, truncated);
  return draftResult(owner, repo, input.title, body, truncated, level);
}

async function submitViaGh(input: SubmitInput, env: CredentialEnv, owner: string, repo: string,
                           body: string, truncated: boolean): Promise<SubmitResult> {
  const label = ISSUE_LABELS[input.contributionType];
  const args = ["issue", "create",
    "--repo", `${owner}/${repo}`,
    "--title", truncateTitle(input.title),
    "--body", body];
  if (label) args.push("--label", label);
  const res = await execOf(env)("gh", args);
  if (res.code === 0) {
    const url = res.stdout.trim().split("\n").pop() ?? null;
    return { level: 1, submitted: true, url, draft: null, deepLink: null, bodyTruncated: truncated,
      note: "Submitted via gh CLI as your GitHub user (level 1)." };
  }
  return { level: 1, submitted: false, url: null, draft: body, deepLink: deepLink(owner, repo, input.title, body),
    bodyTruncated: truncated, note: `gh CLI submission failed (exit ${res.code}): ${res.stderr.slice(0, ERROR_SNIPPET_CHARS)}` };
}

async function submitViaApi(input: SubmitInput, env: CredentialEnv, owner: string, repo: string,
                            body: string, truncated: boolean): Promise<SubmitResult> {
  const token = env.githubToken as string;
  const doFetch = env.fetch ?? globalThis.fetch;
  const res = await doFetch(`https://api.github.com/repos/${owner}/${repo}/issues`, {
    method: "POST",
    headers: {
      "Authorization": `Bearer ${token}`,
      "Accept": "application/vnd.github+json",
      "Content-Type": "application/json",
      "User-Agent": "bizard-mcp",
    },
    body: JSON.stringify({
      title: truncateTitle(input.title),
      body,
      ...(ISSUE_LABELS[input.contributionType] ? { labels: [ISSUE_LABELS[input.contributionType]] } : {}),
    }),
  });
  if (res.ok) {
    const data = (await res.json()) as { html_url: string };
    return { level: 2, submitted: true, url: data.html_url, draft: null, deepLink: null, bodyTruncated: truncated,
      note: "Submitted via GitHub API with your GITHUB_TOKEN (level 2)." };
  }
  const errText = await res.text();
  return { level: 2, submitted: false, url: null, draft: body, deepLink: deepLink(owner, repo, input.title, body),
    bodyTruncated: truncated, note: `GitHub API error ${res.status}: ${errText.slice(0, ERROR_SNIPPET_CHARS)} — draft retained.` };
}

/** Promisified execFile wrapper; non-zero exits resolve (code carries the status), missing binaries resolve with code 127. */
function execOf(env: CredentialEnv): ExecFn {
  if (env.exec) return env.exec;
  return async (cmd, args) => {
    try {
      const { stdout, stderr } = await execFile(cmd, args, { maxBuffer: MAX_BUFFER_BYTES });
      return { code: 0, stdout, stderr };
    } catch (e) {
      const err = e as { code?: unknown; stdout?: string; stderr?: string; message?: string };
      const code = typeof err.code === "number" ? err.code : 1;
      return { code, stdout: err.stdout ?? "", stderr: err.stderr ?? err.message ?? "" };
    }
  };
}

function truncateTitle(t: string): string {
  return t.length > TITLE_MAX_CHARS ? t.slice(0, TITLE_MAX_CHARS - 3) + "..." : t;
}

function deepLink(owner: string, repo: string, title: string, body: string): string {
  return `https://github.com/${owner}/${repo}/issues/new?title=${encodeURIComponent(truncateTitle(title))}&body=${encodeURIComponent(body)}`;
}

function draftResult(owner: string, repo: string, title: string, body: string, truncated: boolean, level: 1 | 2 | 3): SubmitResult {
  return {
    level, submitted: false, url: null,
    draft: `Title: ${truncateTitle(title)}\n\n${body}`,
    deepLink: deepLink(owner, repo, title, body),
    bodyTruncated: truncated,
    note: "No GitHub credentials in this environment (level 3). Open the deep-link to submit the pre-filled issue in your browser.",
  };
}
