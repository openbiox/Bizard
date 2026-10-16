import { describe, expect, it } from "vitest";
import { checkContribution, detectCredentialLevel, submitContribution } from "../src/contribute.js";
import type { ExecFn } from "../src/contribute.js";

const goodQmd = `---
title: "Ridgeline Plot"
author: "[Contributor](https://github.com/contributor)"
---

Intro prose that is long enough to satisfy the description rule of the template.

## Setup

-   Programming language: R
-   Dependent packages: \`ggplot2\`

\`\`\`{r packages setup}
library(ggplot2)
\`\`\`

\`\`\`{r fig-demo}
ggplot(df, aes(x, y)) + geom_point()
\`\`\`
`;

describe("checkContribution", () => {
  it("passes a well-formed new tutorial and previews +10 points", () => {
    const r = checkContribution({ contributionType: "new-tutorial", content: goodQmd, language: "en" });
    expect(r.pass).toBe(true);
    expect(r.issues).toEqual([]);
    expect(r.pointsPreview).toBe("+10");
  });

  it("flags missing Setup section and missing author", () => {
    const bad = goodQmd.replace(/## Setup[\s\S]*?```{r packages setup}\nlibrary\(ggplot2\)\n```/, "").replace("author:", "xuthor:");
    const r = checkContribution({ contributionType: "new-tutorial", content: bad, language: "en" });
    expect(r.pass).toBe(false);
    expect(r.issues.some((i) => /Setup/i.test(i))).toBe(true);
    expect(r.issues.some((i) => /author/i.test(i))).toBe(true);
  });
});

// ── degradation chain ──────────────────────────────────────────────

describe("detectCredentialLevel", () => {
  it("level 1 when gh auth status succeeds", async () => {
    const exec: ExecFn = async () => ({ code: 0, stdout: "", stderr: "" });
    expect(await detectCredentialLevel({ exec })).toBe(1);
  });

  it("level 2 when gh missing but GITHUB_TOKEN set", async () => {
    const exec: ExecFn = async () => ({ code: 127, stdout: "", stderr: "not found" });
    expect(await detectCredentialLevel({ exec, githubToken: "t" })).toBe(2);
  });

  it("level 3 when gh installed but NOT authenticated and no token", async () => {
    const exec: ExecFn = async (cmd, args) =>
      cmd === "gh" && args[0] === "--version" ? { code: 0, stdout: "gh version 2.0", stderr: "" }
      : { code: 1, stdout: "", stderr: "not logged in" };
    // gh auth status fails (exit 1) → not level 1; no token → level 3
    expect(await detectCredentialLevel({ exec })).toBe(3);
  });

  it("level 3 when nothing available", async () => {
    const exec: ExecFn = async (cmd, args) =>
      cmd === "gh" && args[0] === "--version" ? { code: 0, stdout: "gh", stderr: "" }
      : { code: 127, stdout: "", stderr: "" };
    expect(await detectCredentialLevel({ exec })).toBe(3);
  });
});

describe("submitContribution", () => {
  it("level 1: runs gh issue create with title and body, returns issue URL", async () => {
    const calls: Array<{ cmd: string; args: string[] }> = [];
    const exec: ExecFn = async (cmd, args) => {
      calls.push({ cmd, args });
      return { code: 0, stdout: "https://github.com/openbiox/Bizard/issues/999\n", stderr: "" };
    };
    const res = await submitContribution(
      { contributionType: "new-tutorial", title: "New chart", body: "Body text", asPR: false },
      { exec },
    );
    expect(res.level).toBe(1);
    expect(res.submitted).toBe(true);
    expect(res.url).toContain("issues/999");
    const create = calls.find((c) => c.args.includes("issue"))!;
    expect(create.cmd).toBe("gh");
    expect(create.args).toContain("--title");
    expect(create.args.join(" ")).toContain("New chart");
  });

  it("level 1: labels new-tutorial with the repo's real 新增图表-新教程 label", async () => {
    const calls: Array<{ cmd: string; args: string[] }> = [];
    const exec: ExecFn = async (cmd, args) => {
      calls.push({ cmd, args });
      return { code: 0, stdout: "https://github.com/openbiox/Bizard/issues/1\n", stderr: "" };
    };
    await submitContribution(
      { contributionType: "new-tutorial", title: "New chart", body: "Body", asPR: false },
      { exec },
    );
    const create = calls.find((c) => c.args.includes("issue"))!;
    const labelIdx = create.args.indexOf("--label");
    expect(labelIdx).toBeGreaterThanOrEqual(0);
    expect(create.args[labelIdx + 1]).toBe("新增图表-新教程");
  });

  it("level 1: submits fix/translation/gallery-image UNLABELED (no nonexistent labels)", async () => {
    const calls: Array<{ cmd: string; args: string[] }> = [];
    const exec: ExecFn = async (cmd, args) => {
      calls.push({ cmd, args });
      return { code: 0, stdout: "https://github.com/openbiox/Bizard/issues/2\n", stderr: "" };
    };
    for (const contributionType of ["fix", "translation", "gallery-image"] as const) {
      await submitContribution(
        { contributionType, title: "T", body: "B", asPR: false },
        { exec },
      );
      const create = calls.find((c) => c.args.includes("issue"))!;
      expect(create.args).not.toContain("--label");
    }
  });

  it("level 2: POSTs to GitHub issues API with token header", async () => {
    let capturedInit: RequestInit | undefined;
    let capturedUrl = "";
    let capturedBody: Record<string, unknown> = {};
    const fetchMock = (async (url: string | URL, init?: RequestInit) => {
      capturedUrl = String(url);
      capturedInit = init;
      return new Response(JSON.stringify({ html_url: "https://github.com/openbiox/Bizard/issues/1000" }), { status: 201 });
    }) as typeof fetch;
    const res = await submitContribution(
      { contributionType: "new-tutorial", title: "New chart", body: "Body", asPR: false },
      { exec: async () => ({ code: 127, stdout: "", stderr: "" }), githubToken: "tok", fetch: fetchMock },
    );
    expect(res.level).toBe(2);
    expect(res.submitted).toBe(true);
    expect(capturedUrl).toBe("https://api.github.com/repos/openbiox/Bizard/issues");
    expect((capturedInit?.headers as Record<string, string>)["Authorization"]).toBe("Bearer tok");
    capturedBody = JSON.parse(String(capturedInit?.body));
    expect(capturedBody.labels).toEqual(["新增图表-新教程"]);
  });

  it("level 3: returns draft + deep-link, no exec/fetch used", async () => {
    const res = await submitContribution(
      { contributionType: "fix", title: "Fix", body: "Body" },
      { exec: async () => ({ code: 127, stdout: "", stderr: "" }) },
    );
    expect(res.level).toBe(3);
    expect(res.submitted).toBe(false);
    expect(res.draft).toContain("Fix");
    expect(res.deepLink).toMatch(/^https:\/\/github\.com\/openbiox\/Bizard\/issues\/new\?/);
  });

  it("truncates over-long body and notes truncation", async () => {
    const long = "x".repeat(6000);
    const res = await submitContribution(
      { contributionType: "fix", title: "T", body: long, asPR: false },
      { exec: async () => ({ code: 127, stdout: "", stderr: "" }) },
    );
    expect(res.level).toBe(3);
    expect(res.bodyTruncated).toBe(true);
    expect(res.draft!.length).toBeLessThanOrEqual(4000 + 200); // body + markers
  });
});
