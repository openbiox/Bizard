import { loadSnapshot } from "./bundle.js";
import { createBizardServer } from "./server.js";
import { startHttpServer } from "./http.js";
import { checkForUpdate, createNoticeQueue } from "./update-check.js";
import type { Bundle } from "./types.js";
import type { MutableHolder, NoticeQueue } from "./update-check.js";

const DEFAULT_HTTP_PORT = 3000;
const DEGRADED_NOTE =
  "bizard-mcp: no bundled snapshot found. Tools will return a build hint until `npm run build:bundle` runs.";

export interface CliDeps {
  loadSnapshotFn?: () => Bundle | null;
  connect?: (server: ReturnType<typeof createBizardServer>) => Promise<void>;
  startHttp?: (server: ReturnType<typeof createBizardServer>, port: number) => void;
  env?: Record<string, string | undefined>;
  stderr?: (line: string) => void;
  startUpdateCheck?: (holder: MutableHolder, notices: NoticeQueue) => void;
}

/**
 * CLI entry logic, testable in isolation: degraded start (stderr note, server still
 * connects) when the snapshot is missing, `--http` for the streamable-HTTP transport,
 * and a fire-and-forget spec §3.4 update check.
 */
export async function runCli(argv: string[], deps: CliDeps = {}): Promise<void> {
  const load = deps.loadSnapshotFn ?? loadSnapshot;
  const log = deps.stderr ?? ((line: string) => console.error(line));
  const env = deps.env ?? process.env;

  const holder: MutableHolder = { bundle: load(), etag: null };
  if (!holder.bundle) {
    log(DEGRADED_NOTE);
  }
  const notices = createNoticeQueue();
  const server = createBizardServer({ get: () => holder.bundle }, notices);

  const startCheck = deps.startUpdateCheck
    ?? ((h: MutableHolder, ns: NoticeQueue) => {
      void checkForUpdate(h, { notices: ns });
    });
  startCheck(holder, notices);

  if (argv.includes("--http")) {
    const port = Number(env.BIZARD_HTTP_PORT ?? DEFAULT_HTTP_PORT);
    if (deps.startHttp) {
      deps.startHttp(server, port);
    } else {
      startHttpServer(server, port);
    }
  } else {
    const connect = deps.connect
      ? () => deps.connect!(server)
      : async () => {
          const { StdioServerTransport } = await import("@modelcontextprotocol/sdk/server/stdio.js");
          await server.connect(new StdioServerTransport());
        };
    await connect();
  }
}
