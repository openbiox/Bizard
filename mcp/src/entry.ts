import { loadSnapshot } from "./bundle.js";
import { createBizardServer } from "./server.js";
import { startHttpServer } from "./http.js";
import type { Bundle } from "./types.js";

const DEFAULT_HTTP_PORT = 3000;
const DEGRADED_NOTE =
  "bizard-mcp: no bundled snapshot found. Tools will return a build hint until `npm run build:bundle` runs.";

export interface CliDeps {
  loadSnapshotFn?: () => Bundle | null;
  connect?: (server: ReturnType<typeof createBizardServer>) => Promise<void>;
  startHttp?: (server: ReturnType<typeof createBizardServer>, port: number) => void;
  env?: Record<string, string | undefined>;
  stderr?: (line: string) => void;
}

/**
 * CLI entry logic, testable in isolation: degraded start (stderr note, server still
 * connects) when the snapshot is missing, `--http` for the streamable-HTTP transport.
 */
export async function runCli(argv: string[], deps: CliDeps = {}): Promise<void> {
  const load = deps.loadSnapshotFn ?? loadSnapshot;
  const log = deps.stderr ?? ((line: string) => console.error(line));
  const env = deps.env ?? process.env;

  const bundle = load();
  if (!bundle) {
    log(DEGRADED_NOTE);
  }
  const server = createBizardServer(bundle);

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
