#!/usr/bin/env node
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { loadSnapshot } from "./bundle.js";
import { createBizardServer } from "./server.js";
import { startHttpServer } from "./http.js";

const DEFAULT_HTTP_PORT = 3000;

const wantHttp = process.argv.includes("--http");
const bundle = loadSnapshot();
if (!bundle) {
  console.error("bizard-mcp: bundle snapshot missing — build with `npm run build:bundle` or reinstall.");
  process.exit(1);
}
const server = createBizardServer(bundle);

if (wantHttp) {
  const port = Number(process.env.BIZARD_HTTP_PORT ?? DEFAULT_HTTP_PORT);
  startHttpServer(server, port);
} else {
  await server.connect(new StdioServerTransport());
}
