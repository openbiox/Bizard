#!/usr/bin/env node
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { loadSnapshot } from "./bundle.js";
import { createBizardServer } from "./server.js";

const bundle = loadSnapshot();
if (!bundle) {
  console.error("bizard-mcp: no bundled snapshot found (src/snapshot/bundle.json missing). Tools will return a build hint until `npm run build:bundle` runs.");
}
const server = createBizardServer(bundle);
await server.connect(new StdioServerTransport());
