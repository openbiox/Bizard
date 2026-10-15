import { createServer, type IncomingMessage, type ServerResponse } from "node:http";
import { StreamableHTTPServerTransport } from "@modelcontextprotocol/sdk/server/streamableHttp.js";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";

const RATE_CAPACITY = 60; // requests/min per IP
const RATE_INTERVAL_MS = 60_000;

interface Bucket { tokens: number; last: number; }

/**
 * Stateless Streamable HTTP endpoint (spec §6.2: public anonymous access).
 * One transport per POST; per-IP token-bucket rate limiting.
 */
export function startHttpServer(server: McpServer, port: number): void {
  const buckets = new Map<string, Bucket>();

  const http = createServer((req, res) => {
    const ip = req.socket.remoteAddress ?? "unknown";
    if (!allow(ip, buckets)) {
      res.writeHead(429, { "Retry-After": "30" });
      res.end(JSON.stringify({ error: "rate limited" }));
      return;
    }
    if (req.method === "POST" && req.url === "/mcp") {
      void handlePost(server, req, res);
      return;
    }
    res.writeHead(404).end();
  });

  http.listen(port, () => {
    console.error(`bizard-mcp HTTP listening on :${port} (Streamable HTTP, anonymous, rate-limited)`);
  });
}

async function handlePost(server: McpServer, req: IncomingMessage, res: ServerResponse): Promise<void> {
  // One stateless transport per request — each POST gets a fresh transport.
  const transport = new StreamableHTTPServerTransport({ sessionIdGenerator: undefined });
  res.on("close", () => transport.close());
  await server.connect(transport);
  const body = await readBody(req);
  await transport.handleRequest(req, res, body);
}

async function readBody(req: IncomingMessage): Promise<unknown> {
  const chunks: Buffer[] = [];
  for await (const chunk of req) chunks.push(chunk as Buffer);
  const raw = Buffer.concat(chunks).toString("utf-8");
  try {
    return JSON.parse(raw || "{}");
  } catch {
    return {};
  }
}

function allow(ip: string, buckets: Map<string, Bucket>): boolean {
  const now = Date.now();
  const b = buckets.get(ip) ?? { tokens: RATE_CAPACITY, last: now };
  const elapsed = now - b.last;
  b.tokens = Math.min(RATE_CAPACITY, b.tokens + (elapsed / RATE_INTERVAL_MS) * RATE_CAPACITY);
  b.last = now;
  if (b.tokens < 1) { buckets.set(ip, b); return false; }
  b.tokens -= 1;
  buckets.set(ip, b);
  return true;
}
