import { createServer, type IncomingMessage, type ServerResponse } from "node:http";
import { StreamableHTTPServerTransport } from "@modelcontextprotocol/sdk/server/streamableHttp.js";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";

const RATE_CAPACITY = 60; // requests/min per client IP
const RATE_INTERVAL_MS = 60_000;
const BODY_MAX_BYTES = 1_000_000; // JSON-RPC messages are small; reject larger uploads

/** Trust X-Forwarded-For for client keying (spec §6.2 前置反代). Off by default:
 *  without a proxy in front, clients could spoof fresh buckets. Read per
 *  request so a config change takes effect without a restart. */
function trustProxy(): boolean {
  const v = process.env.BIZARD_TRUST_PROXY;
  return v === "1" || v === "true";
}

interface Bucket { tokens: number; last: number; }

/** Client identity for rate limiting: the IP that just handed the request to
 *  the trusted proxy (rightmost X-Forwarded-For hop) when behind one, else the
 *  socket address. The leftmost hop is client-controlled and spoofable. */
function clientIp(req: IncomingMessage): string {
  if (trustProxy()) {
    const forwarded = req.headers["x-forwarded-for"];
    if (typeof forwarded === "string" && forwarded.length > 0) {
      const hops = forwarded.split(",").map((h) => h.trim()).filter(Boolean);
      if (hops.length > 0) return hops[hops.length - 1];
    }
  }
  return req.socket.remoteAddress ?? "unknown";
}

/**
 * Stateless Streamable HTTP endpoint (spec §6.2: public anonymous access).
 * The factory builds one McpServer per POST — the SDK allows a single
 * transport per server instance, so sharing one across concurrent requests
 * throws "Already connected to a transport" (and kills the process via an
 * unhandled rejection). Per-IP token-bucket rate limiting.
 */
export function startHttpServer(makeServer: () => McpServer, port: number): void {
  const buckets = new Map<string, Bucket>();

  const http = createServer((req, res) => {
    const ip = clientIp(req);
    if (!allow(ip, buckets)) {
      res.writeHead(429, { "Retry-After": "30" });
      res.end(JSON.stringify({ error: "rate limited" }));
      return;
    }
    if (req.method === "POST" && req.url === "/mcp") {
      handlePost(makeServer(), req, res).catch((e) => {
        console.error(`bizard-mcp HTTP: request failed: ${(e as Error).message}`);
        if (res.headersSent) return;
        // Oversized uploads are the client's doing — 413, not a server fault.
        const status = e instanceof BodyTooLargeError ? 413 : 500;
        res.writeHead(status, { "Content-Type": "application/json" });
        res.end(JSON.stringify({ error: status === 413 ? "request body too large" : "internal error" }));
      });
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
  let size = 0;
  for await (const chunk of req) {
    size += (chunk as Buffer).length;
    if (size > BODY_MAX_BYTES) throw new BodyTooLargeError();
    chunks.push(chunk as Buffer);
  }
  const raw = Buffer.concat(chunks).toString("utf-8");
  try {
    return JSON.parse(raw || "{}");
  } catch {
    return {};
  }
}

class BodyTooLargeError extends Error {}

function allow(ip: string, buckets: Map<string, Bucket>): boolean {
  const now = Date.now();
  // Opportunistic eviction: a public endpoint sees one-off IPs (spoofed or
  // scanned); without pruning, the bucket map grows without bound.
  if (buckets.size >= RATE_CAPACITY) {
    for (const [key, b] of buckets) {
      if (now - b.last > RATE_INTERVAL_MS) buckets.delete(key);
    }
  }
  const b = buckets.get(ip) ?? { tokens: RATE_CAPACITY, last: now };
  const elapsed = now - b.last;
  b.tokens = Math.min(RATE_CAPACITY, b.tokens + (elapsed / RATE_INTERVAL_MS) * RATE_CAPACITY);
  b.last = now;
  if (b.tokens < 1) { buckets.set(ip, b); return false; }
  b.tokens -= 1;
  buckets.set(ip, b);
  return true;
}
