import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { InMemoryTransport } from "@modelcontextprotocol/sdk/inMemory.js";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";

export async function connect(server: McpServer): Promise<Client> {
  const client = new Client({ name: "test-client", version: "0.0.1" });
  const [clientTransport, serverTransport] = InMemoryTransport.createLinkedPair();
  await server.connect(serverTransport);
  await client.connect(clientTransport);
  return client;
}

export async function listTools(server: McpServer): Promise<Array<{ name: string }>> {
  const client = await connect(server);
  const res = await client.listTools();
  await client.close();
  return res.tools as Array<{ name: string }>;
}

export async function callTool(
  server: McpServer, name: string, args: Record<string, unknown>
): Promise<{ isError?: boolean; content: Array<{ type: string; text: string }> }> {
  const client = await connect(server);
  const res = await client.callTool({ name, arguments: args });
  await client.close();
  return res as { isError?: boolean; content: Array<{ type: string; text: string }> };
}

export async function clientWith(server: McpServer): Promise<Client> {
  return connect(server);
}
