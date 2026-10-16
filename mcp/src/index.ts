#!/usr/bin/env node
import { runCli } from "./entry.js";

await runCli(process.argv.slice(2));
