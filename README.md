# pms-infra-agent-process

----
## ⚠️ Caution

**Do not grant unrestricted control to AI.**  
Unsupervised use or misuse may lead to unintended consequences.  
All AI systems must remain strictly under human oversight and control.  
Use responsibly, with full awareness and at your own risk.  

----
## 📘 Overview

**`pms-infra-agent-process`** is a Haskell infrastructure library that provides AI agents with direct, low-level control over external processes via standard input/output.

Unlike PTY-based tools, this library communicates with spawned processes directly through `stdin`/`stdout`/`stderr` handles, without allocating a pseudo-terminal. This makes it well-suited for non-TUI programs that communicate via plain text streams, such as language servers, compilers, or custom automation scripts.

The library is a core component of the [`pty-mcp-server`](https://github.com/phoityne/pty-mcp-server) ecosystem and implements the `agent-proc-*` family of MCP tools.

---

## 🔧 Provided MCP Tools

### `agent-proc-run`
Spawns an external process with the specified command and options. The process's `stdin`/`stdout` are connected for subsequent `agent-proc-read` and `agent-proc-write` operations. Only one process can be active at a time.

Supports an `allowedAgentCmds` whitelist — if configured, only commands present in the whitelist are permitted to run.

### `agent-proc-read`
Reads up to the specified number of bytes from the `stdout` of the running process. Returns immediately with available data (non-blocking). Returns an empty string if no data is available.

### `agent-proc-write`
Writes the specified string to the `stdin` of the running process.

### `agent-proc-terminate`
Forcefully terminates the currently running process. Resets internal state so `agent-proc-run` can be called again.

---

## 🏗️ Architecture

### Module Structure

```
PMS.Infra.Agent.Process
├── DM.Type        -- Data type definitions (ProcData, AppData, tool parameter types)
├── DM.Constant    -- Constants
├── DS.Core        -- Core domain service logic
├── DS.Utility     -- Utility functions
└── App.Control    -- Application control: tool dispatch and process lifecycle management
```

### Key Design Points

- **Single active process**: Only one process can run at a time per server instance. State is managed via `STM.TMVar`.
- **Non-blocking reads**: `agent-proc-read` returns immediately without blocking, allowing the agent to poll at its own pace.
- **Whitelist enforcement**: `agent-proc-run` checks the command against `allowedAgentCmds` from the configuration before spawning. If the list is empty, all commands are denied by default.
- **Direct stdio**: No PTY allocation. Suitable for programs that do not require a terminal environment.

---

## 📦 Dependencies

- [`pms-domain-model`](https://github.com/phoityne/pms-domain-model)

---

## 📜 Credits & License

- **Execution & Process Lead:** Sonnet 4.6, Gemini 3 Flash
- **Direction & Policy:** phoityne
- **License:** Apache-2.0 — see [LICENSE](./LICENSE)

---
