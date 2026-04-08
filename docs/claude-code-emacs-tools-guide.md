# Claude Code Emacs MCP Tools Guide

Claude Code sessions started inside Emacs (SPC a n) have access to MCP tools
that let Claude interact with your editor. These tools are registered via
`claude-code-ide.el` and the custom extensions in `config.el`.

There are 13 tools total: 5 built-in handler tools, 5 emacs analysis tools
(xref, imenu, treesit, project-info), and 3 custom tools (2 window + 3 REPL)
defined in `~/.config/doom/config.el`.


## Automatic Context (No Tool Needed)

Before covering the tools, note that Claude receives two things automatically
without calling any tool:

- **Active buffer path** -- Claude always knows which file you have open.
- **Selection/cursor position** -- Updated in real-time (debounced 50ms) via
  the `selection_changed` notification. Claude sees your cursor line, column,
  and any selected text.

You can also push context manually:
- `SPC a s` -- send selected code as @-mention context
- `SPC a @` -- send current file reference
- `SPC a i` -- send selection + typed instruction
- `SPC a f` -- "fix errors in this file"


## Built-in Tools (from claude-code-ide.el)

### openFile

Opens a file in Emacs with optional text selection.

| Parameter   | Type    | Required | Description                              |
|-------------|---------|----------|------------------------------------------|
| filePath    | string  | yes      | Path to the file to open                 |
| startLine   | integer | no       | Start line for selection                 |
| endLine     | integer | no       | End line for selection                   |
| startText   | string  | no       | Start text pattern (takes precedence over lines) |
| endText     | string  | no       | End text pattern                         |

Example prompts:
- "Open the system.clj file"
- "Show me lines 40-60 of common.clj"
- "Open config.edn and highlight the :stitch section"


### getDiagnostics

Reads Flycheck/Flymake errors and warnings from your buffers. Claude calls this
to see what your linter/LSP is reporting.

| Parameter | Type   | Required | Description                                |
|-----------|--------|----------|--------------------------------------------|
| uri       | string | no       | File URI to check. Omit for all files.     |

Example prompts:
- "What errors does my editor show?"
- "Check diagnostics for this file"
- "Are there any warnings in the project?"


### openDiff

Shows an ediff view comparing the current file against Claude's proposed changes.
This is how Claude presents code changes for your review -- you accept or reject
in the ediff UI.

| Parameter         | Type   | Required | Description                        |
|-------------------|--------|----------|------------------------------------|
| old_file_path     | string | yes      | Path to the original file          |
| new_file_path     | string | yes      | Path to the new file (usually same)|
| new_file_contents | string | yes      | Proposed new content               |
| tab_name          | string | yes      | Name for the diff tab              |

This tool is called automatically when Claude proposes edits. You don't need to
ask for it directly -- just ask Claude to change code and it will show the diff.


### close_tab

Closes a buffer by path or tab name.

| Parameter | Type   | Required | Description               |
|-----------|--------|----------|---------------------------|
| path      | string | no       | File path to close        |
| tab_name  | string | no       | Tab/buffer name to close  |


### closeAllDiffTabs

Closes all open diff views in the current session. No parameters.


## Emacs Tools (from claude-code-ide-emacs-tools.el)

### xref-find-references

Queries your LSP backend for all references to a symbol. Returns file:line:summary
for each reference.

| Parameter  | Type   | Required | Description                       |
|------------|--------|----------|-----------------------------------|
| identifier | string | yes      | Symbol name to find references of |
| file_path  | string | yes      | File for buffer context           |

Example prompts:
- "Find all references to `process-batch` in the codebase"
- "Where is `BatchConfig` used?"
- "Who calls this function?"

Requires an active LSP server for the file type (clojure-lsp, etc.).


### xref-find-apropos

Searches for symbols matching a pattern across the project via LSP.

| Parameter | Type   | Required | Description                    |
|-----------|--------|----------|--------------------------------|
| pattern   | string | yes      | Pattern to search for          |
| file_path | string | yes      | File for buffer context        |

Example prompts:
- "Find all symbols matching 'batch-gen'"
- "Search for functions with 'stitch' in the name"


### project-info

Returns the project directory, active buffer name, and total file count.
No parameters.

Example prompts:
- "What project am I in?"
- "Show me project info"


### imenu-list-symbols

Lists all symbols (functions, vars, classes) in a file using imenu. Returns
file:line: [category] symbol-name for each entry.

| Parameter | Type   | Required | Description          |
|-----------|--------|----------|----------------------|
| file_path | string | yes      | File to list symbols |

Example prompts:
- "List all functions in this file"
- "What's defined in common.clj?"
- "Show me the structure of this namespace"


### treesit-info

Reads the tree-sitter AST for a file or position. Useful for understanding
syntax structure in languages with tree-sitter grammars (Go, Python, JS, etc.).

| Parameter          | Type    | Required | Description                          |
|--------------------|---------|----------|--------------------------------------|
| file_path          | string  | yes      | File to analyze                      |
| line               | integer | no       | Line number (1-based)                |
| column             | integer | no       | Column number (0-based)              |
| whole_file         | boolean | no       | Show entire file's syntax tree       |
| include_ancestors  | boolean | no       | Include parent node hierarchy        |
| include_children   | boolean | no       | Include child nodes                  |

Example prompts:
- "What's the syntax tree at my cursor?"
- "Show me the AST for this Go function"

Note: Requires tree-sitter support for the file's language. Clojure does not
have a tree-sitter grammar in standard Emacs, so this is mainly useful for
Go, Python, JavaScript, TypeScript, etc.


## Custom Tools: Window Management (from config.el)

### emacs_navigate

Opens a file in Emacs, optionally at a specific line, optionally in a new split
window. This is the tool that lets Claude arrange your editor layout.

| Parameter | Type   | Required | Description                                    |
|-----------|--------|----------|------------------------------------------------|
| file_path | string | yes      | File path (absolute or project-relative)       |
| line      | number | no       | Line number to jump to (1-based)               |
| split     | string | no       | "current" (default), "right", or "below"       |

Example prompts:
- "Open common.clj in a split below"
- "Show me system.clj on the right side at line 45"
- "Put the test file side-by-side with the source"
- "Open the config next to this file"


### emacs_window

Manages Emacs window layout: splitting, closing, switching focus, rebalancing.

| Parameter | Type   | Required | Description                                              |
|-----------|--------|----------|----------------------------------------------------------|
| action    | string | yes      | split_right, split_below, close, close_others, next, previous, balance |

Example prompts:
- "Close this split"
- "Go back to single window"
- "Balance the window sizes"
- "Switch to the other pane"


## Custom Tools: CIDER REPL (from config.el)

These tools require an active CIDER REPL connection. Start one with
`cider-jack-in` (SPC m ') or `cider-connect` before asking Claude to evaluate
code.

### emacs_repl_eval

Evaluates a Clojure expression in the connected CIDER REPL and returns the
result. Output is truncated at 4000 characters.

| Parameter | Type   | Required | Description                                    |
|-----------|--------|----------|------------------------------------------------|
| code      | string | yes      | Clojure expression to evaluate                 |
| namespace | string | no       | Namespace to eval in (default: current buffer) |

The response includes:
- `stdout:` -- any printed output (from println, etc.)
- `=> value` -- the return value
- `Error:` -- stack trace if evaluation fails

Example prompts:
- "Eval `(+ 1 2)` in the REPL"
- "Run `(require '[amperity.profile.rama.common :as common])` in the REPL"
- "Check what `(keys (system))` returns"
- "Evaluate this function definition in the REPL"
- "What does `(doc merge)` say?"
- "In the user namespace, eval `(refresh)`"


### emacs_repl_status

Checks whether a CIDER REPL is connected and returns connection details.
No parameters.

Response includes:
- Connection state (connected or not)
- REPL type (clj, cljs)
- Current namespace
- Project directory

Example prompts:
- "Is the REPL connected?"
- "What namespace am I in?"
- "Check REPL status"


### emacs_repl_load_file

Loads and compiles a Clojure file into the REPL, making all its definitions
available. Equivalent to `C-c C-l` / `cider-load-file`.

| Parameter | Type   | Required | Description                                  |
|-----------|--------|----------|----------------------------------------------|
| file_path | string | yes      | Path to the .clj file to load                |

Example prompts:
- "Load this file into the REPL"
- "Compile common.clj into the REPL"
- "Load the test file so I can run the tests"


## Typical Workflows

### Code review with side-by-side view
> "Open the test file on the right, and show me the source on the left at the
>  function definition"

Claude uses `emacs_navigate` twice: once for the source, once with `split: "right"`
for the test file.

### Interactive development
> "Load this namespace into the REPL, then evaluate the `process-batch` function
>  with test data"

Claude uses `emacs_repl_load_file` to compile the file, then `emacs_repl_eval`
to call the function.

### Diagnosing errors
> "What errors does my editor show? Then check the REPL for the same namespace"

Claude calls `getDiagnostics` for linter/LSP errors, then `emacs_repl_eval` to
test the namespace in the REPL.

### Exploring unfamiliar code
> "List the functions in this file, find all references to `run-batch`, then
>  open the most relevant caller in a split"

Claude chains `imenu-list-symbols` -> `xref-find-references` -> `emacs_navigate`.


## Limitations

- **No executeCode** -- Claude cannot run arbitrary Elisp. The custom tools
  cover window management and REPL interaction; anything else requires manual
  Emacs commands.
- **REPL must be connected first** -- Claude cannot start a CIDER session.
  Run `cider-jack-in` or `cider-connect` yourself before asking Claude to
  evaluate code.
- **Synchronous REPL eval** -- Long-running expressions block Emacs until they
  return. Avoid asking Claude to eval things like `(Thread/sleep 30000)` or
  infinite sequences without `take`.
- **Output truncation** -- REPL output beyond 4000 characters is truncated.
  Adjust `my/claude-code-repl-max-output` if needed.
- **tree-sitter** -- Only works for languages with tree-sitter grammars in your
  Emacs build. Clojure is not supported; Go, Python, JS/TS are.
- **LSP required for xref tools** -- `xref-find-references` and
  `xref-find-apropos` need an active LSP server (clojure-lsp, gopls, etc.).


## Configuration Reference

All custom tools are defined in `~/.config/doom/config.el` inside the
`(use-package! claude-code-ide ...)` block:

- Window tools: lines ~490-536 (`my/claude-code-navigate`, `my/claude-code-window`)
- REPL tools: lines ~538-616 (`my/claude-code-repl-eval`, `my/claude-code-repl-status`, `my/claude-code-repl-load-file`)
- Allowed tools mode: `(setq claude-code-ide-mcp-allowed-tools 'auto)` -- includes all registered tools
- Truncation limit: `(defvar my/claude-code-repl-max-output 4000)`
