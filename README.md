# Doom Emacs Configuration

Modular Doom Emacs config optimized for Clojure + Go development on M4 Mac.
LSP-first approach, daemon mode, Claude Code IDE integration.

## Structure

```
~/.config/doom/
├── init.el                # Doom modules
├── config.el              # Thin loader (load! calls)
├── packages.el            # Package declarations
├── modules/
│   ├── ui.el              # Theme, fonts, symbol fallbacks, display-table
│   ├── completion.el      # Corfu tuning, nerd-icons, consult-lsp, cape extras
│   ├── performance.el     # GC, native-comp, rendering, subprocess tuning
│   ├── lsp.el             # lsp-mode, lsp-ui, monorepo root detection, daemon fix
│   ├── lang-clojure.el    # clojure-mode, CIDER, lispyville, cljstyle
│   ├── lang-go.el         # go-mode, gopls, tree-sitter
│   ├── tools.el           # ace-window, string-inflection, git-timemachine, git-link, devdocs, pcre2el
│   ├── claude-code.el     # Claude Code IDE: MCP tools, REPL integration
│   └── _disabled/         # Reference configs for JS/Python (not loaded)
└── docs/
    └── claude-code-emacs-tools-guide.md
```

## Keybindings Quick Reference

### Global (SPC prefix)

#### Files & Buffers
| Key | Command | Description |
|-----|---------|-------------|
| `SPC f f` | find-file | Open file |
| `SPC f r` | recentf | Recent files |
| `SPC b b` | switch-buffer | Switch buffer |
| `SPC b k` | kill-buffer | Kill buffer |
| `SPC p p` | projectile-switch-project | Switch project |
| `SPC p f` | projectile-find-file | Find file in project |

#### Search (SPC s)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC /` | +default/search-project | Ripgrep project search with preview |
| `SPC s s` | consult-line | Search in current buffer |
| `SPC s i` | consult-imenu | Jump to symbol in file |
| `SPC s f` | consult-lsp-file-symbols | LSP symbols in current file |
| `SPC s o` | consult-outline | Jump to heading (Org/Markdown) |
| `SPC s S` | consult-lsp-symbols | Search ALL symbols across LSP workspace |
| `SPC s D` | consult-lsp-diagnostics | Browse all LSP errors/warnings |

#### Windows (SPC w)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC w w` | ace-window | Jump to window by letter |
| `SPC w v` | evil-window-vsplit | Vertical split |
| `SPC w s` | evil-window-split | Horizontal split |
| `SPC w d` | evil-window-delete | Close window |
| `SPC w =` | balance-windows | Balance window sizes |

#### Git (SPC g)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC g g` | magit-status | Magit status |
| `SPC g t` | git-timemachine | Browse file history in-place |
| `SPC g y` | git-link | Copy GitHub URL for current line |
| `SPC g Y` | git-link-commit | Copy GitHub URL for current commit |
| `SPC g O` | git-link-homepage | **O**pen repo on GitHub |
| `SPC g d` | magit-diff-buffer-file | **D**iff current file |
| `SPC g B` | magit-blame | Git blame |
| `SPC g L` | magit-log-buffer-file | File log |

#### Code (SPC c)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC c d` | lsp-find-definition | Go to definition |
| `SPC c r` | lsp-find-references | Find references |
| `SPC c a` | lsp-execute-code-action | Code actions |
| `SPC c R` | lsp-rename | Rename symbol |
| `SPC c f` | lsp-format-buffer | **F**ormat buffer via LSP |
| `SPC c W` | lsp-workspace-restart | Restart LSP **w**orkspace |

#### Code Style (SPC c s)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC c s s` | string-inflection-all-cycle | Cycle all styles |
| `SPC c s k` | string-inflection-kebab-case | **k**ebab-case |
| `SPC c s c` | string-inflection-camelcase | **c**amelCase |
| `SPC c s p` | string-inflection-pascal-case | **P**ascalCase |
| `SPC c s _` | string-inflection-underscore | snake_case |
| `SPC c s !` | string-inflection-upcase | SCREAMING_SNAKE |

#### Help (SPC h)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC h D` | devdocs-lookup | DevDocs lookup |
| `SPC h B` | devdocs-peruse | DevDocs **b**rowse |
| `SPC h d` | describe-function | Describe function |
| `SPC h k` | describe-key | Describe key |

#### Regex (SPC r)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC r p` | rxt-pcre-to-elisp | Convert PCRE (Java/Clojure) to Elisp regex |
| `SPC r e` | rxt-elisp-to-pcre | Convert Elisp regex to PCRE |
| `SPC r x` | rxt-explain-pcre | Explain a PCRE regex |
| `SPC r r` | pcre-query-replace-regexp | Search-replace with PCRE syntax |

#### AI / Claude Code (SPC a)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC a n` | claude-code-ide | New Claude session |
| `SPC a r` | claude-code-ide-continue | Continue last session |
| `SPC a R` | claude-code-ide-resume | Resume previous conversation |
| `SPC a q` | claude-code-ide-stop | Stop session |
| `SPC a c` | claude-code-ide-toggle-recent | Toggle Claude window |
| `SPC a s` | claude-code-ide-insert-at-mentioned | Send selection to Claude |
| `SPC a p` | claude-code-ide-send-prompt | Send prompt to Claude |
| `SPC a @` | my/claude-code-send-file-ref | Send file reference |
| `SPC a i` | my/claude-code-instruct-region | Send region + instruction |
| `SPC a f` | my/claude-code-fix-errors | Fix errors in current file |
| `SPC a u` | my/claude-code-undo | Undo Claude's last change |
| `SPC a x` | my/claude-code-clear | Clear conversation |
| `SPC a K` | my/claude-code-compact | Compact context |

#### Other
| Key | Command | Description |
|-----|---------|-------------|
| `g~` | string-inflection-all-cycle | Cycle case style (normal mode) |
| `C-;` | embark-act | Context actions on any candidate |
| `C-.` | embark-dwim | Default action for thing at point |
| `C-c C-t` | vterm-copy-mode | Toggle scrollback in vterm |
| `C-c C-c` | (in vterm) | Send Ctrl-C interrupt to terminal |

### Clojure Localleader (, prefix)

#### Eval (, e)
| Key | Command | Description |
|-----|---------|-------------|
| `, e e` | cider-eval-last-sexp | Eval last sexp |
| `, e f` | cider-eval-defun-at-point | Eval defun |
| `, e b` | cider-eval-buffer | Eval buffer |
| `, e r` | cider-eval-region | Eval region |
| `, e ;` | cider-eval-defun-to-comment | Eval defun to comment |
| `, e p e` | cider-pprint-eval-last-sexp | Pretty-print last sexp |
| `, e p ;` | cider-pprint-eval-defun-to-comment | Pretty-print defun to comment |

#### REPL (, r)
| Key | Command | Description |
|-----|---------|-------------|
| `, r j` | cider-jack-in-clj | Start REPL |
| `, r J` | cider-jack-in-cljs | Start ClojureScript REPL |
| `, r c` | cider-connect-clj | Connect to running REPL |
| `, r q` | cider-quit | Quit REPL |
| `, r s` | cider-switch-to-repl-buffer | Switch to REPL |
| `, r n` | cider-ns-refresh | Refresh namespace |

#### Test (, t)
| Key | Command | Description |
|-----|---------|-------------|
| `, t t` | cider-test-run-test | Test at point |
| `, t n` | cider-test-run-ns-tests | Test namespace |
| `, t a` | cider-test-run-project-tests | Test all |
| `, t r` | cider-test-rerun-failed-tests | Rerun failed |

#### Format (, =)
| Key | Command | Description |
|-----|---------|-------------|
| `, = =` | cider-format-buffer | Format buffer |
| `, = f` | cider-format-defun | Format defun |
| `, = l` | clojure-align | Align forms |
| `, = e b` | cider-format-edn-buffer | Format EDN buffer |

#### Debug (, d)
| Key | Command | Description |
|-----|---------|-------------|
| `, d d` | cider-debug-defun-at-point | Debug defun |
| `, d b` | cider-debug-toggle-break | Toggle breakpoint |
| `, d i` | cider-inspect-last-result | Inspect value |

#### Help (, h)
| Key | Command | Description |
|-----|---------|-------------|
| `, h d` | cider-doc | Show doc |
| `, h j` | cider-javadoc | Javadoc |
| `, h c` | cider-clojuredocs | Clojuredocs |
| `, h a` | cider-apropos | Apropos |

### Go Localleader (, prefix)

Doom's Go module provides most bindings. Our config extends with navigate (`, v`) and gopls tuning.

#### Tags & Playground
| Key | Command | Description |
|-----|---------|-------------|
| `, a` | go-tag-add | Add struct tags (json, yaml, etc.) |
| `, d` | go-tag-remove | Remove struct tags |
| `, e` | +go/play-buffer-or-region | Send to Go playground |
| `, i` | go-goto-imports | Jump to imports |

#### Build (, b)
| Key | Command | Description |
|-----|---------|-------------|
| `, b r` | go run . | Run current package |
| `, b b` | go build | Build |
| `, b c` | go clean | Clean |

#### Test (, t)
| Key | Command | Description |
|-----|---------|-------------|
| `, t t` | +go/test-rerun | Rerun last test |
| `, t a` | +go/test-all | Test all |
| `, t s` | +go/test-single | Test at point |
| `, t n` | +go/test-nested | Test nested |
| `, t f` | +go/test-file | Test file |
| `, t g` | go-gen-test-dwim | Generate test for function |
| `, t G` | go-gen-test-all | Generate tests for all functions |
| `, t b s` | +go/bench-single | Benchmark at point |
| `, t b a` | +go/bench-all | Benchmark all |

#### Generate (, g)
| Key | Command | Description |
|-----|---------|-------------|
| `, g f` | +go/generate-file | go generate (file) |
| `, g d` | +go/generate-dir | go generate (directory) |
| `, g a` | +go/generate-all | go generate (all) |

#### Navigate (, v) (custom)
| Key | Command | Description |
|-----|---------|-------------|
| `, v f` | go-goto-function | Jump to function keyword |
| `, v a` | go-goto-arguments | Jump to arguments |
| `, v r` | go-goto-return-values | Jump to return values |
| `, v m` | go-goto-method-receiver | Jump to method receiver |
| `, v i` | go-goto-imports | Jump to imports |
| `, v d` | go-goto-function-name | Jump to function name/docstring |

#### Help (, h)
| Key | Command | Description |
|-----|---------|-------------|
| `, h .` | godoc-at-point | Lookup in godoc |

#### Also available
| Key | Command | Description |
|-----|---------|-------------|
| `SPC p t` | projectile-toggle-implementation-and-test | Switch foo.go ↔ foo_test.go |
| `SPC c a` | lsp-execute-code-action | GoFillStruct, GoImpl, etc. (via gopls) |

## Installed Packages

### Doom Modules (via init.el)

#### Completion
| Module | What it does |
|--------|-------------|
| corfu (+orderless +dabbrev) | As-you-type popup suggestions. Orderless lets you type words in any order to match ("buf swi" finds "switch-to-buffer"). Dabbrev adds words from other open buffers as fallback suggestions. |
| vertico | Vertical list when you press `M-x` or search for files. Bundles consult (preview search results), embark (right-click actions on any item), marginalia (shows descriptions next to items), and cape (extra completion sources). |

#### UI
| Module | What it does |
|--------|-------------|
| doom | The Doom look — themes, fonts, and overall styling |
| doom-dashboard | The splash screen you see when Emacs starts |
| hl-todo | Highlights `TODO`, `FIXME`, `HACK`, `NOTE` in code comments with colors |
| modeline | The status bar at the bottom showing file name, git branch, line number |
| nav-flash | Briefly flashes the cursor line after a big jump so you don't lose your place |
| ophints | Highlights the region you just operated on (e.g., after yanking or deleting) |
| popup (+defaults) | Controls how temporary windows appear (REPL, test results, help) |
| vc-gutter (+pretty) | Shows colored marks in the left margin for added/changed/deleted lines (git diff) |
| vi-tilde-fringe | Shows `~` characters after the end of file, like Vim |
| workspaces | Tab-like workspaces — each project gets its own set of buffers |

#### Editor
| Module | What it does |
|--------|-------------|
| evil (+everywhere) | Vim keybindings everywhere — `hjkl` movement, modal editing, visual mode |
| file-templates | Auto-inserts boilerplate when you create a new file (e.g., namespace declaration for `.clj`) |
| fold | Code folding — collapse/expand functions and blocks with `za` |
| multiple-cursors | Edit multiple places at once — select a word, press a key, all matching words get cursors |
| snippets | Type a short trigger (e.g., `defn`) and expand it into a full code template |
| whitespace (+guess +trim) | Auto-detects indent style and trims trailing whitespace on save |

#### Emacs
| Module | What it does |
|--------|-------------|
| dired (+icons) | Built-in file manager with file type icons |
| electric | Smart auto-indentation as you type |
| tramp | Edit files on remote servers over SSH as if they were local |
| undo | Persistent undo history — survives closing and reopening files |
| vc | Version control integration (git status, diff, log) |

#### Terminal
| Module | What it does |
|--------|-------------|
| vterm | Full terminal emulator inside Emacs — runs shell, Claude Code, any CLI tool |

#### Checkers
| Module | What it does |
|--------|-------------|
| syntax | Real-time error checking — red squiggles and warnings as you type (via flycheck) |

#### Tools
| Module | What it does |
|--------|-------------|
| direnv | Auto-loads environment variables when you enter a project directory (`.envrc` files) |
| editorconfig | Reads `.editorconfig` files to set indent size, line endings per project |
| eval (+overlay) | Run code and show results inline — evaluate expressions without leaving the buffer |
| lookup | Jump to definition (`gd`), find references, look up documentation |
| lsp | Language Server Protocol — powers go-to-definition, autocomplete, rename, error checking for all languages |
| tree-sitter | Better syntax highlighting using an actual code parser instead of regex |
| magit | The best Git interface — stage, commit, push, merge, rebase, all from inside Emacs |

#### Languages
| Module | What it does |
|--------|-------------|
| clojure (+lsp) | Clojure/ClojureScript support with LSP-powered navigation (works without a REPL running) |
| go (+lsp +tree-sitter) | Go support with gopls LSP server and tree-sitter parsing |
| emacs-lisp | Support for editing Doom config and Emacs packages |
| markdown | Markdown preview, formatting, and table editing |
| org | Org-mode for notes, planning, and documents |
| sh | Shell script editing and syntax checking |
| data | JSON, YAML, TOML, CSV file support |

### Custom Packages (via packages.el)

| Package | What it does | Key binding |
|---------|-------------|-------------|
| lispyville | Makes Vim motions aware of parentheses — `dd` won't break your Clojure code, slurp/barf move parens | Automatic in Clojure/Elisp buffers |
| claude-code-ide | Connects Claude Code CLI to Emacs — Claude can open files, show diffs, read errors, and use your REPL | `SPC a` prefix |
| nerd-icons-corfu | Shows small icons next to completion suggestions (function, variable, class, module) | Automatic |
| consult-lsp | Search all symbols or errors across your entire project with live preview | `SPC s S` / `SPC s D` |
| ace-window | Numbers appear on each window, press the number to jump there instantly | `SPC w w` |
| string-inflection | Convert between naming styles: `my-name` to `myName` to `MyName` to `my_name` | `SPC c s` prefix / `g~` |
| git-timemachine | Step through old versions of a file — press `p`/`n` to go back/forward in history | `SPC g t` |
| git-link | Copies a GitHub URL for the exact line you're on — paste it in Slack or a PR | `SPC g y` / `SPC g O` |
| devdocs | Read Go stdlib docs, Clojure docs, etc. inside Emacs without opening a browser | `SPC h D` / `SPC h B` |
| pcre2el | Translate between regex flavors — paste a Java regex, get the Emacs equivalent | `SPC r` prefix |

### Claude Code MCP Tools (inside Emacs sessions)

These tools are available to Claude when running inside Emacs (SPC a n):

| Tool | Description |
|------|-------------|
| emacs_navigate | Open files in splits, jump to lines |
| emacs_window | Manage window layout (split, close, balance) |
| emacs_repl_eval | Evaluate Clojure in connected CIDER REPL |
| emacs_repl_status | Check REPL connection status |
| emacs_repl_load_file | Load/compile a file into the REPL |
| getDiagnostics | Read Flycheck/Flymake errors |
| openDiff | Show ediff for proposed changes |
| xref-find-references | Query LSP for symbol usages |
| xref-find-apropos | Search symbols by pattern |
| imenu-list-symbols | List all symbols in a file |
| project-info | Get project directory and file count |
| treesit-info | Read tree-sitter AST |

See `docs/claude-code-emacs-tools-guide.md` for full tool documentation.

## Setup

### Prerequisites

```sh
# Fonts (required)
brew install --cask font-sauce-code-pro-nerd-font
brew install --cask font-iosevka-nerd-font   # Symbol fallback

# Clojure
brew install clojure/tools/clojure leiningen
brew install clojure-lsp/brew/clojure-lsp-native

# Go
brew install go gopls

# Formatters
brew install cljstyle   # Clojure formatter
go install golang.org/x/tools/cmd/goimports@latest

# DevDocs (first run)
# M-x devdocs-install, then select "go" and "clojure~1.11"
```

### First Run

```sh
doom sync        # Install packages
doom doctor      # Check for issues
emacs --daemon   # Start daemon
emacsclient -c   # Connect client
```

### Environment

`LSP_USE_PLISTS=true` must be set in your shell profile (`.zshrc`). lsp-mode reads this at compile time for plist-based performance optimization.

## Doom Commands

```sh
doom sync          # Sync packages and rebuild
doom upgrade       # Upgrade Doom and packages
doom doctor        # Diagnose configuration issues
doom env           # Rebuild environment variables cache
doom build         # Rebuild packages
doom purge         # Remove orphaned packages
```
