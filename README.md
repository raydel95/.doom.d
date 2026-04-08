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
| `SPC g B` | magit-blame | Git blame |

#### Code (SPC c)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC c ~` | string-inflection-all-cycle | Cycle case: kebab/camel/pascal/snake |
| `SPC c d` | lsp-find-definition | Go to definition |
| `SPC c r` | lsp-find-references | Find references |
| `SPC c a` | lsp-execute-code-action | Code actions |
| `SPC c R` | lsp-rename | Rename symbol |

#### Help (SPC h)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC h D` | devdocs-lookup | Browse DevDocs (Go stdlib, Clojure, etc.) |
| `SPC h d` | describe-function | Describe function |
| `SPC h k` | describe-key | Describe key |

#### Regex (SPC r)
| Key | Command | Description |
|-----|---------|-------------|
| `SPC r p` | rxt-pcre-to-elisp | Convert PCRE (Java/Clojure) to Elisp regex |
| `SPC r e` | rxt-elisp-to-pcre | Convert Elisp regex to PCRE |
| `SPC r x` | rxt-explain-pcre | Explain a PCRE regex |

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
| `C-c C-t` | vterm-copy-mode | Toggle scrollback in vterm |

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

#### Imports (, i)
| Key | Command | Description |
|-----|---------|-------------|
| `, i a` | go-import-add | Add import |
| `, i d` | go-remove-unused-imports | Remove unused |

#### Test (, t)
| Key | Command | Description |
|-----|---------|-------------|
| `, t t` | go-test-current-test | Test function |
| `, t f` | go-test-current-file | Test file |
| `, t p` | go-test-current-project | Test project |

## Installed Packages

### Doom Modules (via init.el)

**Completion**: corfu (+orderless +dabbrev), vertico (includes consult, embark, marginalia, cape)

**UI**: doom, doom-dashboard, hl-todo, modeline, nav-flash, ophints, popup, vc-gutter, vi-tilde-fringe, workspaces

**Editor**: evil (+everywhere), file-templates, fold, multiple-cursors, snippets, whitespace (+guess +trim)

**Emacs**: dired (+icons), electric, tramp, undo, vc

**Terminal**: vterm

**Checkers**: syntax (flycheck)

**Tools**: direnv, editorconfig, eval (+overlay), lookup, lsp, tree-sitter, magit

**OS**: macos

**Languages**: clojure (+lsp), go (+lsp +tree-sitter), emacs-lisp, markdown, org, sh, data

### Custom Packages (via packages.el)

| Package | Purpose | Key binding |
|---------|---------|-------------|
| lispyville | Vim-style structural Lisp editing | Automatic in Clojure/Elisp buffers |
| claude-code-ide | MCP bridge to Claude Code CLI | `SPC a` prefix |
| nerd-icons-corfu | Type icons in completion popup | Automatic |
| consult-lsp | LSP workspace symbol search | `SPC s S` / `SPC s D` |
| ace-window | Number-based window jumping | `SPC w w` |
| string-inflection | Case style cycling (kebab/camel/pascal/snake) | `SPC c ~` / `g~` |
| git-timemachine | Browse file history in-place | `SPC g t` |
| git-link | Copy GitHub URL for current file+line | `SPC g y` |
| devdocs | Browse DevDocs.io inside Emacs | `SPC h D` |
| pcre2el | Regex syntax conversion (PCRE/Elisp/rx) | `SPC r` prefix |

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
