# Doom Emacs Configuration

This is a modular, well-organized Doom Emacs configuration optimized for performance and maintainability.

## 📁 Structure

```
~/.config/doom/
├── init.el              # Doom modules configuration
├── config.el            # Main config (loads sub-modules)
├── packages.el          # Package declarations
├── custom.el            # Emacs custom-set-variables (auto-generated)
└── config/              # Modular configuration directory
    ├── performance.el   # Performance optimizations
    ├── ui.el           # UI and appearance settings
    ├── keybindings.el  # Custom keybindings
    ├── lsp.el          # LSP configuration
    └── clojure.el      # Clojure-specific settings
```

## 🚀 Key Features

### Modern Stack
- **Completion**: Corfu + Vertico (modern, fast)
- **LSP**: Full Language Server Protocol support
- **Tree-sitter**: Modern syntax highlighting
- **Format on save**: Auto-formatting enabled

### Performance Optimizations
- 100MB GC threshold (reduced GC pauses)
- 3MB subprocess read buffer (better LSP performance)
- Font cache optimization
- Bidirectional text disabled
- Optimized LSP settings

### Clojure Development
- Full CIDER + LSP integration
- Lispyville for structural editing
- Custom keybindings under `,` (localleader)
- cljstyle formatter integration
- Optimized for REPL-driven development

## 📦 Enabled Modules

### Completion & UI
- `corfu` + `orderless` - Modern completion
- `vertico` - Fast minibuffer completion
- `doom-dashboard` - Nice startup screen
- `modeline` - Clean status line
- `workspaces` - Tab-like workspace management

### Editor Features
- `evil` - Vim emulation
- `format +onsave` - Auto-format on save
- `multiple-cursors` - Edit multiple locations
- `snippets` - Code templates
- `fold` - Code folding

### Tools
- `lsp` - Language Server Protocol
- `magit` - Git porcelain
- `direnv` - Directory environment
- `docker` - Docker integration
- `tree-sitter` - Advanced syntax highlighting
- `llm` - AI/LLM integration

### Languages (Focused on Your Stack)
**Primary:**
- Clojure (+lsp) - Full CIDER + LSP integration

**Occasional:**
- Python (+lsp) - With black formatter
- JavaScript/TypeScript (+lsp) - With prettier formatter

**Utilities:**
- Shell scripts
- Markdown
- Org mode
- Emacs Lisp
- Data formats (JSON, YAML, TOML)

## 🎨 Customization

### Adding New Language Support

1. Enable module in `init.el`:
   ```elisp
   (rust +lsp)  ; Add to :lang section
   ```

2. Create config file (optional):
   ```bash
   touch config/rust.el
   ```

3. Load in `config.el`:
   ```elisp
   (load! "config/rust")
   ```

4. Sync:
   ```bash
   doom sync
   ```

### Adding New Packages

1. Add to `packages.el`:
   ```elisp
   (package! package-name)
   ```

2. Configure in appropriate `config/*.el` file

3. Sync:
   ```bash
   doom sync
   ```

## ⚙️ Important Commands

```bash
doom sync          # Sync packages and rebuild
doom upgrade       # Upgrade Doom and packages
doom doctor        # Diagnose configuration issues
doom build         # Rebuild packages
doom purge         # Remove orphaned packages
doom env           # Rebuild environment variables
```

## 🔧 Keybindings

### Global Leader: `SPC`
- `SPC f f` - Find file
- `SPC f r` - Recent files
- `SPC p p` - Switch project
- `SPC b b` - Switch buffer
- `SPC w w` - Switch window

### Localleader: `,` (for Clojure)
- `, e ;` - Eval defun to comment
- `, = =` - Format buffer
- `, = f` - Format defun
- `, e p :` - Pretty print last sexp to comment

### Custom Apps: `SPC a`
- `SPC a p` - List processes

## 📝 Notes

### Performance
- Line numbers are enabled (set to `nil` or `'relative` for better performance)
- LSP UI doc is disabled by default (toggle with `SPC t d`)
- Auto-format runs on save (disable per-mode if needed)

### Tree-sitter
Now enabled! Provides better syntax highlighting and code understanding.
Will download grammars on first use.

### Format on Save
Enabled globally. To disable for specific modes:
```elisp
(add-hook 'some-mode-hook
          (lambda () (format-all-mode -1)))
```

## 🐛 Troubleshooting

### Config not loading?
```bash
doom sync
doom doctor
```

### Slow startup?
1. Check `doom doctor` output
2. Disable modules one by one in `init.el`
3. Check `config/performance.el` settings

### LSP issues?
1. Check if language server is installed
2. Run `M-x lsp-doctor`
3. Check `config/lsp.el` for server-specific settings

## 📚 Resources

- [Doom Emacs Documentation](https://docs.doomemacs.org)
- [Doom Emacs Discourse](https://discourse.doomemacs.org)
- [Doom Emacs GitHub](https://github.com/doomemacs/doomemacs)

## 🔄 Migration from Old Config

The old monolithic `config.el` has been split into modular files:
- Performance settings → `config/performance.el`
- UI settings → `config/ui.el`
- Keybindings → `config/keybindings.el`
- LSP config → `config/lsp.el`
- Clojure config → `config/clojure.el`

All functionality is preserved and improved!
