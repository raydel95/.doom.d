;;; packages.el -*- no-bytecomp: t; -*-
;;
;; Custom packages for optimized Doom Emacs

;; Lispyville: Vim-style Lisp editing
(package! lispyville)

;; Claude Code IDE: MCP-based bidirectional bridge to Claude Code CLI
(package! claude-code-ide :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

;; Completion enhancements
(package! nerd-icons-corfu)   ; Icons in corfu popup (function/variable/class)
(package! consult-lsp)        ; consult-lsp-symbols, consult-lsp-diagnostics
