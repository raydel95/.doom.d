;;; packages.el -*- no-bytecomp: t; -*-
;;
;; Custom packages for optimized Doom Emacs

;; Lispyville: Vim-style Lisp editing
;; Provides Evil keybindings for structural Lisp editing
(package! lispyville)

;; Claude Code IDE: MCP-based bidirectional bridge to Claude Code CLI
;; https://github.com/manzaltu/claude-code-ide.el
(package! claude-code-ide :recipe (:host github :repo "manzaltu/claude-code-ide.el"))
