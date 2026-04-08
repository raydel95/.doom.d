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

;; Navigation & editing
(package! ace-window)         ; Number-based window jumping
(package! string-inflection)  ; Cycle between case styles (kebab, camel, pascal, snake)

;; Git enhancements
(package! git-timemachine)    ; Step through file history in-place
(package! git-link)           ; Generate GitHub URLs for current file+line

;; Documentation & development
(package! devdocs)            ; Browse DevDocs.io inside Emacs (Go stdlib, etc.)
(package! pcre2el)            ; Convert between PCRE, Elisp, and rx regex syntaxes
