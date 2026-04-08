;;; modules/_disabled/lang-javascript.el -*- lexical-binding: t; -*-
;;
;; JavaScript/TypeScript configuration — NOT LOADED
;; To enable: uncomment (javascript +lsp) in init.el and add
;; (load! "modules/_disabled/lang-javascript") to config.el
;;
;; Updated for 2026: tree-sitter modes replace js2-mode

;; ============================================================================
;; JAVASCRIPT / TYPESCRIPT (tree-sitter)
;; ============================================================================

;; Doom's (javascript +lsp) now defaults to js-ts-mode and typescript-ts-mode
;; when tree-sitter is available (Emacs 29+).

(after! js-ts-mode
  (setq js-indent-level 2))

(after! typescript-ts-mode
  (setq typescript-indent-level 2))

;; ============================================================================
;; FORMATTING
;; ============================================================================

;; Prettier remains the standard JS/TS formatter.
;; Install: npm install -g prettier
(setq-hook! 'js-ts-mode-hook +format-with 'prettier)
(setq-hook! 'typescript-ts-mode-hook +format-with 'prettier)

;; Alternative: Biome (Rust-based, faster) for new projects
;; Install: npm install -g @biomejs/biome
;; (setq-hook! 'js-ts-mode-hook +format-with 'biome)

;;; modules/_disabled/lang-javascript.el ends here
