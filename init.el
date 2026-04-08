;;; init.el -*- lexical-binding: t; -*-
;;
;; Optimized Doom Emacs configuration for Clojure development with LSP
;; Based on configuration analysis and performance recommendations
;; Focus: Clojure LSP + CIDER with maximum performance on M4

(doom! :input
       ;; No input methods needed

       :completion
       (corfu +orderless)       ; Modern completion with orderless matching
       vertico                  ; Vertical completion UI

       :ui
       doom                     ; Doom's aesthetic
       doom-dashboard           ; Startup screen
       hl-todo                  ; Highlight TODO/FIXME/etc
       modeline                 ; Status bar
       nav-flash                ; Blink cursor after big motions
       ophints                  ; Highlight operation regions
       (popup +defaults)        ; Popup window management
       (vc-gutter +pretty)      ; Git diff in fringe (re-enabled - minimal cost)
       vi-tilde-fringe          ; Fringe tildes for EOF
       workspaces               ; Tab emulation

       :editor
       (evil +everywhere)       ; Vim emulation
       file-templates           ; Auto-snippets for new files
       fold                     ; Code folding
       multiple-cursors         ; Multi-cursor editing
       snippets                 ; Code snippets
       (whitespace +guess +trim) ; Auto whitespace cleanup (re-enabled)

       :emacs
       (dired +icons)           ; File manager with icons
       electric                 ; Smart electric-indent
       tramp                    ; Remote file editing (re-enabled - no cost)
       undo                     ; Better undo system
       vc                       ; Version control

       :term
       vterm                    ; Best terminal emulator

       :checkers
       syntax                   ; Syntax checking

       :tools
       direnv                   ; Environment management (project-specific)
       editorconfig             ; .editorconfig support
       (eval +overlay)          ; REPL support
       lookup                   ; Code navigation
       lsp                      ; Language Server Protocol (critical for Clojure)
       tree-sitter              ; Tree-sitter parsing (required by Go +tree-sitter)
       magit                    ; Git porcelain

       :os
       (:if (featurep :system 'macos) macos) ; macOS integration

       :lang
       ;; PRIMARY: Clojure with LSP for navigation without REPL
       (clojure +lsp)           ; Clojure/Script with clojure-lsp (enables go-to-def, find-refs without REPL)

       ;; Go development with LSP
       (go +lsp +tree-sitter)   ; Go with gopls LSP server and tree-sitter parsing

       ;; UTILITY LANGUAGES
       emacs-lisp               ; Emacs configuration
       markdown                 ; Documentation
       org                      ; Notes and planning
       sh                       ; Shell scripts
       data                     ; JSON, YAML, TOML, CSV

       ;; OPTIONAL: Uncomment if needed
       ;;(python +lsp)          ; Python development
       ;;(javascript +lsp)      ; JavaScript/TypeScript

       :config
       (default +bindings +smartparens))
