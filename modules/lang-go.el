;;; modules/lang-go.el -*- lexical-binding: t; -*-
;;
;; Go development: gopls tuning + keybinding extensions
;;
;; Doom's Go module already provides:
;;   , a     go-tag-add           , d     go-tag-remove
;;   , e     go playground        , i     go-goto-imports
;;   , h .   godoc-at-point       , ri a  go-import-add
;;   , b r/b/c  build/run/clean  , g f/d/a  go generate
;;   , t t/a/s/n/f  tests        , t g/G/e  go-gen-test
;;   , t b s/a  benchmarks
;;
;; This file EXTENDS those defaults — it does not override them.

;; ============================================================================
;; GO-MODE (format on save)
;; ============================================================================

(after! go-mode
  ;; goimports = gofmt + auto-manage imports
  (setq gofmt-command "goimports")

  (defun my/go-mode-setup-h ()
    "Buffer-local tab-width and format-on-save."
    (setq-local tab-width 4)
    (add-hook 'before-save-hook #'gofmt-before-save nil 'local))
  (add-hook 'go-mode-hook #'my/go-mode-setup-h))

;; ============================================================================
;; GOPLS (LSP)
;; ============================================================================

(after! lsp-go
  (setq lsp-go-gopls-server-path "gopls"

        ;; Analyses — extend defaults with useful off-by-default checks
        lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (unusedvariable . t)   ; Catch declared-but-unused vars
                          (useany . t)
                          (nilness . t)          ; Nil pointer issues
                          (ineffassign . t)      ; Ineffectual assignments
                          (tests . t))           ; Check test files

        ;; Codelens
        lsp-go-codelenses '((gc_details . t)
                            (generate . t)
                            (regenerate_cgo . t)
                            (test . t)
                            (tidy . t)
                            (upgrade_dependency . t)
                            (vendor . t))

        ;; Build
        lsp-go-build-flags ["-tags=integration"]

        ;; Completion
        lsp-go-use-placeholders t
        lsp-go-complete-unimported t

        ;; Hover
        lsp-go-hover-kind "FullDocumentation"

        ;; Inlay hints — show parameter names at call sites and inferred types
        lsp-inlay-hint-enable t)

  ;; Configure specific hint types (all are OFF by default in gopls)
  (lsp-register-custom-settings
   '(("gopls.hints" ((parameterNames . t)
                      (assignVariableTypes . t)
                      (compositeLiteralFields . t)
                      (constantValues . t)
                      (functionTypeParameters . t))))))

;; ============================================================================
;; KEYBINDING EXTENSIONS (additions, not replacements)
;; ============================================================================

;; Add goto commands that Doom's Go module doesn't include.
;; These use go-mode's built-in navigation to jump within a function signature.
(map! :after go-mode
      :map go-mode-map
      :localleader
      (:prefix ("v" . "navigate")
       :desc "Goto function"        "f" #'go-goto-function
       :desc "Goto arguments"       "a" #'go-goto-arguments
       :desc "Goto return values"   "r" #'go-goto-return-values
       :desc "Goto method receiver" "m" #'go-goto-method-receiver
       :desc "Goto imports"         "i" #'go-goto-imports
       :desc "Goto docstring"       "d" #'go-goto-function-name))

;; Also bind for go-ts-mode (tree-sitter variant)
(map! :after go-ts-mode
      :map go-ts-mode-map
      :localleader
      (:prefix ("v" . "navigate")
       :desc "Goto function"        "f" #'go-goto-function
       :desc "Goto arguments"       "a" #'go-goto-arguments
       :desc "Goto return values"   "r" #'go-goto-return-values
       :desc "Goto method receiver" "m" #'go-goto-method-receiver
       :desc "Goto imports"         "i" #'go-goto-imports
       :desc "Goto docstring"       "d" #'go-goto-function-name))

;;; modules/lang-go.el ends here
