;;; modules/lang-go.el -*- lexical-binding: t; -*-
;;
;; Go development: go-mode, gopls, tree-sitter

;; ============================================================================
;; GO-MODE
;; ============================================================================

(after! go-mode
  ;; Format on save with goimports (gofmt + import management)
  (setq gofmt-command "goimports")

  (defun my/go-mode-setup-h ()
    "Set buffer-local tab-width and enable goimports on save."
    (setq-local tab-width 4)
    (add-hook 'before-save-hook #'gofmt-before-save nil 'local))
  (add-hook 'go-mode-hook #'my/go-mode-setup-h))

;; ============================================================================
;; GOPLS (LSP)
;; ============================================================================

(after! lsp-go
  (setq lsp-go-gopls-server-path "gopls"

        ;; Analyses — updated for gopls 0.14+ (2025)
        lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (useany . t)
                          (nilness . t)        ; Detect nil pointer issues
                          (ineffassign . t)    ; Detect ineffectual assignments
                          (tests . t))         ; Check test files

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

        ;; Inlay hints
        lsp-go-hover-kind "FullDocumentation"
        lsp-inlay-hint-enable t))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(map! :map go-mode-map
      :localleader
      (:prefix ("i" . "imports")
       :desc "Add import"       "a" #'go-import-add
       :desc "Remove unused"    "d" #'go-remove-unused-imports
       :desc "Goto imports"     "g" #'go-goto-imports)

      (:prefix ("t" . "test")
       :desc "Test function"    "t" #'go-test-current-test
       :desc "Test file"        "f" #'go-test-current-file
       :desc "Test project"     "p" #'go-test-current-project
       :desc "Run tests"        "r" #'go-run)

      (:prefix ("g" . "goto")
       :desc "Goto imports"     "i" #'go-goto-imports
       :desc "Goto method receiver" "m" #'go-goto-method-receiver
       :desc "Goto function"    "f" #'go-goto-function
       :desc "Goto arguments"   "a" #'go-goto-arguments
       :desc "Goto return values" "r" #'go-goto-return-values))

;;; modules/lang-go.el ends here
