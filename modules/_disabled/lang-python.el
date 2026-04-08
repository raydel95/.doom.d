;;; modules/_disabled/lang-python.el -*- lexical-binding: t; -*-
;;
;; Python configuration — NOT LOADED
;; To enable: uncomment (python +lsp) in init.el and add
;; (load! "modules/_disabled/lang-python") to config.el

;; ============================================================================
;; PYTHON MODE
;; ============================================================================

(after! python
  (setq python-indent-offset 4
        python-indent-guess-indent-offset-verbose nil)

  ;; Formatter: ruff (fast, 99% black-compatible) or black (stable, conservative)
  (setq-hook! 'python-mode-hook +format-with 'ruff)
  ;; Alternative: (setq-hook! 'python-mode-hook +format-with 'black)
  )

;; ============================================================================
;; LSP (Pyright)
;; ============================================================================

(after! lsp-pyright
  (setq lsp-pyright-python-executable-cmd "python3"
        lsp-pyright-multi-root nil))

;; ============================================================================
;; TESTING (pytest)
;; ============================================================================

(after! python-pytest
  (setq python-pytest-arguments
        '("--color" "--failed-first" "--maxfail=5" "-v")))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(map! :after python
      :localleader
      :map python-mode-map
      (:prefix ("t" . "test")
       :desc "Run all tests"      "a" #'python-pytest
       :desc "Run test file"      "f" #'python-pytest-file
       :desc "Run test function"  "t" #'python-pytest-function
       :desc "Repeat last test"   "r" #'python-pytest-repeat))

;;; modules/_disabled/lang-python.el ends here
