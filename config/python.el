;;; config/python.el -*- lexical-binding: t; -*-
;;
;; Python development configuration

;; ====================================
;; PYTHON MODE
;; ====================================

(after! python
  ;; Set Python interpreter (use pyenv or virtualenv)
  ;; (setq python-shell-interpreter "python3")

  ;; Indentation
  (setq python-indent-offset 4
        python-indent-guess-indent-offset-verbose nil)

  ;; Formatting
  ;; Options: black, yapf, autopep8
  (setq-hook! 'python-mode-hook +format-with 'black))

;; ====================================
;; LSP PYTHON SETTINGS
;; ====================================

(after! lsp-pyright
  ;; Pyright (default Python LSP in Doom)
  (setq lsp-pyright-python-executable-cmd "python3"
        lsp-pyright-multi-root nil))

;; Alternative: Use pylsp instead
;; (after! lsp-pylsp
;;   (setq lsp-pylsp-plugins-flake8-enabled t
;;         lsp-pylsp-plugins-pylint-enabled nil))

;; ====================================
;; PYTHON TESTING
;; ====================================

(after! python-pytest
  ;; Configure pytest if you use it
  (setq python-pytest-arguments
        '("--color"          ; colored output
          "--failed-first"   ; run failed tests first
          "--maxfail=5"      ; stop after 5 failures
          "-v")))            ; verbose

;; ====================================
;; VIRTUAL ENVIRONMENTS
;; ====================================

;; Pyvenv is included with the python module
;; Use: M-x pyvenv-activate to activate a virtualenv

;; If you use pipenv:
;; (after! pipenv
;;   (setq pipenv-projectile-after-switch-function
;;         #'pipenv-projectile-after-switch-extended))

;; ====================================
;; PYTHON KEYBINDINGS
;; ====================================

(map! :after python
      :localleader
      :map python-mode-map
      ;; Testing
      (:prefix ("t" . "test")
       :desc "Run all tests"      "a" #'python-pytest
       :desc "Run test file"      "f" #'python-pytest-file
       :desc "Run test function"  "t" #'python-pytest-function
       :desc "Repeat last test"   "r" #'python-pytest-repeat))

;; ====================================
;; PYTHON TOOLS
;; ====================================

;; Install these tools for best experience:
;; pip install black         # Formatter
;; pip install pyright       # LSP server
;; pip install pytest        # Testing
;; pip install ipython       # Better REPL

;; For data science:
;; pip install jupyter
;; pip install pandas numpy matplotlib
