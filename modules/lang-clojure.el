;;; modules/lang-clojure.el -*- lexical-binding: t; -*-
;;
;; Clojure/ClojureScript: clojure-mode, CIDER, lispyville, cljstyle

;; ============================================================================
;; CLOJURE-MODE
;; ============================================================================

(after! clojure-mode
  (setq clojure-indent-style 'align-arguments
        clojure-thread-all-but-last t
        clojure-align-forms-automatically t))

;; ============================================================================
;; CIDER
;; ============================================================================

(after! cider
  (setq cider-ns-refresh-show-log-buffer t
        cider-show-error-buffer t
        cider-prompt-for-symbol nil
        cider-font-lock-dynamically t     ; Safe with LSP-first workflow

        ;; REPL behavior
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";; => "
        cider-repl-wrap-history t
        cider-repl-history-size 3000

        ;; Evaluation
        cider-auto-select-error-buffer t
        cider-show-eval-spinner nil
        cider-invert-insert-eval-p t

        ;; Testing
        cider-test-show-report-on-success t
        cider-auto-test-mode nil

        ;; Inspector
        cider-inspector-fill-frame t

        ;; Stacktraces
        cider-stacktrace-fill-column 80

        ;; Daemon mode: reuse dead REPLs instead of prompting
        cider-reuse-dead-repls 'auto)

  ;; Popup rules
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil :ttl nil :size 0.3)
  (set-popup-rule! "^\\*cider-error" :side 'bottom :quit t :size 0.3)
  (set-popup-rule! "^\\*cider-doc" :side 'right :quit t :size 0.5)

  ;; Prefer LSP for navigation (works without REPL)
  (set-lookup-handlers! 'cider-mode nil)

  ;; Re-enable cider-mode after REPL disconnect.
  ;; When a REPL dies, cider-mode can be stripped from Clojure buffers,
  ;; removing all CIDER keybindings. This hook re-enables it.
  ;; See: https://github.com/clojure-emacs/cider/issues/3346
  (add-hook 'cider-disconnected-hook
            (defun my/cider-reenable-mode-after-disconnect ()
              "Re-enable cider-mode in Clojure buffers after REPL disconnect."
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when (and (derived-mode-p 'clojure-mode 'clojurescript-mode 'clojure-ts-mode)
                             (not cider-mode))
                    (cider-mode +1)))))))

;; ============================================================================
;; LISPYVILLE (Vim-style structural editing)
;; ============================================================================

;; Unmaintained since July 2022 but functional and without a clear replacement.
(use-package! lispyville
  :hook ((clojure-mode       . lispyville-mode)
         (emacs-lisp-mode    . lispyville-mode)
         (common-lisp-mode   . lispyville-mode)
         (scheme-mode        . lispyville-mode))
  :config
  (lispyville-set-key-theme
   '(additional
     additional-insert
     (additional-movement normal visual motion)
     (additional-wrap normal insert)
     (atom-movement normal visual)
     c-w
     c-u
     (commentary normal visual)
     escape
     (operators normal)
     (prettify insert)
     slurp/barf-cp)))

;; ============================================================================
;; FORMATTING (cljstyle)
;; ============================================================================

;; Install: brew install cljstyle
;; Config: .cljstyle in project root or ~/.config/cljstyle/config.edn
(set-formatter! 'cljstyle "cljstyle pipe" :modes '(clojure-mode clojurescript-mode))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

(map! :map (clojure-mode-map clojurescript-mode-map)
      :localleader
      ;; Eval
      (:prefix ("e" . "eval")
       :desc "Eval last sexp"           "e" #'cider-eval-last-sexp
       :desc "Eval defun"               "f" #'cider-eval-defun-at-point
       :desc "Eval buffer"              "b" #'cider-eval-buffer
       :desc "Eval region"              "r" #'cider-eval-region
       :desc "Eval defun to comment"    ";" #'cider-eval-defun-to-comment
       :desc "Eval sexp to comment"     ":" #'cider-eval-last-sexp-to-comment
       (:prefix ("p" . "pprint")
        :desc "Pprint sexp to comment"  ":" #'cider-pprint-eval-last-sexp-to-comment
        :desc "Pprint defun to comment" ";" #'cider-pprint-eval-defun-to-comment
        :desc "Pprint defun at point"   "d" #'cider-pprint-eval-defun-at-point
        :desc "Pprint last sexp"        "e" #'cider-pprint-eval-last-sexp))

      ;; REPL
      (:prefix ("r" . "repl")
       :desc "Jack in"                  "j" #'cider-jack-in-clj
       :desc "Jack in ClojureScript"    "J" #'cider-jack-in-cljs
       :desc "Connect"                  "c" #'cider-connect-clj
       :desc "Quit REPL"               "q" #'cider-quit
       :desc "Clear REPL"              "l" #'cider-repl-clear-buffer
       :desc "Switch to REPL"          "s" #'cider-switch-to-repl-buffer
       :desc "Refresh namespace"        "n" #'cider-ns-refresh)

      ;; Testing
      (:prefix ("t" . "test")
       :desc "Run test at point"        "t" #'cider-test-run-test
       :desc "Run namespace tests"      "n" #'cider-test-run-ns-tests
       :desc "Run all tests"            "a" #'cider-test-run-project-tests
       :desc "Rerun failed tests"       "r" #'cider-test-rerun-failed-tests
       :desc "Show test report"         "l" #'cider-test-show-report)

      ;; Debug
      (:prefix ("d" . "debug")
       :desc "Debug defun"              "d" #'cider-debug-defun-at-point
       :desc "Toggle breakpoint"        "b" #'cider-debug-toggle-break
       :desc "Inspect value"            "i" #'cider-inspect-last-result)

      ;; Format
      (:prefix ("=" . "format")
       :desc "Format buffer"            "=" #'cider-format-buffer
       :desc "Format defun"             "f" #'cider-format-defun
       :desc "Align forms"              "l" #'clojure-align
       :desc "Format region"            "r" #'cider-format-region
       (:prefix ("e" . "edn")
        :desc "Format EDN buffer"       "b" #'cider-format-edn-buffer
        :desc "Format EDN last sexp"    "a" #'cider-format-edn-last-sexp
        :desc "Format EDN region"       "r" #'cider-format-edn-region))

      ;; Namespace
      (:prefix ("n" . "namespace")
       :desc "Eval ns form"             "n" #'cider-eval-ns-form
       :desc "Refresh namespace"        "r" #'cider-ns-refresh
       :desc "Load file"                "l" #'cider-load-file)

      ;; Documentation
      (:prefix ("h" . "help")
       :desc "Show doc"                 "d" #'cider-doc
       :desc "Javadoc"                  "j" #'cider-javadoc
       :desc "Clojuredocs"              "c" #'cider-clojuredocs
       :desc "Apropos"                  "a" #'cider-apropos))

;;; modules/lang-clojure.el ends here
