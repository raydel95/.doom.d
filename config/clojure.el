;;; config/clojure.el -*- lexical-binding: t; -*-
;;
;; Clojure and ClojureScript development configuration

;; ====================================
;; CLOJURE MODE
;; ====================================

;; Ensure font-lock (syntax highlighting) is enabled for Clojure
;; Use a timer to ensure fontification happens after everything else loads
(defun my/fontify-clojure-buffer ()
  "Fontify Clojure buffer after a short delay."
  (run-at-time 0.1 nil
               (lambda ()
                 (when (and (derived-mode-p 'clojure-mode 'clojurescript-mode)
                            (buffer-live-p (current-buffer)))
                   (font-lock-mode 1)
                   (font-lock-fontify-buffer)))))

(add-hook 'clojure-mode-hook #'my/fontify-clojure-buffer)
(add-hook 'clojurescript-mode-hook #'my/fontify-clojure-buffer)

(after! clojure-mode
  ;; Indentation style
  (setq clojure-indent-style 'align-arguments
        clojure-thread-all-but-last t)

  ;; Add clj-refactor keybindings
  (cljr-add-keybindings-with-prefix "C-c C-c"))

;; ====================================
;; CLOJURE KEYBINDINGS
;; ====================================

(add-hook! clojure-mode
  (map!
   (:map (clojure-mode-map clojurescript-mode-map)
    (:localleader
     ;; Eval commands
     (:prefix ("e" . "eval")
      :desc "Eval defun to comment" ";" #'cider-eval-defun-to-comment
      (:prefix ("p" . "pprint")
       :desc "Pprint last sexp to comment" ":" #'cider-pprint-eval-last-sexp-to-comment
       :desc "Pprint defun to comment"    ";" #'cider-pprint-eval-defun-to-comment
       :desc "Pprint defun at point"      "d" #'cider-pprint-eval-defun-at-point
       :desc "Pprint last sexp"           "e" #'cider-pprint-eval-last-sexp))

     ;; Format commands
     (:prefix ("=" . "format")
      :desc "Format buffer" "=" #'cider-format-buffer
      :desc "Format defun"  "f" #'cider-format-defun
      :desc "Align forms"   "l" #'clojure-align
      :desc "Format region" "r" #'cider-format-region
      (:prefix ("e" . "edn")
       :desc "Format EDN buffer"   "b" #'cider-format-edn-buffer
       :desc "Format EDN last sexp" "a" #'cider-format-edn-last-sexp
       :desc "Format EDN region"   "r" #'cider-format-edn-region))))))

;; ====================================
;; CIDER (REPL)
;; ====================================

(after! cider
  ;; REPL configuration
  (setq cider-ns-refresh-show-log-buffer t
        cider-show-error-buffer t
        cider-font-lock-dynamically t  ; Enable all CIDER syntax highlighting
        cider-eldoc-display-for-symbol-at-point nil  ; Use LSP instead
        cider-prompt-for-symbol nil)

  ;; Popup rules for CIDER buffers
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)

  ;; Prefer LSP for lookups
  (set-lookup-handlers! 'cider-mode nil))

;; ====================================
;; CLJ-REFACTOR
;; ====================================

(after! clj-refactor
  ;; Disable lookup handlers to prefer LSP
  (set-lookup-handlers! 'clj-refactor-mode nil)

  ;; Configuration
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-add-ns-to-blank-clj-files nil  ; Use LSP instead

        ;; Magic require namespaces (universal only)
        cljr-magic-require-namespaces
        '(("s"   . "schema.core")
          ("d"   . "datomic.api")
          ("m"   . "matcher-combinators.matchers")
          ("pp"  . "clojure.pprint"))))

;; ====================================
;; LISPYVILLE
;; ====================================

(use-package! lispyville
  :hook ((common-lisp-mode . lispyville-mode)
         (emacs-lisp-mode  . lispyville-mode)
         (scheme-mode      . lispyville-mode)
         (racket-mode      . lispyville-mode)
         (hy-mode          . lispyville-mode)
         (lfe-mode         . lispyville-mode)
         (clojure-mode     . lispyville-mode))
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

;; ====================================
;; FORMATTING
;; ====================================

;; Set cljstyle as the formatter for Clojure
;; Install: brew install cljstyle
;; Documentation: https://github.com/greglook/cljstyle

;; Configure cljstyle to work with both Clojure and ClojureScript
(set-formatter! 'cljstyle "cljstyle pipe" :modes '(clojure-mode clojurescript-mode))

;; cljstyle reads configuration from:
;; 1. Project: .cljstyle in project root
;; 2. Global: ~/.config/cljstyle/config.edn
;; 3. Default: built-in rules

;; To format manually: SPC c f  or  M-x format-buffer
