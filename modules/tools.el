;;; modules/tools.el -*- lexical-binding: t; -*-
;;
;; Additional tools: window management, case conversion, git utilities,
;; documentation, and regex conversion.
;;
;; Each package owns its own bindings here. Cross-cutting bindings for
;; LSP, consult, and embark live in their respective modules (lsp.el,
;; completion.el).

;; ============================================================================
;; ACE-WINDOW (number-based window jumping)
;; ============================================================================

(use-package! ace-window
  :defer t
  :init
  (map! :leader
        :desc "Ace window" "w w" #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

;; ============================================================================
;; STRING-INFLECTION (case style conversion)
;; ============================================================================

;; Sub-prefix SPC c s (code → style) groups all case transformations.
;; g~ in normal mode provides vim-native cycling.
(use-package! string-inflection
  :defer t
  :init
  (map! :n "g~" #'string-inflection-all-cycle)
  (map! :leader
        (:prefix ("c" . "code")
         (:prefix ("s" . "style")
          :desc "Cycle all"         "s" #'string-inflection-all-cycle
          :desc "kebab-case"        "k" #'string-inflection-kebab-case
          :desc "camelCase"         "c" #'string-inflection-camelcase
          :desc "PascalCase"        "p" #'string-inflection-pascal-case
          :desc "snake_case"        "_" #'string-inflection-underscore
          :desc "SCREAMING_SNAKE"   "!" #'string-inflection-upcase))))

;; ============================================================================
;; GIT-TIMEMACHINE (browse file history)
;; ============================================================================

(use-package! git-timemachine
  :defer t
  :init
  (map! :leader
        (:prefix ("g" . "git")
         :desc "Time machine" "t" #'git-timemachine)))

;; ============================================================================
;; GIT-LINK (GitHub URLs)
;; ============================================================================

(use-package! git-link
  :defer t
  :init
  (map! :leader
        (:prefix ("g" . "git")
         :desc "Copy GitHub link"         "y" #'git-link
         :desc "Copy GitHub link (commit)" "Y" #'git-link-commit
         :desc "Open repo on GitHub"      "O" #'git-link-homepage))
  :config
  (setq git-link-open-in-browser nil))

;; ============================================================================
;; MAGIT EXTRAS
;; ============================================================================

(map! :leader
      (:prefix ("g" . "git")
       :desc "Diff current file" "d" #'magit-diff-buffer-file))

;; ============================================================================
;; DEVDOCS (in-Emacs documentation browser)
;; ============================================================================

;; First run: M-x devdocs-install to download doc sets.
(use-package! devdocs
  :defer t
  :init
  (map! :leader
        (:prefix ("h" . "help")
         :desc "DevDocs lookup"  "D" #'devdocs-lookup
         :desc "DevDocs browse"  "B" #'devdocs-peruse))
  :config
  (setq-hook! 'go-mode-hook devdocs-current-docs '("go"))
  (setq-hook! 'clojure-mode-hook devdocs-current-docs '("clojure~1.11")))

;; ============================================================================
;; PCRE2EL (regex syntax conversion)
;; ============================================================================

(use-package! pcre2el
  :defer t
  :init
  (map! :leader
        (:prefix ("r" . "regex")
         :desc "PCRE to Elisp"      "p" #'rxt-pcre-to-elisp
         :desc "Elisp to PCRE"      "e" #'rxt-elisp-to-pcre
         :desc "Explain PCRE"       "x" #'rxt-explain-pcre
         :desc "PCRE query-replace" "r" #'pcre-query-replace-regexp)))

;;; modules/tools.el ends here
