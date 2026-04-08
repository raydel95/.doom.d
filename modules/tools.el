;;; modules/tools.el -*- lexical-binding: t; -*-
;;
;; Additional tools: window management, case conversion, git utilities,
;; documentation, and regex conversion

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
;; STRING-INFLECTION (case style cycling)
;; ============================================================================

;; Cycle: kebab-case -> camelCase -> PascalCase -> SCREAMING_SNAKE -> snake_case
(use-package! string-inflection
  :defer t
  :init
  (map! :leader
        (:prefix ("c" . "code")
         :desc "Cycle case style"    "~" #'string-inflection-all-cycle))
  (map! :n "g~" #'string-inflection-all-cycle))

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
         :desc "Copy GitHub link"        "y" #'git-link
         :desc "Copy GitHub link (commit)" "Y" #'git-link-commit))
  :config
  (setq git-link-open-in-browser nil)) ; Copy to kill ring, don't open browser

;; ============================================================================
;; DEVDOCS (in-Emacs documentation browser)
;; ============================================================================

;; Browse Go stdlib, Clojure, and other docs without leaving Emacs.
;; First run: M-x devdocs-install to download doc sets.
(use-package! devdocs
  :defer t
  :init
  (map! :leader
        (:prefix ("h" . "help")
         :desc "DevDocs lookup"    "D" #'devdocs-lookup))
  :config
  ;; Auto-set relevant doc sets per major mode
  (setq-hook! 'go-mode-hook devdocs-current-docs '("go"))
  (setq-hook! 'clojure-mode-hook devdocs-current-docs '("clojure~1.11")))

;; ============================================================================
;; PCRE2EL (regex syntax conversion)
;; ============================================================================

;; Convert between PCRE (Java/Clojure), Emacs Lisp, and rx regex syntaxes.
;; Interactive: M-x pcre-to-elisp, rxt-explain-pcre, etc.
(use-package! pcre2el
  :defer t
  :init
  (map! :leader
        (:prefix ("r" . "regex")
         :desc "PCRE to Elisp"  "p" #'rxt-pcre-to-elisp
         :desc "Elisp to PCRE"  "e" #'rxt-elisp-to-pcre
         :desc "Explain PCRE"   "x" #'rxt-explain-pcre)))

;;; modules/tools.el ends here
