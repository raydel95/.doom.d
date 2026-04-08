;;; modules/tools.el -*- lexical-binding: t; -*-
;;
;; Additional tools: window management, case conversion, git utilities,
;; documentation, regex conversion, and extra bindings for built-in commands

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

(use-package! string-inflection
  :defer t
  :init
  ;; g~ in normal mode: cycle through all case styles
  (map! :n "g~" #'string-inflection-all-cycle)
  ;; SPC c prefix: cycle + direct targets
  (map! :leader
        (:prefix ("c" . "code")
         :desc "Cycle case style"  "~" #'string-inflection-all-cycle
         :desc "kebab-case"        "-" #'string-inflection-kebab-case
         :desc "camelCase"         "i" #'string-inflection-camelcase
         :desc "PascalCase"        "C" #'string-inflection-pascal-case)))

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
         :desc "Copy GitHub link (commit)" "Y" #'git-link-commit
         :desc "Open repo on GitHub"     "H" #'git-link-homepage))
  :config
  (setq git-link-open-in-browser nil)) ; Copy to kill ring, don't open browser

;; ============================================================================
;; DEVDOCS (in-Emacs documentation browser)
;; ============================================================================

(use-package! devdocs
  :defer t
  :init
  (map! :leader
        (:prefix ("h" . "help")
         :desc "DevDocs lookup"  "D" #'devdocs-lookup
         :desc "DevDocs browse"  "P" #'devdocs-peruse))
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
         :desc "PCRE to Elisp"        "p" #'rxt-pcre-to-elisp
         :desc "Elisp to PCRE"        "e" #'rxt-elisp-to-pcre
         :desc "Explain PCRE"         "x" #'rxt-explain-pcre
         :desc "PCRE query-replace"   "r" #'pcre-query-replace-regexp)))

;; ============================================================================
;; EMBARK EXTRAS (beyond Doom's C-; default)
;; ============================================================================

(map! "C-." #'embark-dwim)  ; "Do What I Mean" — default action for thing at point

;; ============================================================================
;; LSP EXTRA BINDINGS
;; ============================================================================

(map! :leader
      (:prefix ("c" . "code")
       :desc "Format buffer"       "f" #'lsp-format-buffer
       :desc "Restart LSP"         "W" #'lsp-workspace-restart))

;; ============================================================================
;; CONSULT EXTRA BINDINGS
;; ============================================================================

(map! :leader
      (:prefix ("s" . "search")
       :desc "File symbols (LSP)"  "f" #'consult-lsp-file-symbols
       :desc "Outline (headings)"  "o" #'consult-outline))

;; ============================================================================
;; MAGIT EXTRA BINDINGS
;; ============================================================================

(map! :leader
      (:prefix ("g" . "git")
       :desc "Diff current file"   "d" #'magit-diff-buffer-file))

;;; modules/tools.el ends here
