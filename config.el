;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ivan Galban"
      user-mail-address "ivan.galban.smith@gmail.com")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;; (setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; disable confirmation message on exit
(setq confirm-kill-emacs nil)

;; set localleader the same as Spacemacs
(setq doom-localleader-key ",")

;; which-key
(setq which-key-idle-delay 0.3)

;; mode-line
(setq doom-modeline-major-mode-icon t
      doom-modeline-env-version t
      doom-modeline-env-load-string "..."
      doom-modeline-buffer-encoding t)

;; font
(setq doom-font     (font-spec :family "Source Code Pro" :size 14)
      doom-big-font (font-spec :family "Source Code Pro" :size 22))


(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)                          ; On laptops it's nice to know how much power you have

(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

(setq evil-vsplit-window-right t                  ; move to the new window and select buffer to display
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(setq doom-theme 'doom-vibrant)                   ; theme
;; (delq! t custom-theme-load-path)

;; (after! flyspell (require 'flyspell-lazy) (flyspell-lazy-mode 1)) ; use flyspell-lazy


; FIXME:
;; company improvment
;; (after! company
;;   (setq
;;    company-idle-delay 0.1
;;    company-box-doc-delay 0.2
;;    company-box-show-single-candidate t
;;    company-minimum-prefix-length 1
;;    company-show-numbers t)
;;   (setq-default history-length 1000)
;;   (setq-default prescient-history-length 1000))

;; Workaround bug in completion (see autolad.el)
;; (after! cider
;;   (add-hook 'company-completion-started-hook 'user/set-company-maps)
;;   (add-hook 'company-completion-finished-hook 'user/unset-company-maps)
;;   (add-hook 'company-completion-cancelled-hook 'user/unset-company-maps))

;; (add-hook 'company-completion-started-hook 'user/set-company-maps)
;; (add-hook 'company-completion-finished-hook 'user/unset-company-maps)
;; (add-hook 'company-completion-cancelled-hook 'user/unset-company-maps)


;; REBL
;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; C-S-x send defun to rebl
;; C-x C-r send last sexp to rebl (Normally bound to "find-file-read-only"... Who actually uses that though?)
(add-hook 'cider-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-x") #'rebl-eval-defun-at-point)
            (local-set-key (kbd "C-x C-r") #'rebl-eval-last-sexp)))


;; apps configuration
;;dired
(setq ranger-show-hidden t
      ranger-preview-file t)

(map! :leader
      (:prefix ("a" . "apps")
       :desc "processes" "p" 'list-processes
       :desc "ranger" "r" 'ranger
       :desc "deer" "d" 'deer))


;; FIXME:
;; (map! :leader
;;       (:prefix ("c" . "code")
;;        "l" 'evilnc-comment-or-uncomment-lines
;;        "p" 'evilnc-comment-or-uncomment-paragraphs
;;        "y" 'evilnc-copy-and-comment-lines
;;        "t" 'evilnc-quick-comment-or-uncomment-to-the-line))


;;; Major Mode

;; Clojure

(add-hook! clojure-mode
  (map!
   (:map (clojure-mode-map clojurescript-mode-map)
    (:localleader
     (:prefix ("s" . "send-to-rebl")
      ("d" #'rebl-eval-defun-at-point)
      ("e" #'rebl-eval-last-sexp))
     (:prefix ("e" . "eval")
     (";" #'cider-eval-defun-to-comment)
     (:prefix ("p" . "pprint")
      (":" #'cider-pprint-eval-last-sexp-to-comment)
      (";" #'cider-pprint-eval-defun-to-comment)
      ("d" #'cider-pprint-eval-defun-at-point)
      ("e" #'cider-pprint-eval-last-sexp)))
     (:prefix ("=" . "format")
      ("=" #'cider-format-buffer
       "f" #'cider-format-defun
       "l" #'clojure-align
       "r" #'cider-format-region
       (:prefix ("e" . "edn")
        ("b" #'cider-format-edn-buffer
         "a" #'cider-format-edn-last-sexp
         "r" #'cider-format-edn-region))))))))

;;modeline
(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)


(setq large-file-warning-threshold 1000000)

(setq doom-themes-treemacs-theme "doom-colors") ;;treemacs theme
(doom-themes-treemacs-config)


(defun typescript-mode-setup ()
  "Custom setup for Typescript mode"
  (setq flycheck-checker 'javascript-eslint)
  )
(add-hook 'typescript-mode-hook 'typescript-mode-setup)


;; lsp-config
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )

(use-package! lsp-treemacs
  :config
  (setq lsp-treemacs-error-list-current-project-only t))

(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-list-width 60
        lsp-ui-doc-max-width 60
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil))

(use-package! treemacs-all-the-icons
  :after treemacs)

(use-package! cider
  :after clojure-mode
  :config
  (set-lookup-handlers! 'cider-mode nil))

(use-package! clj-refactor
  :after clojure-mode
  :config
  (set-lookup-handlers! 'clj-refactor-mode nil))

(setq cljr-add-ns-to-blank-clj-files nil) ; disable clj-refactor adding ns to blank files


(let ((private-emacs-path "~/.doom-private/"))
  (when (file-directory-p private-emacs-path)
    (add-to-list 'load-path private-emacs-path)
    (require 'private)))

(setq auto-save-default t)
