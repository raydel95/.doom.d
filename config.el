;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
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


;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(let ((nudev-emacs-path "~/dev/nu/nudev/ides/emacs/"))
  (when (file-directory-p nudev-emacs-path)
    (add-to-list 'load-path nudev-emacs-path)
    (require 'nu nil t)
    (require 'nu-datomic-query nil t)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ivan Galban"
      user-mail-address "ivan.galban.smith@gmail.com")


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;; (setq display-line-numbers-type t)

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

(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; apps configuration
(map! :leader
      (:prefix ("a" . "apps")
       :desc "processes" "p" 'list-processes
       :desc "deer" "d" 'deer))

;;modeline
(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)


(setq large-file-warning-threshold 1000000)

(setq doom-themes-treemacs-theme "doom-colors") ;;treemacs theme
(doom-themes-treemacs-config)

(use-package! lsp-mode
  :commands lsp
  :hook ((clojure-mode . lsp)
         (dart-mode . lsp)
         (java-mode . lsp))
  :config
  (let ((omnisharp-path (-some-> (executable-find "omnisharp")
                          file-chase-links
                          file-name-directory
                          directory-file-name
                          file-name-directory)))
    (when omnisharp-path
      (setq lsp-csharp-server-install-dir omnisharp-path
            lsp-csharp-server-path (f-join omnisharp-path "bin/omnisharp")))
    (setq lsp-headerline-breadcrumb-enable nil
          lsp-lens-enable t
          lsp-enable-file-watchers t
          lsp-signature-render-documentation nil
          lsp-signature-function 'lsp-signature-posframe
          lsp-semantic-tokens-enable t
          lsp-idle-delay 0.3
          lsp-use-plists nil
          lsp-completion-sort-initial-results t ; check if should keep as t
          lsp-completion-no-cache t
          lsp-completion-use-last-result nil))
  (advice-add #'lsp-rename :after (lambda (&rest _) (projectile-save-project-buffers)))
  (add-hook 'lsp-mode-hook (lambda () (setq-local company-format-margin-function #'company-vscode-dark-icons-margin))))

(use-package! lsp-treemacs
  :config
  (setq lsp-treemacs-error-list-current-project-only t))

(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-list-width 60
        lsp-ui-doc-max-width 60
        lsp-ui-doc-enable nil
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil))

(use-package! treemacs-all-the-icons
  :after treemacs)

(use-package! cider
  :after clojure-mode
  :config
  (setq cider-ns-refresh-show-log-buffer t
        cider-show-error-buffer t ;'only-in-repl
        cider-font-lock-dynamically nil ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil)
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)
  (set-lookup-handlers! 'cider-mode nil) ; use lsp
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point))) ; use lsp
  )

(use-package! clj-refactor
  :after clojure-mode
  :config
  (set-lookup-handlers! 'clj-refactor-mode nil)
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-add-ns-to-blank-clj-files nil ; use lsp
        cljr-magic-require-namespaces
        '(("s"   . "schema.core")
          ("gen" . "common-test.generators")
          ("d-pro" . "common-datomic.protocols.datomic")
          ("ex" . "common-core.exceptions.core")
          ("dth" . "common-datomic.test-helpers")
          ("t-money" . "common-core.types.money")
          ("t-time" . "common-core.types.time")
          ("d" . "datomic.api")
          ("m" . "matcher-combinators.matchers")
          ("pp" . "clojure.pprint"))))

(use-package! clojure-mode
  :config
  (setq clojure-indent-style 'align-arguments
        clojure-thread-all-but-last t)
  (cljr-add-keybindings-with-prefix "C-c C-c"))

(use-package! company
  :config
  (setq company-tooltip-align-annotations t
        company-icon-size 20))



(setq auto-save-default t)
