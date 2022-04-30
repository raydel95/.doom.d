;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'auto-mode-alist '("\\.repl\\'" . clojure-mode))

(setq
 confirm-kill-emacs nil
 doom-localleader-key ","
 auto-save-default t
 large-file-warning-threshold 1000000

 doom-theme                 'doom-vibrant
 doom-themes-treemacs-theme "doom-colors"

 doom-font     (font-spec :family "Source Code Pro" :size 14)
 doom-big-font (font-spec :family "Source Code Pro" :size 22)

 doom-modeline-major-mode-icon t
 doom-modeline-env-version t
 doom-modeline-env-load-string "..."
 doom-modeline-buffer-encoding t

 ; move to the new window and select buffer to display
 evil-vsplit-window-right t
 evil-split-window-below t

 org-directory "~/org/"

 +ivy-buffer-preview t

 ;;modeline
 inhibit-compacting-font-caches t
 find-file-visit-truename t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)                          ; On laptops it's nice to know how much power you have

;; (setq-default
;;  delete-by-moving-to-trash t                      ; Delete files to trash
;;  window-combination-resize t                      ; take new window space from all other windows (not just current)
;;  x-stretch-cursor t)
                                        ; Stretch cursor to the glyph width

;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; (doom-themes-treemacs-config)

(use-package! lsp-java
  :after java-mode
  :config
  (setq lsp-java-references-code-lens-enabled t
        lsp-java-implementations-code-lens-enabled t))

(use-package! dap-mode
  :init
  (require 'dap-chrome)
  :config
  (setq dap-enable-mouse-support nil))

(use-package! lsp-mode
  :commands lsp
  :config

  ;; Core
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil
        lsp-signature-function 'lsp-signature-posframe
        lsp-semantic-tokens-enable t
        lsp-idle-delay 0.3
        lsp-use-plists nil)
  (add-hook 'lsp-after-apply-edits-hook (lambda (&rest _) (save-buffer)))
  (add-hook 'lsp-mode-hook (lambda () (setq-local company-format-margin-function #'company-vscode-dark-icons-margin)))

  ;; C#
  (when-let ((omnisharp-path (-some-> (executable-find "omnisharp") file-chase-links file-name-directory directory-file-name file-name-directory)))
    (setq lsp-csharp-server-install-dir omnisharp-path
          lsp-csharp-server-path (f-join omnisharp-path "bin/omnisharp")))

  ;; Clojure
  (let ((clojure-lsp-dev (expand-file-name "~/dev/clojure-lsp/clojure-lsp/clojure-lsp")))
    (when (file-exists-p clojure-lsp-dev)
      ;; clojure-lsp local development
      (setq lsp-clojure-custom-server-command `("bash" "-c" ,clojure-lsp-dev)
            lsp-completion-no-cache t
            lsp-completion-use-last-result nil)))

  ;; Rust
  (setq lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-chaining-hints t))


(use-package! lsp-treemacs
  :config
  (setq lsp-treemacs-error-list-current-project-only t))

(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil))

(use-package! treemacs-all-the-icons
  :after treemacs)

(use-package! cider
  :after clojure-mode
  :config
  (setq cider-show-error-buffer t ;'only-in-repl
        cider-font-lock-dynamically nil ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil
        cider-use-xref nil) ; use lsp
  (set-lookup-handlers! '(cider-mode cider-repl-mode) nil) ; use lsp
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)
  ;; use lsp completion
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point))))

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
  (setq clojure-indent-style 'align-arguments))

(use-package! company
  :config
  (setq company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend)))


(load! "+bindings")
(load! "+nubank")
