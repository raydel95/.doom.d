;;; modules/lsp.el -*- lexical-binding: t; -*-
;;
;; LSP configuration, monorepo support, and daemon-mode fixes

;; ============================================================================
;; LSP-MODE
;; ============================================================================

;; NOTE: LSP_USE_PLISTS=true must be set as an environment variable (in .zshrc
;; or Doom's env file), NOT as a setq. lsp-mode reads it at compile time.

(after! lsp-mode
  ;; Performance
  (setq lsp-log-io nil
        lsp-idle-delay 0.5

        ;; UI
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        lsp-signature-render-documentation t
        lsp-semantic-tokens-enable t
        lsp-enable-symbol-highlighting t

        ;; Navigation
        lsp-enable-xref t
        lsp-enable-imenu t
        lsp-enable-snippet t

        ;; File watchers — high threshold for monorepo
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 50000

        ;; Keep servers alive when all buffers close (avoids 1-2min re-index)
        lsp-keep-workspace-alive t)

  ;; Monorepo-specific ignore patterns (appended to lsp-mode defaults)
  (dolist (pattern '("[/\\\\]\\.cache$"
                     "[/\\\\]\\.clj-kondo$"
                     "[/\\\\]\\.shadow-cljs$"
                     "[/\\\\]resources/public/js/compiled$"))
    (add-to-list 'lsp-file-watch-ignored-directories pattern))

  ;; Auto-save project buffers after LSP rename
  (advice-add #'lsp-rename :after
              (lambda (&rest _) (projectile-save-project-buffers))))

;; ============================================================================
;; MONOREPO ROOT DETECTION
;; ============================================================================

;; Force LSP to use monorepo root for lein-monolith projects
(defun my/find-lein-monolith-root (dir)
  "Find lein-monolith root by looking for project.clj with lein-monolith plugin."
  (let ((current dir)
        (found nil))
    (while (and current (not found))
      (let ((project-file (expand-file-name "project.clj" current)))
        (when (file-exists-p project-file)
          (with-temp-buffer
            (insert-file-contents project-file)
            (when (search-forward "lein-monolith" nil t)
              (setq found current))))
        (setq current (file-name-directory (directory-file-name current)))
        (when (string= current "/") (setq current nil))))
    found))

(after! lsp-mode
  (defun my/lsp-clojure-project-root-override (orig-fun &rest args)
    "Use monorepo root if lein-monolith is detected."
    (or (my/find-lein-monolith-root default-directory)
        (apply orig-fun args)))
  (advice-add 'lsp-clojure--get-project-root :around #'my/lsp-clojure-project-root-override))

;; ============================================================================
;; LSP-UI
;; ============================================================================

(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-max-width 60
        lsp-ui-peek-list-width 60
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 0.5))

;; ============================================================================
;; LSP EXTRA BINDINGS
;; ============================================================================

(map! :leader
      (:prefix ("c" . "code")
       :desc "Format buffer" "f" #'lsp-format-buffer
       :desc "Restart LSP"   "W" #'lsp-workspace-restart))

;; ============================================================================
;; DAEMON MODE FIX
;; ============================================================================

;; lsp-deferred checks buffer visibility via get-buffer-window, but in daemon
;; mode the idle timer can fire before the emacsclient frame is fully set up,
;; causing lsp() to never be called. Retry after frame creation.
;; See: https://github.com/emacs-lsp/lsp-mode/issues/4190
(when (daemonp)
  (defun my/lsp-retry-deferred-buffers-h ()
    "Retry LSP initialization for buffers deferred before the frame existed."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (bound-and-true-p lsp--buffer-deferred)
                   (not (bound-and-true-p lsp-mode))
                   (buffer-file-name))
          (lsp)))))
  (add-hook 'server-after-make-frame-hook #'my/lsp-retry-deferred-buffers-h))

;;; modules/lsp.el ends here
