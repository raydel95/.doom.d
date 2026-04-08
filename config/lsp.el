;;; config/lsp.el -*- lexical-binding: t; -*-
;;
;; LSP (Language Server Protocol) configuration

;; ====================================
;; LSP-MODE
;; ====================================

(after! lsp-mode
  ;; Performance settings
  (setq lsp-idle-delay 0.5              ; Default (modern LSP is fast)
        lsp-log-io nil                  ; Disable logging for performance
        lsp-use-plists t                ; Better performance with plists

        ;; UI settings
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        lsp-signature-render-documentation t
        lsp-signature-function 'lsp-signature-posframe

        ;; Semantic tokens (re-enabled for better syntax highlighting)
        lsp-semantic-tokens-enable t

        ;; Re-enabled features (modern hardware handles these)
        lsp-enable-symbol-highlighting t
        lsp-enable-on-type-formatting nil

        ;; File watchers — monorepo needs a high threshold
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 50000

        ;; Keep LSP servers alive in daemon mode
        lsp-keep-workspace-alive t

        ;; Completion settings
        lsp-completion-sort-initial-results t
        lsp-completion-no-cache t
        lsp-completion-use-last-result nil)

  ;; Auto-save project buffers after rename
  (advice-add #'lsp-rename :after
              (lambda (&rest _)
                (projectile-save-project-buffers))))

;; ====================================
;; LSP-UI
;; ====================================

(after! lsp-ui
  (setq lsp-ui-peek-list-width 60
        lsp-ui-doc-max-width 60
        lsp-ui-doc-enable t              ; Re-enabled (modern systems handle this fine)
        lsp-ui-peek-fontify 'on-demand   ; Only fontify when needed
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 0.5))
