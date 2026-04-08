;;; modules/completion.el -*- lexical-binding: t; -*-
;;
;; Completion enhancements: corfu tuning, nerd-icons, consult-lsp, cape extras
;;
;; Base stack (from Doom modules):
;;   corfu (+orderless +dabbrev) — in-buffer completion with fuzzy matching
;;   vertico — minibuffer completion (bundles consult, embark, marginalia)
;;
;; This file adds:
;;   nerd-icons-corfu — type icons in completion popup
;;   consult-lsp — workspace symbol search and diagnostics browsing
;;   cape extras — keyword completion for prog-mode

;; ============================================================================
;; CORFU TUNING
;; ============================================================================

(after! corfu
  ;; Show documentation popup alongside completion (already enabled by Doom,
  ;; just tune the delay)
  (setq corfu-popupinfo-delay '(0.5 . 0.2))) ; 0.5s initial, 0.2s when navigating

;; ============================================================================
;; NERD ICONS IN COMPLETION POPUP
;; ============================================================================

;; Show function/variable/class icons next to each completion candidate.
;; Requires a Nerd Font (SauceCodePro NF is already configured in ui.el).
(use-package! nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ============================================================================
;; CONSULT-LSP (workspace symbols & diagnostics)
;; ============================================================================

;; Adds two commands:
;;   consult-lsp-symbols — search ALL symbols across LSP workspace with preview
;;   consult-lsp-diagnostics — browse all errors/warnings with preview
(use-package! consult-lsp
  :defer t
  :init
  (map! :leader
        (:prefix ("s" . "search")
         :desc "LSP symbols (workspace)" "S" #'consult-lsp-symbols
         :desc "LSP diagnostics"         "D" #'consult-lsp-diagnostics)))

;; ============================================================================
;; CAPE EXTRAS
;; ============================================================================

;; Add keyword completion (language keywords like defn, let, if, etc.)
;; Low priority (depth 15) so LSP completions take precedence.
(add-hook! 'prog-mode-hook
  (defun my/add-cape-keyword-h ()
    (add-hook 'completion-at-point-functions #'cape-keyword 15 t)))

;; ============================================================================
;; EMBARK EXTRAS
;; ============================================================================

;; embark-act (C-;) is bound by Doom's vertico module.
;; embark-dwim runs the default action for thing at point without showing a menu.
(map! "C-." #'embark-dwim)

;; ============================================================================
;; CONSULT EXTRA BINDINGS
;; ============================================================================

(map! :leader
      (:prefix ("s" . "search")
       :desc "File symbols (LSP)" "f" #'consult-lsp-file-symbols
       :desc "Outline (headings)" "o" #'consult-outline))

;;; modules/completion.el ends here
