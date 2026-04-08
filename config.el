;;; config.el -*- lexical-binding: t; -*-
;;
;; Doom Emacs configuration — modular loader
;; Each topic lives in its own file under modules/

;; ============================================================================
;; IDENTITY
;; ============================================================================

(setq user-full-name "Raydel Alonso"
      user-mail-address "raydel.alonso@amperity.com")

;; Localleader key (Spacemacs-style)
(setq doom-localleader-key ",")

;; Prevent "Do you want to restart?" prompts for LSP servers on quit
(setq confirm-kill-processes nil)

;; ============================================================================
;; MODULES (load order matters: performance < lsp < lang-*)
;; ============================================================================

(load! "modules/ui")
(load! "modules/performance")
(load! "modules/lsp")
(load! "modules/lang-clojure")
(load! "modules/lang-go")
(load! "modules/claude-code")

;;; config.el ends here
