;;; config/ui.el -*- lexical-binding: t; -*-
;;
;; UI and appearance configuration

;; ====================================
;; THEME & APPEARANCE
;; ====================================

(setq doom-theme 'doom-sourcerer)

;; ====================================
;; FONTS
;; ====================================

(setq doom-font     (font-spec :family "SauceCodePro Nerd Font" :size 14)
      doom-big-font (font-spec :family "SauceCodePro Nerd Font" :size 22))

;; Symbol font fallbacks — no Nerd Font has ⏺ ★ ⎿ ✻ (standard Unicode,
;; not in the Nerd Font patch set). Without overrides, Apple Color Emoji
;; grabs ⏺ and renders it 2-cells wide, breaking vterm column alignment.
(defun my/setup-symbol-font-fallbacks ()
  "Route symbol blocks to Iosevka Nerd Font Mono for correct vterm width.
Covers all characters Claude Code uses: ⏺ ⎿ ⏵ ★ ✻ ✽ ✶ and spinner variants."
  (set-fontset-font t (cons #x2300 #x23FF) (font-spec :family "Iosevka Nerd Font Mono") nil 'prepend) ; Misc Technical (⏺ ⎿ ⏵)
  (set-fontset-font t (cons #x2600 #x26FF) (font-spec :family "Iosevka Nerd Font Mono") nil 'prepend) ; Misc Symbols (★)
  (set-fontset-font t (cons #x2700 #x27BF) (font-spec :family "Iosevka Nerd Font Mono") nil 'prepend)); Dingbats (✻ ✽ ✶ spinners)

(add-hook 'after-init-hook #'my/setup-symbol-font-fallbacks)
(add-hook 'after-setting-font-hook #'my/setup-symbol-font-fallbacks)
(advice-add 'doom/reload-font :after (lambda (&rest _) (my/setup-symbol-font-fallbacks)))
(when (display-graphic-p) (my/setup-symbol-font-fallbacks))

;; Display-table fixes for Claude Code vterm buffers:
;; ⏺ U+23FA (result indicator) → ● U+25CF (native to SauceCodePro NF, exact 1-cell)
;; Spinner dingbats → ● (consistent size, no jitter between frames)
(defun my/setup-claude-display-table ()
  "Normalize Claude Code UI characters to consistent-width glyphs."
  (let ((dt (or buffer-display-table (make-display-table))))
    (aset dt #x23FA (vector (make-glyph-code #x25CF)))  ; ⏺ → ●
    (dolist (ch (list #x2736 #x273D #x273B #x2733 #x2734 #x2735
                      #x2737 #x2738 #x2739 #x273A #x273C #x273E))
      (aset dt ch (vector (make-glyph-code #x25CF))))   ; spinners → ●
    (setq buffer-display-table dt)))

(add-hook 'vterm-mode-hook #'my/setup-claude-display-table)

;; ====================================
;; LINE NUMBERS
;; ====================================

;; Enable line numbers
;; Options: t (absolute), 'relative, or nil (disabled)
(setq display-line-numbers-type t)

;; For better performance, consider using 'relative or disabling entirely:
;; (setq display-line-numbers-type 'relative) ; Vim-style relative numbers
;; (setq display-line-numbers-type nil)       ; Disable for max performance

;; ====================================
;; WHICH-KEY
;; ====================================

;; Reduce which-key delay for faster key hint display
(setq which-key-idle-delay 0.4)

;; ====================================
;; MODELINE
;; ====================================

;; Doom modeline configuration is mostly handled by the :ui modeline module
;; Add custom modeline tweaks here if needed

;; ====================================
;; TREEMACS (if enabled)
;; ====================================

;; Uncomment if you enable :ui treemacs in init.el
;; (after! treemacs
;;   (setq treemacs-width 30
;;         treemacs-follow-mode t))
