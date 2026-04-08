;;; modules/ui.el -*- lexical-binding: t; -*-
;;
;; Theme, fonts, symbol rendering, and visual settings

;; ============================================================================
;; THEME & FONTS
;; ============================================================================

(setq doom-theme 'doom-sourcerer
      doom-font (font-spec :family "SauceCodePro Nerd Font" :size 14)
      doom-big-font (font-spec :family "SauceCodePro Nerd Font" :size 22)
      display-line-numbers-type 'relative)

;; ============================================================================
;; SYMBOL FONT FALLBACKS
;; ============================================================================

;; No Nerd Font has standard Unicode symbols like ⏺ ★ ⎿ ✻ (they're not in
;; the Nerd Font patch set). Without overrides, macOS falls back to Apple Color
;; Emoji (renders 2-cells wide) or Apple Symbols (wrong width), breaking vterm
;; column alignment.
;;
;; Route entire Unicode blocks to Iosevka Nerd Font Mono (true monospace)
;; so ANY character Claude Code uses in these ranges renders at exactly 1 cell.

(defun my/setup-symbol-font-fallbacks ()
  "Route symbol blocks to Iosevka Nerd Font Mono for correct vterm width."
  (set-fontset-font t (cons #x2300 #x23FF) (font-spec :family "Iosevka Nerd Font Mono") nil 'prepend) ; Misc Technical (⏺ ⎿ ⏵)
  (set-fontset-font t (cons #x2600 #x26FF) (font-spec :family "Iosevka Nerd Font Mono") nil 'prepend) ; Misc Symbols (★)
  (set-fontset-font t (cons #x2700 #x27BF) (font-spec :family "Iosevka Nerd Font Mono") nil 'prepend)) ; Dingbats (✻ ✽ ✶ spinners)

;; Apply on startup, after font changes, and after doom/reload-font
(add-hook 'after-init-hook #'my/setup-symbol-font-fallbacks)
(add-hook 'after-setting-font-hook #'my/setup-symbol-font-fallbacks)
(advice-add 'doom/reload-font :after (lambda (&rest _) (my/setup-symbol-font-fallbacks)))
(when (display-graphic-p) (my/setup-symbol-font-fallbacks))

;; ============================================================================
;; DISPLAY TABLE (Claude Code vterm normalization)
;; ============================================================================

;; Claude Code uses various dingbat characters for its spinner animation and
;; ⏺ (U+23FA) as the result indicator. These have inconsistent visual sizes
;; across fonts, causing line jitter. Replace them all with ● (U+25CF) which
;; is native to SauceCodePro NF at exact 1-cell width.

(defun my/setup-claude-display-table ()
  "Normalize Claude Code UI characters to consistent-width glyphs."
  (let ((dt (or buffer-display-table (make-display-table))))
    (aset dt #x23FA (vector (make-glyph-code #x25CF)))  ; ⏺ → ●
    (dolist (ch (list #x2736 #x273D #x273B #x2733 #x2734 #x2735
                      #x2737 #x2738 #x2739 #x273A #x273C #x273E))
      (aset dt ch (vector (make-glyph-code #x25CF))))    ; spinners → ●
    (setq buffer-display-table dt)))

(add-hook 'vterm-mode-hook #'my/setup-claude-display-table)

;; ============================================================================
;; MISC UI
;; ============================================================================

(setq which-key-idle-delay 0.4)

;;; modules/ui.el ends here
