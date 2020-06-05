;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; built using https://github.com/tecosaur/emacs-config/blob/master/config.el

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Raydel Enrique Alonso Baryolo"
      user-mail-address "raydelalonsobaryolo@gmail.com")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


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
(setq which-key-idle-delay 0.4)

(setq doom-modeline-major-mode-icon t)

(setq doom-font (font-spec  :family "Source Code Pro" :size 13)
            doom-big-font (font-spec :family "Source Code Pro" :size 22))

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)                          ; On laptops it's nice to know how much power you have

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(setq evil-vsplit-window-right t                  ; move to the new window and select buffer to display
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)

(setq doom-theme 'doom-vibrant)                   ; theme
(delq! t custom-theme-load-path)

(after! flyspell (require 'flyspell-lazy) (flyspell-lazy-mode 1)) ; use flyspell-lazy
(after! company ;;company improvment
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)

(setq-default history-length 1000)
(setq-default prescient-history-length 1000))

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'Info-mode-hook #'mixed-pitch-mode)


;; ranger configuration
(setq ranger-show-hidden t
      ranger-preview-file t)

(map! :leader
      (:prefix ("a" . "apps")
       :desc "ranger" "r" 'ranger
       :desc "deer" "d" 'deer
      )
)
