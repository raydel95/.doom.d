;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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


;; disable confirmation message on exit
(setq confirm-kill-emacs nil)

;; set localleader the same as Spacemacs
(setq doom-localleader-key ",")

;; which-key
(setq which-key-idle-delay 0.4)

;; mode-line
(setq doom-modeline-major-mode-icon t)

;; font
(setq doom-font (font-spec  :family "Source Code Pro" :size 13)
            doom-big-font (font-spec :family "Source Code Pro" :size 22))


;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t)            ; When there are lots of glyphs, keep them in memory

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

;; company improvment
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  (setq company-box-doc-delay 0.2
        company-box-show-single-candidate t))

(add-hook 'company-mode-hook 'company-box-mode)


;; Workaround bug in completion (see autolad.el)
(after! cider
  (add-hook 'company-completion-started-hook 'user/set-company-maps)
  (add-hook 'company-completion-finished-hook 'user/unset-company-maps)
  (add-hook 'company-completion-cancelled-hook 'user/unset-company-maps)


  )

;; apps configuration
(setq ranger-show-hidden t
      ranger-preview-file t)

(map! :leader
      (:prefix ("a" . "apps")
       :desc "processes" "p" 'list-processes
       :desc "ranger" "r" 'ranger
       :desc "deer" "d" 'deer))


(map! :leader
      (:prefix ("c" . "code")
       "l" 'evilnc-comment-or-uncomment-lines
       "p" 'evilnc-comment-or-uncomment-paragraphs
       "y" 'evilnc-copy-and-comment-lines
       "t" 'evilnc-quick-comment-or-uncomment-to-the-line))

(add-hook! clojure-mode
  (map!
   (:map (clojure-mode-map clojurescript-mode-map)
    (:localleader
     (:prefix ("=" . "format")
      ("=" #'cider-format-buffer
       "f" #'cider-format-defun
       "l" #'clojure-align
       "r" #'cider-format-region
       (:prefix ("e" . "edn")
        ("b" #'cider-format-edn-buffer
         "a" #'cider-format-edn-last-sexp
         "r" #'cider-format-edn-region))))))))


(setq large-file-warning-threshold 1000000)
;; "Disable font-lock in files with size > 100k
;; or open in literall mode when size > 1mb
;; (defun big-files ()
;;   (interactive)
;;     (when (and (> (buffer-size) (* 100 1024)) font-lock-mode (yes-or-no-p
;;                                (concat "This is a large file: "
;;                                        (buffer-file-name)
;;                                        ", do you want disable font-lock mode to imporve performance?")))
;;       (font-lock-mode -1)))
;; (add-hook 'find-file-hook 'big-files)

(setq doom-themes-treemacs-theme "doom-colors") ;;treemacs theme
(doom-themes-treemacs-config)
(doom-themes-visual-bell-config)
