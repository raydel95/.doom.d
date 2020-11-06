;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ivan Galban"
      user-mail-address "ivan.galban.smith@gmail.com")

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
(setq which-key-idle-delay 0.1)

;; mode-line
(setq doom-modeline-major-mode-icon t
      doom-modeline-env-version t
      doom-modeline-env-load-string "..."
      doom-modeline-buffer-encoding t)

;; font
(setq doom-font (font-spec  :family "Source Code Pro" :size 14)
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

(setq doom-theme 'doom-vibrant)                   ; theme
;; (delq! t custom-theme-load-path)

;; (after! flyspell (require 'flyspell-lazy) (flyspell-lazy-mode 1)) ; use flyspell-lazy


(setq company-selection-wrap-around t)
;; company improvment
(after! company
  (setq
   company-idle-delay 0.1
   company-box-doc-delay 0.2
   company-box-show-single-candidate t
   company-minimum-prefix-length 0
   company-show-numbers t)
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000))

;; Workaround bug in completion (see autolad.el)
;; (after! cider
;;   (add-hook 'company-completion-started-hook 'user/set-company-maps)
;;   (add-hook 'company-completion-finished-hook 'user/unset-company-maps)
;;   (add-hook 'company-completion-cancelled-hook 'user/unset-company-maps))

(add-hook 'company-completion-started-hook 'user/set-company-maps)
(add-hook 'company-completion-finished-hook 'user/unset-company-maps)
(add-hook 'company-completion-cancelled-hook 'user/unset-company-maps)

;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; C-S-x send defun to rebl
;; C-x C-r send last sexp to rebl (Normally bound to "find-file-read-only"... Who actually uses that though?)
(add-hook 'cider-mode-hook
          (lambda ()
            (local-set-key (kbd "C-S-x") #'rebl-eval-defun-at-point)
            (local-set-key (kbd "C-x C-r") #'rebl-eval-last-sexp)))


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
     (:prefix ("s" . "send-to-rebl")
      ("d" #'rebl-eval-defun-at-point)
      ("e" #'rebl-eval-last-sexp))
     (:prefix ("e" . "eval")
     (";" #'cider-eval-defun-to-comment)
     (:prefix ("p" . "pprint")
      (":" #'cider-pprint-eval-last-sexp-to-comment)
      (";" #'cider-pprint-eval-defun-to-comment)
      ("d" #'cider-pprint-eval-defun-at-point)
      ("e" #'cider-pprint-eval-last-sexp)))
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

(setq doom-themes-treemacs-theme "doom-colors") ;;treemacs theme
(doom-themes-treemacs-config)


(defun typescript-mode-setup ()
  "Custom setup for Typescript mode"
  (setq flycheck-checker 'javascript-eslint)
  )
(add-hook 'typescript-mode-hook 'typescript-mode-setup)

(defun setup-tide-mode ()
  "Setup function for tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(add-hook 'typescript-mode #'setup-tide-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(omnisharp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
