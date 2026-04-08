;;; config/keybindings.el -*- lexical-binding: t; -*-
;;
;; Custom keybindings and key remapping

;; ====================================
;; LEADER KEYS
;; ====================================

;; Set localleader to comma (Spacemacs-style)
(setq doom-localleader-key ",")

;; ====================================
;; CUSTOM LEADER BINDINGS
;; ====================================

(map! :leader
      (:prefix-map ("a" . "apps")
       :desc "List processes"    "p" #'list-processes))

;; Note: Removed ranger/deer bindings as those modules aren't enabled
;; To use them, enable :emacs dirvish in init.el and use:
;; (map! :leader
;;       (:prefix "a"
;;        :desc "Ranger" "r" #'ranger
;;        :desc "Deer"   "d" #'deer))

;; ====================================
;; ADDITIONAL GLOBAL BINDINGS
;; ====================================

;; Add any custom global keybindings here
;; Example:
;; (map! :g "C-c t" #'toggle-truncate-lines)
