;;; ../github/ivangalbans/dotfiles/emacs/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; apps configuration
(map! :leader
      (:prefix ("a" . "apps")
       :desc "processes" "p" 'list-processes
       :desc "deer" "d" 'deer))

(map! :leader
       :desc "Expand region" "v" #'er/expand-region)
