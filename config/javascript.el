;;; config/javascript.el -*- lexical-binding: t; -*-
;;
;; JavaScript/TypeScript development configuration

;; ====================================
;; JAVASCRIPT MODE
;; ====================================

(after! js2-mode
  ;; Indentation
  (setq js-indent-level 2
        js2-basic-offset 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  ;; Use js2-mode for .js files
  (setq-hook! 'js2-mode-hook +format-with 'prettier))

;; ====================================
;; TYPESCRIPT MODE
;; ====================================

(after! typescript-mode
  ;; Indentation
  (setq typescript-indent-level 2)

  ;; Formatting
  (setq-hook! 'typescript-mode-hook +format-with 'prettier))

;; ====================================
;; WEB MODE (for JSX/TSX)
;; ====================================

;; If you enable :lang web module in init.el, configure it:
;; (after! web-mode
;;   (setq web-mode-markup-indent-offset 2
;;         web-mode-css-indent-offset 2
;;         web-mode-code-indent-offset 2
;;         web-mode-script-padding 2
;;         web-mode-style-padding 2))

;; ====================================
;; REACT/JSX SUPPORT
;; ====================================

;; For React development, you might want to enable :lang web
;; in init.el instead of just javascript module

;; (after! rjsx-mode
;;   (setq js-indent-level 2))

;; ====================================
;; LSP JAVASCRIPT/TYPESCRIPT
;; ====================================

;; LSP servers are configured automatically by Doom
;; Default: typescript-language-server for both JS and TS

;; If you want to use a different server:
;; (after! lsp-mode
;;   (setq lsp-clients-typescript-prefer-use-project-ts-server t))

;; ====================================
;; NODE.JS SETTINGS
;; ====================================

(after! nodejs-repl
  ;; Node REPL settings
  (setq nodejs-repl-command "node"))

;; ====================================
;; NPM/PACKAGE.JSON INTEGRATION
;; ====================================

;; Doom provides SPC p x to run npm scripts
;; Configure additional npm behaviors:
;; (setq npm-mode-command-prefix "C-c n")

;; ====================================
;; FORMATTING
;; ====================================

;; Prettier is the standard for JS/TS
;; Install: npm install -g prettier

;; ESLint integration (if you use it):
;; (after! flycheck
;;   (setq flycheck-javascript-eslint-executable "eslint"))

;; ====================================
;; JAVASCRIPT KEYBINDINGS
;; ====================================

(map! :after js2-mode
      :localleader
      :map js2-mode-map
      ;; Node REPL
      (:prefix ("r" . "repl")
       :desc "Send region"  "r" #'nodejs-repl-send-region
       :desc "Send buffer"  "b" #'nodejs-repl-send-buffer
       :desc "Send line"    "l" #'nodejs-repl-send-line
       :desc "Start REPL"   "s" #'nodejs-repl))

;; ====================================
;; RECOMMENDED TOOLS
;; ====================================

;; Install these globally for best experience:
;; npm install -g typescript
;; npm install -g typescript-language-server
;; npm install -g prettier
;; npm install -g eslint

;; For testing:
;; npm install -g jest

;; For React development:
;; npm install -g create-react-app
