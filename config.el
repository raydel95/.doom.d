;;; config.el -*- lexical-binding: t; -*-
;;
;; Optimized Doom Emacs configuration for M4 Mac + Clojure development
;; Focus: Maximum performance + LSP-first approach (works without REPL)

;; ============================================================================
;; BASIC SETTINGS
;; ============================================================================

(setq user-full-name "Raydel Alonso"
      user-mail-address "raydel.alonso@amperity.com")

;; Theme and appearance
(setq doom-theme 'doom-sourcerer
      doom-font (font-spec :family "SauceCodePro Nerd Font" :size 14)
      display-line-numbers-type 'relative)

;; Localleader key (Spacemacs-style)
(setq doom-localleader-key ",")

;; ============================================================================
;; PERFORMANCE TUNING (Maximized for M4)
;; ============================================================================

;; LSP performance - Emacs 30 default is 64KB, bump for large LSP responses
(setq read-process-output-max (* 3 1024 1024)) ; 3MB for LSP

;; File handling
(setq large-file-warning-threshold (* 100 1024 1024)  ; Warn for files > 100MB
      so-long-threshold 1000)                          ; Long line detection

;; Compilation
(setq compilation-scroll-output t)

;; Native compilation (M4 specific)
;; native-comp-jit-compilation replaces the old native-comp-deferred-compilation.
(when (native-comp-available-p)
  (setq native-comp-async-jobs-number 8
        native-comp-async-report-warnings-errors nil
        native-comp-jit-compilation t))

;; ============================================================================
;; LSP-MODE CONFIGURATION (Optimized for M4)
;; ============================================================================

;; NOTE: LSP_USE_PLISTS=true is set in .zshrc and Doom's env file.
;; It must be an env var (read at compile time), NOT a setq.

(after! lsp-mode
  ;; Performance
  (setq lsp-log-io nil

        ;; UI features (M4 can handle them)
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        lsp-signature-render-documentation t
        lsp-semantic-tokens-enable t
        lsp-enable-symbol-highlighting t

        ;; File watchers - high threshold for monorepo (1237 source paths)
        ;; Append project-specific ignores to lsp-mode's ~30 default patterns
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 50000

        ;; Enable full project analysis for references
        lsp-enable-xref t
        lsp-enable-imenu t
        lsp-enable-snippet t

        ;; Keep LSP servers alive when all buffers are closed.
        ;; Without this, closing all .clj buffers kills clojure-lsp
        ;; and the next open triggers a full 1-2 min re-index.
        lsp-keep-workspace-alive t)

  ;; Append monorepo-specific ignore patterns (don't replace defaults)
  (dolist (pattern '("[/\\\\]\\.cache$"
                     "[/\\\\]\\.clj-kondo$"
                     "[/\\\\]\\.shadow-cljs$"
                     "[/\\\\]resources/public/js/compiled$"))
    (add-to-list 'lsp-file-watch-ignored-directories pattern))

  ;; Auto-save after LSP rename operations
  (defun my/save-project-after-lsp-rename (&rest _)
    "Save all project buffers after an LSP rename."
    (projectile-save-project-buffers))
  (advice-add #'lsp-rename :after #'my/save-project-after-lsp-rename))

;; Force LSP to use monorepo root for lein-monolith projects
(defun my/find-lein-monolith-root (dir)
  "Find lein-monolith root by looking for project.clj with lein-monolith plugin."
  (let ((current dir)
        (found nil))
    (while (and current (not found))
      (let ((project-file (expand-file-name "project.clj" current)))
        (when (file-exists-p project-file)
          (with-temp-buffer
            (insert-file-contents project-file)
            (when (search-forward "lein-monolith" nil t)
              (setq found current))))
        (setq current (file-name-directory (directory-file-name current)))
        (when (string= current "/") (setq current nil))))
    found))

(after! lsp-mode
  (defun my/lsp-clojure-project-root-override (orig-fun &rest args)
    "Use monorepo root if lein-monolith is detected."
    (or (my/find-lein-monolith-root default-directory)
        (apply orig-fun args)))

  (advice-add 'lsp-clojure--get-project-root :around #'my/lsp-clojure-project-root-override))

(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-max-width 60
        lsp-ui-peek-list-width 60
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-delay 0.5))

;; ============================================================================
;; CLOJURE CONFIGURATION (LSP + CIDER)
;; ============================================================================

(after! clojure-mode
  (setq clojure-indent-style 'align-arguments
        clojure-thread-all-but-last t
        clojure-align-forms-automatically t))

;; CIDER Configuration
;; Note: Doom's (clojure +lsp) already sets cider-eldoc-display-for-symbol-at-point
;; to nil and cider-font-lock-dynamically to nil. Don't override those.
(after! cider
  (setq cider-ns-refresh-show-log-buffer t
        cider-show-error-buffer t
        cider-prompt-for-symbol nil

        ;; REPL behavior
        cider-repl-display-help-banner nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-pretty-printing t
        cider-repl-result-prefix ";; => "
        cider-repl-wrap-history t
        cider-repl-history-size 3000

        ;; Evaluation
        cider-auto-select-error-buffer t
        cider-show-eval-spinner nil
        cider-invert-insert-eval-p t

        ;; Testing
        cider-test-show-report-on-success t
        cider-auto-test-mode nil

        ;; Inspector
        cider-inspector-fill-frame t

        ;; Stacktraces
        cider-stacktrace-fill-column 80

        ;; Daemon mode: reuse dead REPLs instead of prompting
        cider-reuse-dead-repls 'auto)

  ;; Popup rules — :ttl nil prevents Doom from killing the REPL buffer on timeout
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil :ttl nil :size 0.3)
  (set-popup-rule! "^\\*cider-error" :side 'bottom :quit t :size 0.3)
  (set-popup-rule! "^\\*cider-doc" :side 'right :quit t :size 0.5)

  ;; Prefer LSP for navigation (works without REPL!)
  (set-lookup-handlers! 'cider-mode nil)

  ;; Fix: Re-enable cider-mode after REPL disconnect.
  ;; When a REPL dies or is quit, cider-mode (minor mode) can be stripped from
  ;; Clojure buffers, removing all CIDER keybindings and eval commands.
  ;; This hook re-enables it so you can reconnect without manually toggling.
  ;; See: https://github.com/clojure-emacs/cider/issues/3346
  (add-hook 'cider-disconnected-hook
            (defun my/cider-reenable-mode-after-disconnect ()
              "Re-enable cider-mode in Clojure buffers after REPL disconnect."
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when (and (derived-mode-p 'clojure-mode 'clojurescript-mode 'clojure-ts-mode)
                             (not cider-mode))
                    (cider-mode +1)))))))

;; Lispyville for vim-style Lisp editing
;; Note: lispyville is unmaintained since July 2022 but still functional.
;; No clear replacement exists as of 2026.
(use-package! lispyville
  :hook ((clojure-mode       . lispyville-mode)
         (emacs-lisp-mode    . lispyville-mode)
         (common-lisp-mode   . lispyville-mode)
         (scheme-mode        . lispyville-mode))
  :config
  (lispyville-set-key-theme
   '(additional
     additional-insert
     (additional-movement normal visual motion)
     (additional-wrap normal insert)
     (atom-movement normal visual)
     c-w
     c-u
     (commentary normal visual)
     escape
     (operators normal)
     (prettify insert)
     slurp/barf-cp)))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

;; Clojure-specific keybindings
(map! :map (clojure-mode-map clojurescript-mode-map)
      :localleader
      ;; Eval
      (:prefix ("e" . "eval")
       :desc "Eval last sexp"          "e" #'cider-eval-last-sexp
       :desc "Eval defun"              "f" #'cider-eval-defun-at-point
       :desc "Eval buffer"             "b" #'cider-eval-buffer
       :desc "Eval region"             "r" #'cider-eval-region
       :desc "Eval defun to comment"   ";" #'cider-eval-defun-to-comment
       :desc "Eval last sexp to comment" ":" #'cider-eval-last-sexp-to-comment
       (:prefix ("p" . "pprint")
        :desc "Pprint last sexp to comment" ":" #'cider-pprint-eval-last-sexp-to-comment
        :desc "Pprint defun to comment"    ";" #'cider-pprint-eval-defun-to-comment
        :desc "Pprint defun at point"      "d" #'cider-pprint-eval-defun-at-point
        :desc "Pprint last sexp"           "e" #'cider-pprint-eval-last-sexp))

      ;; REPL
      (:prefix ("r" . "repl")
       :desc "Jack in"                 "j" #'cider-jack-in-clj
       :desc "Jack in ClojureScript"   "J" #'cider-jack-in-cljs
       :desc "Connect"                 "c" #'cider-connect-clj
       :desc "Quit REPL"               "q" #'cider-quit
       :desc "Clear REPL"              "l" #'cider-repl-clear-buffer
       :desc "Switch to REPL"          "s" #'cider-switch-to-repl-buffer
       :desc "Refresh namespace"       "n" #'cider-ns-refresh)

      ;; Testing
      (:prefix ("t" . "test")
       :desc "Run test at point"       "t" #'cider-test-run-test
       :desc "Run namespace tests"     "n" #'cider-test-run-ns-tests
       :desc "Run all tests"           "a" #'cider-test-run-project-tests
       :desc "Rerun failed tests"      "r" #'cider-test-rerun-failed-tests
       :desc "Show test report"        "l" #'cider-test-show-report)

      ;; Debug
      (:prefix ("d" . "debug")
       :desc "Debug defun"             "d" #'cider-debug-defun-at-point
       :desc "Toggle breakpoint"       "b" #'cider-debug-toggle-break
       :desc "Inspect value"           "i" #'cider-inspect-last-result)

      ;; Format
      (:prefix ("=" . "format")
       :desc "Format buffer"           "=" #'cider-format-buffer
       :desc "Format defun"            "f" #'cider-format-defun
       :desc "Align forms"             "l" #'clojure-align
       :desc "Format region"           "r" #'cider-format-region)

      ;; Namespace
      (:prefix ("n" . "namespace")
       :desc "Eval ns form"            "n" #'cider-eval-ns-form
       :desc "Refresh namespace"       "r" #'cider-ns-refresh
       :desc "Load file"               "l" #'cider-load-file)

      ;; Documentation
      (:prefix ("h" . "help")
       :desc "Show doc"                "d" #'cider-doc
       :desc "Javadoc"                 "j" #'cider-javadoc
       :desc "Clojuredocs"             "c" #'cider-clojuredocs
       :desc "Apropos"                 "a" #'cider-apropos))

;; ============================================================================
;; GO CONFIGURATION (LSP with gopls)
;; ============================================================================

(after! go-mode
  ;; Format on save with goimports (includes gofmt + import management)
  (setq gofmt-command "goimports")

  ;; Auto-format before save + buffer-local tab-width
  (defun my/go-mode-setup-h ()
    "Set buffer-local tab-width and enable goimports on save."
    (setq-local tab-width 4)
    (add-hook 'before-save-hook #'gofmt-before-save nil 'local))
  (add-hook 'go-mode-hook #'my/go-mode-setup-h))

(after! lsp-go
  (setq lsp-go-gopls-server-path "gopls"

        ;; Analysis settings
        lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false)
                          (unusedparams . t)
                          (unusedwrite . t)
                          (useany . t))

        ;; Codelens
        lsp-go-codelenses '((gc_details . t)
                            (generate . t)
                            (regenerate_cgo . t)
                            (test . t)
                            (tidy . t)
                            (upgrade_dependency . t)
                            (vendor . t))

        ;; Build flags
        lsp-go-build-flags ["-tags=integration"]

        ;; Completion
        lsp-go-use-placeholders t
        lsp-go-complete-unimported t

        ;; Inlay hints
        lsp-go-hover-kind "FullDocumentation"
        lsp-inlay-hint-enable t))

;; Go-specific keybindings
(map! :map go-mode-map
      :localleader
      (:prefix ("i" . "imports")
       :desc "Add import"       "a" #'go-import-add
       :desc "Remove unused"    "d" #'go-remove-unused-imports
       :desc "Goto imports"     "g" #'go-goto-imports)

      (:prefix ("t" . "test")
       :desc "Test function"    "t" #'go-test-current-test
       :desc "Test file"        "f" #'go-test-current-file
       :desc "Test project"     "p" #'go-test-current-project
       :desc "Run tests"        "r" #'go-run)

      (:prefix ("g" . "goto")
       :desc "Goto imports"     "i" #'go-goto-imports
       :desc "Goto method receiver" "m" #'go-goto-method-receiver
       :desc "Goto function"    "f" #'go-goto-function
       :desc "Goto arguments"   "a" #'go-goto-arguments
       :desc "Goto return values" "r" #'go-goto-return-values))

;; ============================================================================
;; DAEMON MODE
;; ============================================================================

;; Prevent "Do you want to restart?" prompts for every LSP server on quit
(setq confirm-kill-processes nil)

;; Fix lsp-deferred race condition in daemon mode.
;; lsp-deferred checks buffer visibility via get-buffer-window, but in daemon
;; mode the idle timer can fire before the emacsclient frame is fully set up,
;; causing lsp() to never be called. This hook retries initialization for any
;; buffers that are still deferred after a new frame is created.
;; See: https://github.com/emacs-lsp/lsp-mode/issues/4190
(when (daemonp)
  (defun my/lsp-retry-deferred-buffers-h ()
    "Retry LSP initialization for buffers that were deferred before the frame existed."
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (bound-and-true-p lsp--buffer-deferred)
                   (not (bound-and-true-p lsp-mode))
                   (buffer-file-name))
          (lsp)))))
  (add-hook 'server-after-make-frame-hook #'my/lsp-retry-deferred-buffers-h))

;; ============================================================================
;; CLAUDE CODE IDE (MCP bridge to Claude Code CLI)
;; https://github.com/manzaltu/claude-code-ide.el
;; ============================================================================

;; --- Custom helpers: slash commands and workflow composites ---
;; These use the vterm public API and the package's buffer naming convention
;; (*claude-code[project]*) to send text to the Claude terminal.

(defun my/claude-code--send-to-terminal (text)
  "Send TEXT to the active Claude Code terminal and press return."
  (when-let* ((buf (seq-find
                    (lambda (b)
                      (and (string-prefix-p "*claude-code" (buffer-name b))
                           (get-buffer-process b)))
                    (buffer-list))))
    (with-current-buffer buf
      (vterm-send-string text)
      (vterm-send-return))))

(defun my/claude-code-undo ()
  "Send /undo to roll back Claude's last file changes."
  (interactive)
  (my/claude-code--send-to-terminal "/undo"))

(defun my/claude-code-clear ()
  "Send /clear to reset the Claude conversation context."
  (interactive)
  (my/claude-code--send-to-terminal "/clear"))

(defun my/claude-code-compact ()
  "Send /compact to compress the conversation and reclaim context window."
  (interactive)
  (my/claude-code--send-to-terminal "/compact"))

(defun my/claude-code-instruct-region ()
  "Send the selected region as context, then prompt for an instruction.
Chains `insert-at-mentioned' (MCP context notification) with `send-prompt'
\(minibuffer instruction)."
  (interactive)
  (when (use-region-p)
    (claude-code-ide-insert-at-mentioned))
  (call-interactively #'claude-code-ide-send-prompt))

(defun my/claude-code-fix-errors ()
  "Ask Claude to fix diagnostics errors in the current buffer.
Claude uses the getDiagnostics MCP tool to pull Flycheck/Flymake data."
  (interactive)
  (let ((file (or (buffer-file-name) (buffer-name))))
    (my/claude-code--send-to-terminal
     (format "Fix the errors and warnings in %s"
             (file-name-nondirectory file)))))

(defun my/claude-code-send-file-ref ()
  "Send the current buffer's file path as an @-reference to Claude.
Uses Claude Code CLI's @file syntax to make Claude read the full file."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (my/claude-code--send-to-terminal (concat "@" file))
    (user-error "Buffer is not visiting a file")))

;; --- Package configuration ---

(use-package! claude-code-ide
  :commands (claude-code-ide-menu
             claude-code-ide
             claude-code-ide-toggle-recent
             claude-code-ide-insert-at-mentioned
             claude-code-ide-send-prompt
             claude-code-ide-continue
             claude-code-ide-resume
             claude-code-ide-stop
             claude-code-ide-list-sessions)
  :init
  ;; Keybindings — SPC a prefix for AI tools
  ;; Use :prefix-map to create a proper prefix keymap (avoids conflict with
  ;; general.el's non-prefix key detection).
  (map! :leader
        (:prefix-map ("a" . "AI")
         ;; Session management
         :desc "Claude menu"        "C" #'claude-code-ide-menu
         :desc "New session"        "n" #'claude-code-ide
         :desc "Continue session"   "r" #'claude-code-ide-continue
         :desc "Resume session"     "R" #'claude-code-ide-resume
         :desc "Stop session"       "q" #'claude-code-ide-stop
         :desc "List sessions"      "l" #'claude-code-ide-list-sessions
         :desc "Toggle Claude"      "c" #'claude-code-ide-toggle-recent
         ;; Send context
         :desc "Send selection"     "s" #'claude-code-ide-insert-at-mentioned
         :desc "Send prompt"        "p" #'claude-code-ide-send-prompt
         :desc "Send file ref"      "@" #'my/claude-code-send-file-ref
         :desc "Instruct region"    "i" #'my/claude-code-instruct-region
         :desc "Fix errors"         "f" #'my/claude-code-fix-errors
         ;; Slash commands
         :desc "Undo last change"   "u" #'my/claude-code-undo
         :desc "Clear conversation" "x" #'my/claude-code-clear
         :desc "Compact context"    "K" #'my/claude-code-compact))

  :config
  ;; --- Terminal backend ---
  (setq claude-code-ide-terminal-backend 'vterm
        claude-code-ide-vterm-anti-flicker t
        ;; Claude's CLAUDE_CODE_NO_FLICKER=1 redraws screen in-place, which
        ;; destroys vterm scrollback — you can't scroll up at all.
        ;; The Emacs-side vterm-anti-flicker handles flicker without this tradeoff.
        claude-code-ide-no-flicker nil
        claude-code-ide-prevent-reflow-glitch t)  ; upstream bug #1422 workaround

  ;; --- Scrollback: 100k lines so Claude responses aren't lost.
  ;; Enter vterm-copy-mode (C-c C-t) to scroll freely. ---
  (setq vterm-max-scrollback 100000)

  ;; --- Window layout ---
  (setq claude-code-ide-use-side-window t
        claude-code-ide-window-side 'right
        claude-code-ide-window-width 100
        claude-code-ide-focus-on-open t)

  ;; --- Ediff integration ---
  (setq claude-code-ide-use-ide-diff t
        claude-code-ide-focus-claude-after-ediff t
        claude-code-ide-show-claude-window-in-ediff t
        claude-code-ide-switch-tab-on-ediff t)

  ;; --- Security: executeCode lets Claude eval arbitrary Elisp in Emacs.
  ;; Flip to t once comfortable with the implications. ---
  (setq claude-code-ide-enable-execute-code nil)

  ;; --- MCP Emacs tools: xref, imenu, treesit, project-info, diagnostics ---
  (setq claude-code-ide-diagnostics-backend 'auto)
  (claude-code-ide-emacs-tools-setup)

  ;; --- Custom MCP tools: window management and navigation ---
  ;; These give Claude control over Emacs layout without full executeCode access.
  (defun my/claude-code-navigate (file_path &optional line split)
    "Open FILE_PATH in Emacs, optionally at LINE in a SPLIT window."
    (let ((split (or split "current")))
      (pcase split
        ("right" (split-window-right) (other-window 1))
        ("below" (split-window-below) (other-window 1)))
      (find-file (expand-file-name file_path))
      (when line
        (goto-char (point-min))
        (forward-line (1- (truncate line)))
        (recenter))
      (format "Opened %s%s%s"
              (file-name-nondirectory file_path)
              (if line (format " at line %d" (truncate line)) "")
              (if (not (equal split "current"))
                  (format " in %s split" split) ""))))

  (defun my/claude-code-window (action)
    "Manage Emacs windows. ACTION: split_right, split_below, close, close_others, next, previous, balance."
    (pcase action
      ("split_right" (split-window-right))
      ("split_below" (split-window-below))
      ("close" (delete-window))
      ("close_others" (delete-other-windows))
      ("next" (other-window 1))
      ("previous" (other-window -1))
      ("balance" (balance-windows))
      (_ (error "Unknown action: %s" action)))
    (format "Window '%s' done. %d windows visible." action (length (window-list))))

  (claude-code-ide-make-tool
   :function #'my/claude-code-navigate
   :name "emacs_navigate"
   :description "Open a file in Emacs, optionally in a new split window and at a specific line. Use this to show code side-by-side or jump to specific locations in the user's editor."
   :args '((:name "file_path" :type string :description "File path (absolute or project-relative)")
           (:name "line" :type number :description "Line number to jump to (1-based)" :optional t)
           (:name "split" :type string :description "Where to open: 'current' (reuse window), 'right' (vertical split), 'below' (horizontal split)" :optional t)))

  (claude-code-ide-make-tool
   :function #'my/claude-code-window
   :name "emacs_window"
   :description "Manage Emacs window layout: split, close, switch focus between panes, or balance sizes."
   :args '((:name "action" :type string
                   :description "Action: split_right, split_below, close, close_others, next, previous, balance"
                   :enum ["split_right" "split_below" "close" "close_others" "next" "previous" "balance"])))

  ;; --- Custom MCP tools: CIDER REPL interaction ---
  ;; Let Claude evaluate Clojure, check REPL status, and load files without executeCode.
  (defvar my/claude-code-repl-max-output 4000
    "Max characters of REPL output before truncation.")

  (defun my/claude-code--truncate (s)
    "Truncate S to `my/claude-code-repl-max-output' chars."
    (if (> (length s) my/claude-code-repl-max-output)
        (concat (substring s 0 my/claude-code-repl-max-output) "\n[truncated]")
      s))

  (defun my/claude-code-repl-eval (code &optional namespace)
    "Evaluate Clojure CODE via CIDER nREPL. Returns value, stdout, and errors."
    (require 'cider)
    (unless (cider-connected-p)
      (user-error "No CIDER REPL connected. Run cider-jack-in or cider-connect first"))
    (let* ((ns (or namespace (cider-current-ns)))
           (resp (cider-nrepl-sync-request:eval code nil ns))
           (val (nrepl-dict-get resp "value"))
           (out (nrepl-dict-get resp "out"))
           (err (nrepl-dict-get resp "err"))
           (parts nil))
      (when out (push (format "stdout:\n%s" (string-trim-right out)) parts))
      (if err
          (push (format "Error:\n%s" (string-trim-right err)) parts)
        (push (format "=> %s" (or val "nil")) parts))
      (my/claude-code--truncate (string-join (nreverse parts) "\n"))))

  (defun my/claude-code-repl-status ()
    "Get CIDER REPL connection status: connected, namespace, project."
    (require 'cider)
    (if (cider-connected-p)
        (let* ((repl (cider-current-repl))
               (ns (cider-current-ns))
               (repl-type (cider-repl-type repl))
               (proj (with-current-buffer repl
                       (when (bound-and-true-p cider-repl-project-dir)
                         cider-repl-project-dir))))
          (format "Connected (%s)\nNamespace: %s%s"
                  repl-type ns
                  (if proj (format "\nProject: %s" proj) "")))
      "Not connected. Start a REPL with cider-jack-in or cider-connect."))

  (defun my/claude-code-repl-load-file (file_path)
    "Load FILE_PATH into the CIDER REPL (compile + require)."
    (require 'cider)
    (unless (cider-connected-p)
      (user-error "No CIDER REPL connected. Run cider-jack-in or cider-connect first"))
    (let* ((path (expand-file-name file_path))
           (resp (cider-sync-request:load-file
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))
                  path
                  (file-name-nondirectory path)))
           (val (nrepl-dict-get resp "value"))
           (err (nrepl-dict-get resp "err")))
      (if err
          (my/claude-code--truncate (format "Load error:\n%s" (string-trim-right err)))
        (format "Loaded %s => %s" (file-name-nondirectory path) (or val "ok")))))

  (claude-code-ide-make-tool
   :function #'my/claude-code-repl-eval
   :name "emacs_repl_eval"
   :description "Evaluate a Clojure expression in the connected CIDER REPL and return the result. Use for testing code, checking values, running functions, or exploring data. Requires an active CIDER connection."
   :args '((:name "code" :type string :description "Clojure expression to evaluate")
           (:name "namespace" :type string :description "Namespace to evaluate in (default: current buffer's ns)" :optional t)))

  (claude-code-ide-make-tool
   :function #'my/claude-code-repl-status
   :name "emacs_repl_status"
   :description "Check CIDER REPL connection status: whether connected, current namespace, REPL type, and project directory."
   :args '())

  (claude-code-ide-make-tool
   :function #'my/claude-code-repl-load-file
   :name "emacs_repl_load_file"
   :description "Load and compile a Clojure file into the CIDER REPL, making its definitions available. Equivalent to C-c C-l in Emacs."
   :args '((:name "file_path" :type string :description "Path to the .clj file to load (absolute or project-relative)")))

  ;; --- MCP allowed tools: use 'auto to include all registered tools
  ;; (built-in + our custom emacs_navigate and emacs_window).
  ;; Tool names are prefixed as mcp__emacs-tools__<name> by the CLI.
  ;; An explicit list would need those full prefixed names, so 'auto is cleaner. ---
  (setq claude-code-ide-mcp-allowed-tools 'auto)

  ;; --- System prompt: nil globally, set per-project via .dir-locals.el:
  ;; ((nil . ((claude-code-ide-system-prompt . "Project-specific instructions."))))
  (setq claude-code-ide-system-prompt nil)

  ;; --- CLI flags: e.g. "--model opus --no-cache" ---
  (setq claude-code-ide-cli-extra-flags "")

  ;; --- Debug: off by default. Flip claude-code-ide-debug to t and use
  ;; claude-code-ide-show-debug to view WebSocket/MCP message logs. ---
  (setq claude-code-ide-debug nil
        claude-code-ide-log-with-context t)

  ;; --- Custom buffer naming (optional, uncomment to use):
  ;; (setq claude-code-ide-buffer-name-function
  ;;       (lambda (dir) (format "*Claude:%s*" (file-name-nondirectory (directory-file-name dir)))))
  )

;;; config.el ends here
