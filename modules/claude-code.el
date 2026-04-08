;;; modules/claude-code.el -*- lexical-binding: t; -*-
;;
;; Claude Code IDE: MCP bridge, custom tools, REPL integration
;; https://github.com/manzaltu/claude-code-ide.el

;; ============================================================================
;; HELPER FUNCTIONS (slash commands & workflow composites)
;; ============================================================================

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
  "Send the selected region as context, then prompt for an instruction."
  (interactive)
  (when (use-region-p)
    (claude-code-ide-insert-at-mentioned))
  (call-interactively #'claude-code-ide-send-prompt))

(defun my/claude-code-fix-errors ()
  "Ask Claude to fix diagnostics errors in the current buffer."
  (interactive)
  (let ((file (or (buffer-file-name) (buffer-name))))
    (my/claude-code--send-to-terminal
     (format "Fix the errors and warnings in %s"
             (file-name-nondirectory file)))))

(defun my/claude-code-send-file-ref ()
  "Send the current buffer's file path as an @-reference to Claude."
  (interactive)
  (if-let* ((file (buffer-file-name)))
      (my/claude-code--send-to-terminal (concat "@" file))
    (user-error "Buffer is not visiting a file")))

;; ============================================================================
;; EMBARK INTEGRATION (Claude actions in context menus)
;; ============================================================================

;; Doom's vertico module binds SPC a to embark-act, which conflicts with
;; our AI prefix. Move embark-act to SPC ; (mnemonic: C-; is embark-act
;; globally, so SPC ; mirrors it under leader). C-; still works everywhere.
(map! :leader "a" nil)
(after! embark
  (map! :leader
        "a" nil
        :desc "Embark act" ";" #'embark-act)

  ;; --- Embark → Claude actions ---
  ;; Press C-; on any target, then C to send it to Claude.

  (defun my/claude-embark-explain-symbol (symbol)
    "Ask Claude to explain SYMBOL in the codebase."
    (my/claude-code--send-to-terminal
     (format "Explain what `%s` does in this codebase. Show me where it's defined and how it's used." symbol)))

  (defun my/claude-embark-send-region (start end)
    "Send the region between START and END to Claude with an instruction prompt."
    (let* ((text (buffer-substring-no-properties start end))
           (instruction (read-string "Instruction for Claude: ")))
      (my/claude-code--send-to-terminal
       (format "%s\n\n```\n%s\n```" instruction text))))

  (defun my/claude-embark-send-file (file)
    "Send FILE as an @-reference to Claude."
    (my/claude-code--send-to-terminal (concat "@" (expand-file-name file))))

  (defun my/claude-embark-review-defun ()
    "Send the function at point to Claude for review."
    (interactive)
    (let ((defun-text (save-excursion
                        (mark-defun)
                        (buffer-substring-no-properties (region-beginning) (region-end)))))
      (deactivate-mark)
      (my/claude-code--send-to-terminal
       (format "Review this function for bugs, clarity, and improvements:\n\n```\n%s\n```" defun-text))))

  (defun my/claude-embark-fix-error (diagnostic)
    "Send DIAGNOSTIC error text to Claude to fix."
    (my/claude-code--send-to-terminal
     (format "Fix this error in %s: %s"
             (file-name-nondirectory (or (buffer-file-name) ""))
             diagnostic)))

  ;; Register Claude actions in embark target maps.
  ;; C = Claude in every embark menu.
  (define-key embark-symbol-map     "C" #'my/claude-embark-explain-symbol)
  (define-key embark-identifier-map "C" #'my/claude-embark-explain-symbol)
  (define-key embark-region-map     "C" #'my/claude-embark-send-region)
  (define-key embark-file-map       "C" #'my/claude-embark-send-file)
  (define-key embark-defun-map      "C" #'my/claude-embark-review-defun)
  (define-key embark-general-map    "C" #'my/claude-embark-review-defun))

;; ============================================================================
;; PACKAGE CONFIGURATION
;; ============================================================================

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
        ;; CLAUDE_CODE_NO_FLICKER=1 redraws screen in-place, which destroys
        ;; vterm scrollback completely. The Emacs-side anti-flicker handles
        ;; flicker without this tradeoff.
        claude-code-ide-no-flicker nil
        claude-code-ide-prevent-reflow-glitch t)

  ;; 100K lines of scrollback so Claude responses aren't lost.
  ;; Enter vterm-copy-mode (C-c C-t) to scroll freely.
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

  ;; --- Security: executeCode is disabled. Custom MCP tools below provide
  ;; window management and REPL interaction without full Elisp eval. ---
  (setq claude-code-ide-enable-execute-code nil)

  ;; --- MCP Emacs tools ---
  (setq claude-code-ide-diagnostics-backend 'auto)
  (claude-code-ide-emacs-tools-setup)

  ;; ========================================================================
  ;; CUSTOM MCP TOOLS: Window management
  ;; ========================================================================

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

  ;; ========================================================================
  ;; CUSTOM MCP TOOLS: CIDER REPL interaction
  ;; ========================================================================

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

  ;; --- MCP allowed tools ---
  (setq claude-code-ide-mcp-allowed-tools 'auto)

  ;; --- System prompt: nil globally, set per-project via .dir-locals.el ---
  (setq claude-code-ide-system-prompt nil)

  ;; --- CLI flags ---
  (setq claude-code-ide-cli-extra-flags "")

  ;; --- Debug ---
  (setq claude-code-ide-debug nil
        claude-code-ide-log-with-context t))

;;; modules/claude-code.el ends here
