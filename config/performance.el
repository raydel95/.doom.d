;;; config/performance.el -*- lexical-binding: t; -*-
;;
;; Performance optimizations for Doom Emacs

;; ====================================
;; SYNTAX HIGHLIGHTING
;; ====================================

;; Ensure syntax highlighting is enabled globally
(global-font-lock-mode t)

;; ====================================
;; GARBAGE COLLECTION
;; ====================================

;; Let Doom's GCMH manage gc-cons-threshold dynamically.
;; Manual override interferes with GCMH and causes issues in daemon mode.
(after! gcmh
  (setq gcmh-high-cons-threshold (* 32 1024 1024)
        gcmh-idle-delay 'auto))

;; ====================================
;; SUBPROCESS COMMUNICATION
;; ====================================

;; Increase the amount of data Emacs reads from subprocesses (3MB)
;; This is especially important for LSP performance
(setq read-process-output-max (* 3 1024 1024))

;; ====================================
;; RENDERING OPTIMIZATIONS
;; ====================================

;; Reduce rendering/startup time by not compacting font caches
(setq inhibit-compacting-font-caches t)

;; Disable bidirectional text rendering for performance boost
;; Only enable if you need to work with RTL languages
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disable cursor line in non-selected windows for performance
(setq-default cursor-in-non-selected-windows nil)

;; ====================================
;; FILE HANDLING
;; ====================================

;; Increase large file warning threshold to 100MB
(setq large-file-warning-threshold 100000000)

;; ====================================
;; LONG LINE OPTIMIZATION
;; ====================================

;; Enable so-long mode for files with very long lines
;; This prevents Emacs from freezing on minified files
(after! so-long
  (setq so-long-threshold 1000
        so-long-max-lines 100))
