;;; modules/performance.el -*- lexical-binding: t; -*-
;;
;; Performance optimizations for M4 Mac

;; ============================================================================
;; SUBPROCESS & FILE HANDLING
;; ============================================================================

;; Increase read buffer for LSP and other subprocesses
(setq read-process-output-max (* 3 1024 1024)  ; 3MB
      large-file-warning-threshold (* 100 1024 1024)
      compilation-scroll-output t)

;; ============================================================================
;; NATIVE COMPILATION (M4)
;; ============================================================================

(when (native-comp-available-p)
  (setq native-comp-async-jobs-number 8
        native-comp-async-report-warnings-errors nil
        native-comp-jit-compilation t))

;; ============================================================================
;; GARBAGE COLLECTION
;; ============================================================================

;; Let Doom's GCMH manage gc-cons-threshold dynamically.
(after! gcmh
  (setq gcmh-high-cons-threshold (* 32 1024 1024)
        gcmh-idle-delay 'auto))

;; ============================================================================
;; RENDERING
;; ============================================================================

(setq inhibit-compacting-font-caches t)

;; Disable bidirectional text rendering (only needed for RTL languages)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disable cursor in non-selected windows
(setq-default cursor-in-non-selected-windows nil)

;; ============================================================================
;; LONG LINES
;; ============================================================================

(after! so-long
  (setq so-long-threshold 1000
        so-long-max-lines 100))

;; Ensure syntax highlighting is enabled
(global-font-lock-mode t)

;;; modules/performance.el ends here
