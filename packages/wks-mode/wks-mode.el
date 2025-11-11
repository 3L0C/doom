;;; wks-mode.el --- Major mode for editing wks files -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2023-2025, 3L0C
;; Author: 3L0C ( dotbox at mailbox dot org )
;; Version: 0.1.0
;; Package-Version: 20250110.1
;; Created: December 24 2023
;; Keywords: languages, tools, wk, which-key
;; Homepage: https://github.com/3L0C/wk
;; URL: https://github.com/3L0C/wk
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Major mode for editing wks (Which-Key Source) files.
;;
;; wks files define key chord mappings for the `wk` program, a Which-Key
;; implementation for X11 and Wayland.
;;
;; Features:
;; - Syntax highlighting for chords, prefixes, commands, hooks, and macros
;; - Support for :var macro and variable interpolations
;; - Automatic indentation with configurable offset
;; - Imenu support for navigating chords, prefixes, and variables
;; - Outline-mode support for folding
;; - Interactive commands for compiling and running
;; - Completion for macros, flags, hooks, and interpolations
;; - Flymake integration for real-time syntax checking
;; - Electric pair mode for auto-pairing brackets and quotes
;;
;; Installation:
;;
;; Add wks-mode.el to your load-path and add:
;;   (require 'wks-mode)
;;
;; Or with use-package:
;;   (use-package wks-mode
;;     :mode "\\.wks\\'")
;;
;; Usage:
;;
;; Open a .wks file and the mode will activate automatically.
;;
;; Key bindings:
;;   C-c C-c - Compile current file (wk --transpile)
;;   C-c C-r - Run wk with current file
;;   C-c C-w i c - Insert chord template
;;   C-c C-w i p - Insert prefix template
;;
;; Customization:
;;
;; M-x customize-group RET wks RET
;;
;; See the wk documentation for wks syntax details:
;;   https://codeberg.org/3L0C/wk
;;
;; Acknowledgments:
;;
;; Indentation code adapted from `zig-mode' (https://github.com/ziglang/zig-mode).
;; Licensed under GPLv3. Credit to the zig-mode maintainers and contributors.
;;
;;; Code:

(require 'rx)

;;; Customization

(defgroup wks nil
  "Major mode for editing wks (Which-Key Source) files."
  :group 'languages
  :prefix "wks-")

(defcustom wks-indent-offset 4
  "Number of spaces to use for indentation in `wks-mode'."
  :type 'integer
  :group 'wks
  :safe #'integerp)

(defcustom wks-command "wk"
  "Command to use for running wk."
  :type 'string
  :group 'wks
  :safe #'stringp)

;;; Syntax Table

(defvar wks-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax table for `wks-mode'.")

;;; Syntactic Face Function

(defun wks-syntactic-face-function (state)
  "Return syntactic face given STATE.
STATE is a `parse-partial-sexp' state, and the returned face is the face to
apply to the character at point."
  (cond
   ((nth 3 state)
    ;; In a string
    font-lock-string-face)
   ((nth 4 state)
    ;; In a comment - return comment face with high priority
    font-lock-comment-face)
   (t
    ;; Otherwise, no face
    nil)))

(defun wks--match-unless-comment (regex limit)
  "Match REGEX up to LIMIT but only if not in a comment.
This allows matching in strings and normal code, but skips comments
to prevent syntax elements from being highlighted in comments."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward regex limit t))
      (unless (nth 4 (syntax-ppss))  ; nil if not in comment
        (setq found t)))
    found))

;;; Font Lock - Helper Functions

(defun wks--font-lock-commands ()
  "Font-lock patterns for wks commands."
  `(
    ;; Commands - with `%{{' and `}}' delimiters.
    (,(rx (seq (group "%{{")
               (group (*? (not (any "\n"))))
               (group "}}")))
     (1 font-lock-builtin-face keep)
     (2 'default keep)
     (3 font-lock-builtin-face keep))
    ;; Commands - with `%((' and `))' delimiters.
    (,(rx (seq (group "%((")
               (group (*? (not (any "\n"))))
               (group "))")))
     (1 font-lock-builtin-face keep)
     (2 'default keep)
     (3 font-lock-builtin-face keep))
    ;; Commands - with `%[[' and `]]' delimiters.
    (,(rx (seq (group "%[[")
               (group (*? (not (any "\n"))))
               (group "]]")))
     (1 font-lock-builtin-face keep)
     (2 'default keep)
     (3 font-lock-builtin-face keep))
    ;; Commands - with single-char arbitrary delimiter (e.g., %||...||, %##...##)
    ;; Note: Simplified to avoid catastrophic backtracking
    (,(rx (seq (group "%")
               (group (seq (group (any "!" "#" "$" "&" "'" "*" "+" "," "-" "."
                                       "/" ":" ";" "<" "=" ">" "?" "@" "\\" "^"
                                       "_" "`" "|" "~"))
                           (backref 3)))
               (group (*? (not (any "\n"))))
               (group (backref 2))))
     (1 font-lock-builtin-face keep)
     (2 font-lock-builtin-face keep)
     (4 'default keep)
     (5 font-lock-builtin-face keep))))

(defun wks--font-lock-keywords ()
  "Font-lock patterns for wks keywords (hooks and flags)."
  `(
    ;; Hooks
    (,(rx (seq (group "^")
               (group (or "before" "after" "sync-before" "sync-after"))))
     (1 'default)
     (2 font-lock-keyword-face))
    ;; Flags
    (,(rx (seq (group "+")
               (group (or "keep" "close" "inherit" "execute"
                          "ignore" "ignore-sort" "unhook"
                          "deflag" "no-before" "no-after"
                          "write" "sync-command"))))
     (1 'default)
     (2 font-lock-keyword-face))))

(defun wks--font-lock-macros ()
  "Font-lock patterns for wks preprocessor macros."
  `(
    ;; Preprocessor switch macros (no arguments)
    (,(rx (seq ":"
               (group (or "debug" "sort" "top" "bottom"))
               (or space eol)))
     (1 font-lock-preprocessor-face))

    ;; Special handling for :var - highlight variable name distinctly
    (,(rx (seq ":"
               (group "var")
               (one-or-more space)
               (group (seq "\""
                          (zero-or-more (or (seq "\\" anything)
                                           (not (any "\""))))
                          "\""))
               (one-or-more space)
               (group (seq "\""
                          (zero-or-more (or (seq "\\" anything)
                                           (not (any "\""))))
                          "\""))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-variable-name-face t)
     (3 font-lock-string-face t))

    ;; Other string macros
    (,(rx (seq ":"
               (group (or "include" "implicit-array-keys"
                         "fg-key" "fg-delimiter" "fg-prefix"
                         "fg-chord" "fg" "bg" "bd" "shell" "font"))
               (one-or-more space)
               (group (seq "\""
                          (zero-or-more (or (seq "\\" anything)
                                           (not (any "\""))))
                          "\""))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-string-face))

    ;; Integer macros (including negative)
    (,(rx (seq ":"
               (group (or "menu-width" "menu-gap"))
               (one-or-more space)
               (group (seq (zero-or-one "-") (one-or-more digit)))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))

    ;; Positive integer macros
    (,(rx (seq ":"
               (group (or "max-columns" "border-width" "width-padding"
                         "height-padding" "delay"))
               (one-or-more space)
               (group (one-or-more digit))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))

    ;; Floating-point macros
    (,(rx (seq ":"
               (group "border-radius")
               (one-or-more space)
               (group (seq (one-or-more digit)
                          (zero-or-more (seq "." (one-or-more digit)))))))
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))))

(defun wks--font-lock-interpolations ()
  "Font-lock patterns for wks interpolations.
NOTE: Order matters! Builtin interpolations MUST come before user-defined
variables."
  `(
    ;; Builtin interpolations - these MUST be first, use prepend to take priority
    ;; Use matcher function to skip comments while highlighting in strings
    ((lambda (limit)
       (wks--match-unless-comment
        (rx (group (seq "%("
                       (or "key"
                           "index+1" "index"
                           "desc^^" "desc^"
                           "desc,," "desc,"
                           "desc")
                       ")")))
        limit))
     (1 font-lock-constant-face prepend))

    ;; User-defined variable interpolations - catch-all for %(anything-not-builtin)
    ;; Constrained to not cross newlines to prevent runaway highlighting
    ;; Use matcher function to skip comments while highlighting in strings
    ((lambda (limit)
       (wks--match-unless-comment
        (rx (group (seq "%(" (one-or-more (not (any ")" "\n"))) ")")))
        limit))
     (1 font-lock-variable-name-face prepend))))

(defun wks--font-lock-special-keys ()
  "Font-lock patterns for special keys and chord arrays."
  `(
    ;; Chord array trigger - three dots
    (,(rx (seq line-start
               (zero-or-more space)
               (group "...")
               (one-or-more space)))
     (1 font-lock-keyword-face))

    ;; Chord array trigger - [keys]
    (,(rx (seq line-start
               (zero-or-more space)
               (group "[" (one-or-more (not (any "]" "\n"))) "]")
               (one-or-more space)))
     (1 font-lock-keyword-face))

    ;; Single-character trigger keys (any non-whitespace UTF-8 character)
    ;; This includes ASCII letters, digits, punctuation, and UTF-8 codepoints
    ;; Must come before special keys pattern to avoid conflicts
    (,(rx (seq line-start
               (zero-or-more space)
               ;; Optional modifiers (highlighted separately by modifier pattern)
               (zero-or-more (or "C-" "M-" "H-" "S-"))
               ;; Capture any single non-whitespace character
               (group (not (any space "\t" "\n")))
               (one-or-more space)
               "\""))
     (1 font-lock-constant-face))

    ;; Special keys (TAB, SPC, F1-F35, arrow keys, etc.)
    (,(rx (seq word-boundary
               (or "TAB" "SPC" "RET" "DEL" "ESC" "Home" "End" "Begin"
                   "PgUp" "PgDown" "Left" "Right" "Up" "Down"
                   "VolDown" "VolMute" "VolUp" "Play" "Stop" "Prev" "Next"
                   (seq "F" (one-or-more digit)))
               word-boundary))
     (0 font-lock-constant-face))

    ;; Key modifiers
    (,(rx (or "C-" "M-" "H-" "S-"))
     (0 font-lock-keyword-face))))

(defun wks--font-lock-strings ()
  "Font-lock patterns for strings."
  `(
    ;; Strings (descriptions and other quoted text)
    ;; Use :keep to not override interpolations already highlighted
    (,(rx (seq "\""
               (zero-or-more (or (seq "\\" anything)
                                (not (any "\""))))
               "\""))
     (0 font-lock-string-face :keep))))

(defun wks--font-lock-misc ()
  "Font-lock patterns for miscellaneous syntax."
  `(
    ;; Escaped special characters
    ;; Use matcher function to skip comments while highlighting in strings
    ((lambda (limit)
       (wks--match-unless-comment
        (rx (seq "\\" (or "\\" "[" "]" "{" "}" "#" "\"" ":" "^" "+" "(" ")")))
        limit))
     (0 font-lock-warning-face prepend))

    ;; Delimiters
    (,(rx (or "{" "}" "[" "]" "(" ")"))
     (0 font-lock-builtin-face))))

;;; Font Lock Keywords - Three Levels

(defconst wks-font-lock-keywords-1
  (append
   (wks--font-lock-misc))
  "Minimal highlighting for wks mode.")

(defconst wks-font-lock-keywords-2
  (append
   (wks--font-lock-commands)
   (wks--font-lock-keywords)
   (wks--font-lock-macros)
   (wks--font-lock-interpolations)
   (wks--font-lock-strings)
   wks-font-lock-keywords-1)
  "Medium highlighting for wks mode.")

(defconst wks-font-lock-keywords-3
  (append
   (wks--font-lock-special-keys)
   wks-font-lock-keywords-2)
  "Maximum highlighting for wks mode.")

(defvar wks-font-lock-keywords wks-font-lock-keywords-3
  "Default highlighting for wks mode.")

;;; Indentation

(defun wks-paren-nesting-level ()
  "Return the paren nesting level for `wks-mode'."
  (nth 0 (syntax-ppss)))

(defun wks-currently-in-str ()
  "Return non-nil if point is inside a string in `wks-mode'."
  (nth 3 (syntax-ppss)))

(defun wks-start-of-current-str-or-comment ()
  "Return position of start of current string or comment in `wks-mode'."
  (nth 8 (syntax-ppss)))

(defun wks-skip-backwards-past-whitespace-and-comments ()
  "Skip backwards past whitespace and comments in `wks-mode'."
  (while (or
          ;; If inside a comment, jump to start of comment.
          (let ((start (wks-start-of-current-str-or-comment)))
            (and start
                 (not (wks-currently-in-str))
                 (goto-char start)))
          ;; Skip backwards past whitespace and comment end delimiters.
          (/= 0 (skip-syntax-backward " #")))))

(defun wks-mode-indent-line ()
  "Indent current line for wks syntax.

Indentation rules:
- Base level is 0 (no indentation at top level)
- Each nested {} [] () block adds `wks-indent-offset' spaces
- Closing brackets align with the indentation of their opening bracket's line
- Comments maintain the previous line's indentation

This function is adapted from `zig-mode'."
  (interactive)
  (let ((indent-col
         (save-excursion
           (back-to-indentation)
           (let* ((paren-level (wks-paren-nesting-level))
                  (prev-block-indent-col
                   (if (<= paren-level 0) 0
                     (save-excursion
                       (while (>= (wks-paren-nesting-level) paren-level)
                         (backward-up-list)
                         (back-to-indentation))
                       (+ (current-column) wks-indent-offset)))))
             (cond
              ((looking-at "[]})]")
               (let ((matching-open-line
                      (save-excursion
                        (backward-up-list)
                        (line-number-at-pos))))
                 (if (= matching-open-line (line-number-at-pos))
                     ;; Closing bracket on the same line as opening bracket
                     (current-indentation)
                   ;; Closing bracket on a different line
                   (- prev-block-indent-col wks-indent-offset))))
              ((looking-at "#")
               ;; Comment line
               (if (bobp)
                   0
                 (save-excursion
                   (forward-line -1)
                   (current-indentation))))
              (t
               ;; Regular line
               prev-block-indent-col))))))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent-col)
      (save-excursion (indent-line-to indent-col)))))

;;; Imenu

(defvar wks-imenu-generic-expression
  '(("Chords" "^\\s-*\\([^[:space:]]+\\)\\s-+\"[^\"]+\"\\s-+.*%{{" 1)
    ("Prefixes" "^\\s-*\\([^[:space:]]+\\)\\s-+\"[^\"]+\"\\s-*\n?\\s-*{" 1)
    ("Variables" "^\\s-*:var\\s-+\"\\([^\"]+\\)\"" 1))
  "Imenu generic expression for wks mode.")

;;; Interactive Commands

(defun wks-insert-chord ()
  "Insert a chord template at point."
  (interactive)
  (insert "KEY \"Description\" %{{command}}")
  (backward-char 12))

(defun wks-insert-prefix ()
  "Insert a prefix template at point."
  (interactive)
  (insert "KEY \"Prefix\"\n{\n    \n}")
  (forward-line -1)
  (indent-according-to-mode))

(defun wks-compile ()
  "Compile the current wks file with wk --transpile."
  (interactive)
  (if buffer-file-name
      (compile (format "%s --transpile %s"
                       wks-command
                       (shell-quote-argument buffer-file-name)))
    (message "Buffer is not visiting a file")))

(defun wks-run ()
  "Run wk with the current file as key chords."
  (interactive)
  (if buffer-file-name
      (compile (format "%s --key-chords %s"
                       wks-command
                       (shell-quote-argument buffer-file-name)))
    (message "Buffer is not visiting a file")))

;;; Completion

(defun wks-completion-at-point ()
  "Completion at point function for wks mode."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            (cond
             ;; After :
             ((save-excursion
                (goto-char (car bounds))
                (or (eq (char-before) ?:)
                    (eq (char-after) ?:)))
              '("include" "var" "fg-key" "fg-delimiter" "fg-prefix" "fg-chord"
                "fg" "bg" "bd" "shell" "font" "debug" "sort" "top" "bottom"
                "menu-width" "menu-gap" "max-columns" "border-width"
                "width-padding" "height-padding" "delay" "border-radius"
                "implicit-array-keys"))
             ;; After +
             ((save-excursion
                (goto-char (car bounds))
                (or (eq (char-before) ?+)
                    (eq (char-after) ?+)))
              '("keep" "close" "inherit" "execute" "ignore" "ignore-sort"
                "unhook" "deflag" "no-before" "no-after" "write" "sync-command"))
             ;; After ^
             ((save-excursion
                (goto-char (car bounds))
                (or (eq (char-before) ?^)
                    (eq (char-after) ?^)))
              '("before" "after" "sync-before" "sync-after"))
             ;; After %(
             ((save-excursion
                (goto-char (car bounds))
                (and (>= (point) 2)
                     (equal (buffer-substring (- (point) 2) (point)) "%(")))
              '("key" "index" "index+1" "desc" "desc^" "desc^^"
                "desc," "desc,,"))
             (t nil))
            :exclusive 'no))))

;;; Flymake

(defun wks-flymake (report-fn &rest _args)
  "Flymake backend for wks mode using wk --transpile.
REPORT-FN is a callback function to report diagnostics."
  (unless (executable-find wks-command)
    (error "Cannot find wk command: %s" wks-command))
  (let* ((source (current-buffer))
         (temp-file (make-temp-file "wks-flymake" nil ".wks")))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'silent))
    (make-process
     :name "wks-flymake"
     :buffer (generate-new-buffer " *wks-flymake*")
     :command (list wks-command "--transpile" temp-file)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (if (buffer-live-p source)
                 (with-current-buffer (process-buffer proc)
                   (goto-char (point-min))
                   (let (diags)
                     (while (search-forward-regexp
                             "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
                             nil t)
                       (let ((line (string-to-number (match-string 2)))
                             (col (string-to-number (match-string 3)))
                             (msg (match-string 4)))
                         (with-current-buffer source
                           (save-excursion
                             (goto-char (point-min))
                             (forward-line (1- line))
                             (move-to-column col)
                             (push (flymake-make-diagnostic
                                    source
                                    (point)
                                    (line-end-position)
                                    :error
                                    msg)
                                   diags)))))
                     (funcall report-fn (nreverse diags))))
               (flymake-log :warning "Canceling obsolete check %s" proc))
           (kill-buffer (process-buffer proc))
           (delete-file temp-file)))))))

;;; Keymap

(defvar wks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'wks-compile)
    (define-key map (kbd "C-c C-r") #'wks-run)
    (define-key map (kbd "C-c C-w i c") #'wks-insert-chord)
    (define-key map (kbd "C-c C-w i p") #'wks-insert-prefix)
    map)
  "Keymap for `wks-mode'.")

;;; Define Major Mode

;;;###autoload
(define-derived-mode wks-mode fundamental-mode "wks"
  "Major mode for editing wks (Which-Key Source) files.

\\{wks-mode-map}"
  :syntax-table wks-mode-syntax-table
  :group 'wks

  ;; Font lock
  (setq-local font-lock-defaults
              '((wks-font-lock-keywords-1
                 wks-font-lock-keywords-2
                 wks-font-lock-keywords-3)
                nil nil nil nil
                (font-lock-syntactic-face-function . wks-syntactic-face-function)))

  ;; Indentation
  (setq-local indent-line-function #'wks-mode-indent-line)

  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)

  ;; Imenu
  (setq-local imenu-generic-expression wks-imenu-generic-expression)

  ;; Outline mode
  (setq-local outline-regexp "^\\s-*\\([^[:space:]]+\\)\\s-+\"[^\"]+\"\\s-*{")
  (setq-local outline-level (lambda () 1))

  ;; Completion
  (add-hook 'completion-at-point-functions #'wks-completion-at-point nil t)

  ;; Flymake
  (when (executable-find wks-command)
    (add-hook 'flymake-diagnostic-functions #'wks-flymake nil t))

  ;; Electric pairs
  (setq-local electric-pair-pairs
              '((?{ . ?})
                (?\[ . ?\])
                (?\( . ?\))
                (?\" . ?\")))
  (electric-pair-local-mode 1))

;; Associate `.wks' extension with `wks-mode'.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wks\\'" . wks-mode))

;; Add the mode to the `features' list
(provide 'wks-mode)

;;; wks-mode.el ends here
