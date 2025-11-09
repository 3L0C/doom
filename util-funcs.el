;;; util-funcs.el -*- lexical-binding: t; -*-

(defun buffer-file-name-match-p (file-pattern)
  "Return t if buffer-file-name matches FILE-PATTERN."
  (when (stringp buffer-file-name)
    (string-match-p file-pattern buffer-file-name)))
