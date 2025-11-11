;;; wks-mode-test.el --- Tests for wks-mode -*- lexical-binding: t; -*-

;; Copyright © 2025, 3L0C

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test suite for wks-mode using ERT (Emacs Regression Test).
;;
;; Run tests with: M-x ert RET t RET
;; Or from command line: emacs -batch -l ert -l wks-mode.el -l wks-mode-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'wks-mode)
(require 'imenu)

;;; Helper Functions

(defmacro wks-test-with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT in wks-mode and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (wks-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Indentation Tests

(ert-deftest wks-mode-test-indentation-basic ()
  "Test basic indentation for nested blocks."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\na \"Chord\" %{{cmd}}\n}"
   (forward-line 2)
   (wks-mode-indent-line)
   (should (= (current-indentation) 4))))

(ert-deftest wks-mode-test-indentation-closing-bracket ()
  "Test that closing brackets align correctly."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\na \"Chord\" %{{cmd}}\n}"
   (goto-char (point-max))
   (wks-mode-indent-line)
   (should (= (current-indentation) 0))))

(ert-deftest wks-mode-test-indentation-comment ()
  "Test comment indentation follows previous line."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\n    a \"Chord\" %{{cmd}}\n# comment\n}"
   (search-forward "# comment")
   (beginning-of-line)
   (wks-mode-indent-line)
   (should (= (current-indentation) 4))))

(ert-deftest wks-mode-test-indentation-nested ()
  "Test nested block indentation."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\n    q \"Nested\"\n    {\n        r \"Deep\" %{{cmd}}\n    }\n}"
   (search-forward "r \"Deep\"")
   (beginning-of-line)
   (wks-mode-indent-line)
   (should (= (current-indentation) 8))))

;;; Syntax Highlighting Tests

(ert-deftest wks-mode-test-font-lock-hook ()
  "Test that hooks are highlighted correctly."
  (wks-test-with-temp-buffer
   "a \"Test\" ^before +keep %{{cmd}}"
   (font-lock-ensure)
   (search-forward "before")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-keyword-face))))

(ert-deftest wks-mode-test-font-lock-flag ()
  "Test that flags are highlighted correctly."
  (wks-test-with-temp-buffer
   "a \"Test\" +keep %{{cmd}}"
   (font-lock-ensure)
   (search-forward "keep")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-keyword-face))))

(ert-deftest wks-mode-test-font-lock-builtin-interpolation ()
  "Test that builtin interpolations are highlighted as constants."
  (wks-test-with-temp-buffer
   "a \"Test %(key)\" %{{echo %(desc)}}"
   (font-lock-ensure)
   (search-forward "%(key)")
   (let ((face (get-text-property (match-beginning 0) 'face)))
     (should (or (eq face font-lock-constant-face)
                 (and (listp face) (memq font-lock-constant-face face)))))))

(ert-deftest wks-mode-test-font-lock-user-var-interpolation ()
  "Test that user variable interpolations are highlighted as variables."
  (wks-test-with-temp-buffer
   ":var \"myvar\" \"value\"\na \"Test %(myvar)\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "%(myvar)")
   (let ((face (get-text-property (match-beginning 0) 'face)))
     (should (or (eq face font-lock-variable-name-face)
                 (and (listp face) (memq font-lock-variable-name-face face)))))))

(ert-deftest wks-mode-test-font-lock-var-macro ()
  "Test that :var macro is highlighted correctly."
  (wks-test-with-temp-buffer
   ":var \"myvar\" \"value\""
   (font-lock-ensure)
   (search-forward "var")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-preprocessor-face))))

(ert-deftest wks-mode-test-font-lock-string ()
  "Test that strings are highlighted."
  (wks-test-with-temp-buffer
   "a \"Description\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "\"Description\"")
   (should (eq (get-text-property (1+ (match-beginning 0)) 'face)
               font-lock-string-face))))

(ert-deftest wks-mode-test-font-lock-command ()
  "Test that command delimiters are highlighted."
  (wks-test-with-temp-buffer
   "a \"Test\" %{{echo hello}}"
   (font-lock-ensure)
   (search-forward "%{{")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-builtin-face))))

(ert-deftest wks-mode-test-font-lock-chord-array ()
  "Test that chord arrays are highlighted."
  (wks-test-with-temp-buffer
   "[abc] \"Test %(index)\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "[abc]")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-keyword-face))))

(ert-deftest wks-mode-test-font-lock-special-key ()
  "Test that special keys are highlighted."
  (wks-test-with-temp-buffer
   "TAB \"Tab key\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "TAB")
   (should (eq (get-text-property (match-beginning 0) 'face)
               font-lock-constant-face))))

;;; Interactive Command Tests

(ert-deftest wks-mode-test-insert-chord ()
  "Test that wks-insert-chord inserts correct template."
  (wks-test-with-temp-buffer
   ""
   (wks-insert-chord)
   (should (string= (buffer-string) "KEY \"Description\" %{{command}}"))))

(ert-deftest wks-mode-test-insert-prefix ()
  "Test that wks-insert-prefix inserts correct template."
  (wks-test-with-temp-buffer
   ""
   (wks-insert-prefix)
   (should (string-match-p "KEY \"Prefix\"" (buffer-string)))
   (should (string-match-p "{" (buffer-string)))
   (should (string-match-p "}" (buffer-string)))))

;;; Completion Tests

(ert-deftest wks-mode-test-completion-after-colon ()
  "Test completion after : suggests macros."
  (wks-test-with-temp-buffer
   ":v"
   (goto-char (point-max))
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "var" candidates))
     (should (member "include" candidates)))))

(ert-deftest wks-mode-test-completion-after-plus ()
  "Test completion after + suggests flags."
  (wks-test-with-temp-buffer
   "+keep"
   (goto-char (1+ (point-min)))  ; Position after the +
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "keep" candidates))
     (should (member "close" candidates)))))

(ert-deftest wks-mode-test-completion-after-caret ()
  "Test completion after ^ suggests hooks."
  (wks-test-with-temp-buffer
   "^b"
   (goto-char (point-max))
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "before" candidates))
     (should (member "after" candidates)))))

(ert-deftest wks-mode-test-completion-interpolation ()
  "Test completion after %( suggests interpolations."
  (wks-test-with-temp-buffer
   "%(k"
   (goto-char (point-max))
   (let* ((result (wks-completion-at-point))
          (candidates (nth 2 result)))
     (should (member "key" candidates))
     (should (member "index" candidates)))))

;;; Imenu Tests

(ert-deftest wks-mode-test-imenu-chord ()
  "Test that Imenu finds chords."
  (wks-test-with-temp-buffer
   "a \"Chord\" %{{cmd}}\nb \"Another\" %{{cmd}}"
   (let ((index (imenu--make-index-alist t)))
     (should (assoc "Chords" index)))))

(ert-deftest wks-mode-test-imenu-prefix ()
  "Test that Imenu finds prefixes."
  (wks-test-with-temp-buffer
   "p \"Prefix\"\n{\n}"
   (let ((index (imenu--make-index-alist t)))
     (should (assoc "Prefixes" index)))))

(ert-deftest wks-mode-test-imenu-variable ()
  "Test that Imenu finds variables."
  (wks-test-with-temp-buffer
   ":var \"myvar\" \"value\""
   (let ((index (imenu--make-index-alist t)))
     (should (assoc "Variables" index)))))

;;; Syntax Table Tests

(ert-deftest wks-mode-test-comment-syntax ()
  "Test that # starts a comment."
  (wks-test-with-temp-buffer
   "# This is a comment"
   (goto-char (point-min))
   (should (eq (syntax-class (syntax-after (point))) 11)))) ; 11 is comment-start

(ert-deftest wks-mode-test-string-syntax ()
  "Test that quotes delimit strings."
  (wks-test-with-temp-buffer
   "\"test string\""
   (goto-char (1+ (point-min)))
   (should (nth 3 (syntax-ppss))))) ; In a string

(ert-deftest wks-mode-test-comment-highlighting ()
  "Test that syntax elements in comments have comment face dominant."
  (wks-test-with-temp-buffer
   "# Comment with :var %(key) +flag ^hook %{{cmd}}\n"
   (font-lock-ensure)
   ;; Check that comment face is present (and typically first in list)
   (goto-char 3) ; Inside comment after "# "
   (let ((face (get-text-property (point) 'face)))
     (should (or (eq face 'font-lock-comment-face)
                 (and (listp face) (memq 'font-lock-comment-face face)))))
   ;; Check at :var position
   (search-forward ":var")
   (let ((face (get-text-property (1- (point)) 'face)))
     (should (or (eq face 'font-lock-comment-face)
                 (and (listp face) (memq 'font-lock-comment-face face)))))
   ;; Check at interpolation
   (search-forward "%(key)")
   (let ((face (get-text-property (match-beginning 0) 'face)))
     (should (or (eq face 'font-lock-comment-face)
                 (and (listp face)
                      (memq 'font-lock-comment-face face)
                      ;; Comment face should be first (dominant)
                      (eq (car face) 'font-lock-comment-face)))))))

;;; Customization Tests

(ert-deftest wks-mode-test-indent-offset-customization ()
  "Test that indent offset can be customized."
  (let ((wks-indent-offset 2))
    (wks-test-with-temp-buffer
     "p \"Prefix\"\n{\na \"Chord\" %{{cmd}}\n}"
     (forward-line 2)
     (wks-mode-indent-line)
     (should (= (current-indentation) 2)))))

;;; Edge Case Tests

(ert-deftest wks-mode-test-empty-buffer ()
  "Test that mode works with empty buffer."
  (wks-test-with-temp-buffer
   ""
   (should (eq major-mode 'wks-mode))))

(ert-deftest wks-mode-test-multiline-string ()
  "Test handling of strings (they should not be multiline in wks)."
  (wks-test-with-temp-buffer
   "a \"Description\n\" %{{cmd}}"
   (should (eq major-mode 'wks-mode))))

(ert-deftest wks-mode-test-escaped-quote ()
  "Test that escaped quotes in strings work."
  (wks-test-with-temp-buffer
   "a \"Test \\\" quote\" %{{cmd}}"
   (font-lock-ensure)
   (search-forward "quote")
   (should (nth 3 (syntax-ppss))))) ; Still in string after escaped quote

;;; Regression Tests

(ert-deftest wks-mode-test-no-catastrophic-backtracking ()
  "Test that arbitrary delimiter pattern doesn't hang on malformed input."
  (wks-test-with-temp-buffer
   "%||this is a very long line without closing delimiter and should not hang emacs or cause catastrophic backtracking"
   (font-lock-ensure)
   ;; If this test completes, backtracking is bounded
   (should t)))

(ert-deftest wks-mode-test-user-var-not-cross-newline ()
  "Test that user variable interpolation doesn't cross newlines."
  (wks-test-with-temp-buffer
   "a \"Test %(unclosed\" %{{cmd}}\nb \"Next line\" %{{cmd}}"
   (font-lock-ensure)
   ;; The unclosed interpolation should not highlight the entire rest of file
   (goto-char (point-max))
   (backward-char 5)
   (should-not (eq (get-text-property (point) 'face)
                   font-lock-variable-name-face))))

;;; Integration Tests

(ert-deftest wks-mode-test-full-file ()
  "Test a complete wks file example."
  (wks-test-with-temp-buffer
   ":var \"greeting\" \"hello\"

# Comment
p \"Prefix\" ^before
{
    a \"Chord %(greeting)\" +keep %{{echo %(desc)}}
    [xyz] \"Array %(index+1)\" %{{cmd}}
}

TAB \"Tab key\" %{{special}}"
   (font-lock-ensure)
   ;; Just ensure no errors occur
   (should (eq major-mode 'wks-mode))))

(provide 'wks-mode-test)

;;; wks-mode-test.el ends here
