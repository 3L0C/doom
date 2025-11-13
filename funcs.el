;;; funcs.el --- Custom utility functions -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;
;; This file contains custom utility functions for Doom Emacs.
;; Functions are organized by category and use the +my/ prefix.
;;
;;; Code:

;;;; Forward Declarations
;; Functions from optional packages loaded at runtime

(declare-function org-insert-link "org" (arg &optional link description))
(declare-function org-id-uuid "org-id" ())
(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl))
(declare-function vterm-other-window "vterm" ())
(declare-function vterm-send-string "vterm" (string &optional paste))
(declare-function dap--debug-session-name "dap-mode" (debug-session))

;;;; External Variables
;; These variables are expected to be defined in config.el/config.org

(defvar cb/pdf-viewer nil
  "External program used to view PDF files.
Should be defined in user configuration.")

(defvar cb/pdf-viewer-page-argument nil
  "Command-line argument format for specifying page number.
Should be defined in user configuration.")

(defvar cb/pdf-viewer-extra-arguments nil
  "Additional command-line arguments for PDF viewer.
Should be defined in user configuration.")

(defvar cb/pdf-directories nil
  "List of directories to search for PDF files.
Should be defined in user configuration.")

(defvar cb/pdf-files-cache nil
  "Cache of PDF file paths.
Populated by `+my/update-pdf-files-cache'.")

;;;; Utility Functions

(defun +my/buffer-file-name-match-p (file-pattern)
  "Return t if current buffer's file name matches FILE-PATTERN."
  (when (stringp buffer-file-name)
    (string-match-p file-pattern buffer-file-name)))

(defun +my/execute-if-confirmed (func &optional arg prompt)
  "Execute FUNC with optional ARG if user confirms via PROMPT.
If ARG is a list, apply FUNC to it; otherwise call FUNC with ARG."
  (when (y-or-n-p (or prompt "Proceed? "))
    (cond
     ((null arg) (funcall func))
     ((listp arg) (apply func arg))
     (t (funcall func arg)))))

;;;; Buffer Utilities

(defun +my/choose-buffer (&optional pattern)
  "Interactively choose a buffer matching PATTERN.
If PATTERN is nil, choose from all buffers.
Returns buffer name, or nil if cancelled or no buffers found.
Auto-selects if only one buffer matches."
  (interactive)
  (let ((buffer-names (mapcar #'buffer-name (buffer-list))))
    (if pattern
        (let ((matching-buffers
               (seq-filter (lambda (name) (string-match-p pattern name))
                           buffer-names)))
          (pcase (length matching-buffers)
            (0 (message "No buffers matching \"%s\" found." pattern)
               nil)
            (1 (car matching-buffers))
            (_ (completing-read "Choose buffer: " matching-buffers nil t))))
      (pcase (length buffer-names)
        (0 (message "No buffers open.")
           nil)
        (1 (car buffer-names))
        (_ (completing-read "Choose buffer: " buffer-names nil t))))))

(defun +my/kill-selected-buffer (&optional pattern)
  "Kill a buffer selected by the user.
If PATTERN is provided, filter buffer list to only show matching buffers."
  (interactive)
  (when-let ((buffer-to-kill (+my/choose-buffer pattern)))
    (delete-windows-on buffer-to-kill)
    (kill-buffer buffer-to-kill)
    (message "Killed buffer %s" buffer-to-kill)))

;;;; Org-Mode Utilities

(defun +my/org-link-at-point (link-format)
  "Create an Org mode link at point using LINK-FORMAT.
LINK-FORMAT should be a function taking (ref description) and returning
a formatted link string. If no word at point, do nothing."
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (word (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (let ((ref (read-string (format "Enter reference for \"%s\": " word)
                            nil nil word)))
      (when (not (string-empty-p ref))
        (delete-region (car bounds) (cdr bounds))
        (insert (funcall link-format ref word))))))

(defun +my/org-add-id-link ()
  "Convert word at point to an Org ID link [[#id][description]]."
  (interactive)
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (word (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "[[#%s][%s]]" word word))))

(defun +my/org-add-id-link-desc ()
  "Convert word at point to an Org ID link with custom reference."
  (interactive)
  (+my/org-link-at-point
   (lambda (ref desc) (format "[[#%s][%s]]" ref desc))))

(defun +my/org-add-header-link ()
  "Convert word at point to an Org header link [[header][description]]."
  (interactive)
  (when-let* ((bounds (bounds-of-thing-at-point 'word))
              (word (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (delete-region (car bounds) (cdr bounds))
    (insert (format "[[%s][%s]]" word word))))

(defun +my/org-add-header-link-desc ()
  "Convert word at point to an Org header link with custom reference."
  (interactive)
  (+my/org-link-at-point
   (lambda (ref desc) (format "[[%s][%s]]" ref desc))))

(defun +my/org-insert-link-from-clipboard ()
  "Replace word at point with an Org link using URL from clipboard.
If no word at point, prompt for description."
  (interactive)
  (unless (fboundp 'org-insert-link)
    (user-error "Org mode not loaded"))
  (when-let ((url (gui-backend-get-selection 'CLIPBOARD 'TEXT)))
    (let* ((bounds (bounds-of-thing-at-point 'word))
           (word (if bounds
                     (buffer-substring-no-properties (car bounds) (cdr bounds))
                   (read-string "Enter link description: "))))
      (when bounds
        (delete-region (car bounds) (cdr bounds)))
      (org-insert-link nil url word))))

(defun +my/add-header-to-org-files (&optional directory)
  "Add a header with ID and title to Org files in DIRECTORY.
Prompts for file tags to add. Skips files that already have a TITLE.
If DIRECTORY is not provided, prompts for directory."
  (interactive)
  (require 'org-id)
  (let* ((directory (or directory (read-directory-name "Select directory: ")))
         (files (directory-files directory t "\\.org$"))
         (filetags '()))
    ;; Collect tags
    (let ((tag "[empty]"))
      (while (not (string-empty-p tag))
        (setq tag (read-string "Enter a file tag (leave blank to finish): "))
        (unless (string-empty-p tag)
          (push (concat "\"" tag "\"") filetags))))
    ;; Process files
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (unless (search-forward-regexp "^#\\+TITLE:" nil t)
          (goto-char (point-min))
          (let* ((filename (file-name-sans-extension
                            (file-name-nondirectory file)))
                 (title (if (string-match "^[0-9]+-\\(.*\\)" filename)
                            (match-string 1 filename)
                          filename)))
            (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n#+TITLE: %s\n"
                            (string-trim (org-id-uuid))
                            title))
            (when filetags
              (insert (format "#+filetags: %s\n"
                              (mapconcat 'identity (reverse filetags) " "))))
            (insert "\n")
            (write-region (point-min) (point-max) file nil 'quiet)))))))

(defun +my/rename-org-file-based-on-header (directory)
  "Rename Org files in DIRECTORY based on first header in file.
Expects header format: * N.M --- Title
Creates filename: YYYYMMDD-NN.MM___title.org"
  (interactive (list (read-directory-name "Directory: " default-directory)))
  (let ((files (directory-files directory nil "\\.org$")))
    (dolist (file files)
      (let* ((filepath (expand-file-name file directory))
             (buffer (find-file-noselect filepath))
             (filename (file-name-sans-extension
                        (file-name-nondirectory filepath)))
             (filedate (if (string-match "^\\([0-9]+\\)-" filename)
                           (match-string 1 filename)
                         filename)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (when (re-search-forward
                 "^\\* \\([0-9]+\\|[a-zA-Z]\\).\\([0-9]+\\|[a-zA-Z]\\) -+ \\(.*\\)$"
                 nil t)
            (let* ((value1 (if (string-match-p "^[0-9]+$" (match-string 1))
                               (format "%02d"
                                       (string-to-number (match-string 1)))
                             (match-string 1)))
                   (value2 (if (string-match-p "^[0-9]+$" (match-string 2))
                               (format "%02d"
                                       (string-to-number (match-string 2)))
                             (match-string 2)))
                   (value3 (replace-regexp-in-string
                            "[ /]" "-"
                            (downcase (match-string 3))))
                   (new-filename
                    (format "%s/%d-%s.%s___%s.org"
                            directory
                            (string-to-number filedate)
                            value1
                            value2
                            value3)))
              (message "Renaming: %s -> %s" filepath new-filename)
              (rename-file (expand-file-name filepath)
                           (expand-file-name new-filename) t))))
        (kill-buffer buffer)))))

;; PDF link handling
(defun +my/org-pdf-link-follow (link)
  "Open a PDF LINK with optional page number.
LINK format: pdf:/path/to/file.pdf#page=105
Uses `cb/pdf-viewer' variable for viewer command."
  (require 'org)
  (unless (boundp 'cb/pdf-viewer)
    (user-error "Variable cb/pdf-viewer not configured"))
  (let* ((path+page (split-string link "#page="))
         (pdf-file (car path+page))
         (page (cadr path+page))
         (opener (mapconcat
                  #'identity
                  (append
                   (list "nohup" cb/pdf-viewer)
                   (when page (list cb/pdf-viewer-page-argument page))
                   cb/pdf-viewer-extra-arguments
                   (list (shell-quote-argument (file-truename pdf-file))
                         ">/dev/null"
                         "2>&1"))
                  " ")))
    (start-process "org-view-pdf" nil "/usr/bin/env" "bash" "-c" opener)))

(defun +my/update-pdf-files-cache ()
  "Update cache of PDF files from cb/pdf-directories."
  (interactive)
  (unless (boundp 'cb/pdf-directories)
    (user-error "Variable cb/pdf-directories not configured"))
  (setq cb/pdf-files-cache
        (apply #'append
               (mapcar (lambda (dir)
                         (when (file-directory-p dir)
                           (directory-files-recursively dir "\\.pdf$")))
                       cb/pdf-directories))))

(defun +my/insert-org-pdf-link (&optional prompt-for-description)
  "Insert a PDF link into current Org document.
With prefix arg PROMPT-FOR-DESCRIPTION, prompt for link description."
  (interactive "P")
  (+my/update-pdf-files-cache)
  (let* ((pdf-file (completing-read "Select PDF file: " cb/pdf-files-cache))
         (page (read-string "Page number (optional): "))
         (description (when prompt-for-description
                        (read-string "Description (optional): ")))
         (link (concat "pdf:" (abbreviate-file-name pdf-file)
                       (when (and page (not (string-empty-p page)))
                         (concat "#page=" page)))))
    (insert (if (and description (not (string-empty-p description)))
                (format "[[%s][%s]]" link description)
              (format "[[%s]]" link)))))

;;;; VTerm Utilities

(defun +my/vterm-send-shift-enter ()
  "Send Shift+Enter key sequence to vterm."
  (interactive)
  (unless (fboundp 'vterm-send-key)
    (user-error "VTerm not loaded"))
  (vterm-send-key (kbd "RET") nil t nil))

(defun +my/run-command-in-vterm (base-command)
  "Open vterm and run BASE-COMMAND with optional arguments.
Prompts for arguments interactively. Runs command in current file's
directory or $HOME if no file."
  (interactive (list (read-string "Command: ")))
  (unless (fboundp 'vterm-other-window)
    (user-error "VTerm not loaded"))
  (let ((default-directory (or (when buffer-file-name
                                 (file-name-directory buffer-file-name))
                               (getenv "HOME"))))
    (vterm-other-window)
    (vterm-send-string (concat base-command " "
                               (read-string "Args: ") "\n"))))

;;;; Shell Command Utilities

(defun +my/run-shell-command-split-window (&optional command)
  "Run shell COMMAND in a horizontal split window.
If COMMAND is not provided, prompt for it. Offers to close window
after command completes."
  (interactive)
  (let* ((command (or command (read-shell-command "Shell command: ")))
         (split-buffer (split-window-below))
         (output-buffer (get-buffer-create "*Shell Command Output*")))
    (set-window-buffer split-buffer output-buffer)
    (async-shell-command command output-buffer)
    (with-selected-window split-buffer
      (run-with-timer 0.1 nil
                      (lambda (window)
                        (when (and (window-live-p window)
                                   (y-or-n-p "Delete output window? "))
                          (delete-window window)))
                      split-buffer))))

(defun +my/run-if-file-name-matches (file-pattern command)
  "Run COMMAND after save if buffer file name matches FILE-PATTERN."
  (when (+my/buffer-file-name-match-p file-pattern)
    (add-hook 'after-save-hook
              (lambda () (+my/run-shell-command-split-window command))
              nil t)))

(defun +my/run-after-saving (mode-hook file-pattern command)
  "In MODE-HOOK, run COMMAND after saving files matching FILE-PATTERN."
  (add-hook mode-hook
            (lambda ()
              (+my/run-if-file-name-matches file-pattern command))))

(defun +my/run-after-saving-unix-mode (file-pattern command)
  "Run COMMAND after saving `conf-unix-mode' files matching FILE-PATTERN."
  (+my/run-after-saving 'conf-unix-mode-hook file-pattern command))

(defun +my/prompt-if-file-name-matches (file-pattern command)
  "Prompt user to run COMMAND after saving files matching FILE-PATTERN."
  (when (+my/buffer-file-name-match-p file-pattern)
    (add-hook 'after-save-hook
              (lambda ()
                (when (y-or-n-p (format "Run %s? " command))
                  (+my/run-shell-command-split-window command)))
              nil t)))

(defun +my/maybe-run-after-saving (mode-hook file-pattern command)
  "In MODE-HOOK, prompt to run COMMAND after saving files matching FILE-PATTERN."
  (add-hook mode-hook
            (lambda ()
              (+my/prompt-if-file-name-matches file-pattern command))))

;;;; Debug Utilities

(defun +my/debug-cleanup-output (arg)
  "Kill DAP mode output buffer for debug session ARG if confirmed.
Calls hydra-keyboard-quit before cleanup."
  (interactive)
  (unless (fboundp 'dap--debug-session-name)
    (user-error "DAP mode not loaded"))
  (when (fboundp 'hydra-keyboard-quit)
    (hydra-keyboard-quit))
  (let ((target-session (concat "\\*" (dap--debug-session-name arg)
                                "[^*]+-\\scppdbg:.*\\*")))
    (+my/execute-if-confirmed #'+my/kill-selected-buffer
                              target-session
                              "Kill debug output buffer? ")
    (message "Debug session: %s" (dap--debug-session-name arg))))

;;;; Project-Specific Utilities

;; CCSS project functions
(defun +my/ccss-camelcase-to-snake-case (s)
  "Convert camelCase string S to snake_case."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2"
      s))))

(defun +my/ccss-rename-camelcase-functions ()
  "Find and rename camelCase functions to snake_case in CCSS project.
Uses LSP rename for project-wide refactoring. Prompts for each function."
  (interactive)
  (let ((continue t)
        (count 0)
        (search-pattern "\\b\\(ccss[A-Z][a-zA-Z0-9]*\\)\\s-*("))
    (while (and continue (re-search-forward search-pattern nil t))
      (let* ((match-pos (match-beginning 1))
             (old-name (match-string 1))
             (new-name (+my/ccss-camelcase-to-snake-case old-name))
             (response (read-string
                        (format "Rename '%s' to '%s'? (y/n/q, default: y): "
                                old-name new-name)
                        nil nil "y")))
        (cond
         ((string= response "q")
          (setq continue nil)
          (message "Renaming stopped. %d functions renamed." count))
         ((string= response "y")
          (when (and match-pos (fboundp 'lsp-rename))
            (goto-char match-pos)
            (lsp-rename new-name)
            (setq count (1+ count))
            (message "Renamed %s to %s (%d so far)" old-name new-name count)))
         (t (message "Skipped %s" old-name)))
        ;; Give LSP time to process
        (sit-for 0.05)))
    (message "Finished renaming. %d functions renamed in total." count)))

(provide 'funcs)
;;; funcs.el ends here
