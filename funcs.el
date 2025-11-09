;;; funcs.el -*- lexical-binding: t; -*-
(load! "util-funcs.el")

(defun choose-buffer (&optional pattern)
  "Interactively choose an open buffer that matches the given PATTERN
   (or any buffer if PATTERN is nil) and return its name. If there is
   only one buffer in the list, return its name automatically."
  (interactive)
  (let ((buffer-names (mapcar #'buffer-name (buffer-list))))
    (if pattern
        (let ((matching-buffers ()))
          (dolist (buffer-name buffer-names)
            (when (string-match-p pattern buffer-name)
              (push buffer-name matching-buffers)))
          (cond
           ((null matching-buffers)
            (message "No buffers matching \"%s\" found." pattern)
            nil)
           ((= (length matching-buffers) 1)
            (car matching-buffers))
           (t
            (completing-read "Choose buffer: " matching-buffers nil t))))
      (cond
       ((null buffer-names)
        (message "No buffers open.")
        nil)
       ((= (length buffer-names) 1)
        (car buffer-names))
       (t
        (completing-read "Choose buffer: " buffer-names nil t))))))

(defun kill-selected-buffer (&optional pattern)
  "Kill the buffer selected by the user.
  If PATTERN is provided, use it to filter the list of
  buffers displayed to the user."
  (interactive)
  (let ((buffer-to-kill (choose-buffer pattern)))
    (when buffer-to-kill
      (delete-windows-on buffer-to-kill)
      (message "Killed buffer %s" buffer-to-kill))))

(defun execute-if-confirmed (func &optional arg prompt)
  "Execute FUNCTION with optional ARG if user confirms the action
   with a \=y\= response to PROMPT."
  (when (y-or-n-p prompt)
    (if arg
        (if (listp arg)
            (apply func arg)
          (funcall func arg))
      (funcall func))))

(defun debug-cleanup-output (arg)
  "Kill output buffer if confirmed"
  (interactive)
  (hydra-keyboard-quit)
  (let ((target-session (concat "\*" (dap--debug-session-name arg) "[^*]+-\scppdbg:.*\*")))
    (execute-if-confirmed #'kill-selected-buffer target-session "Kill output?")
    (message target-session))
  (message (dap--debug-session-name arg)))

(defun ccss-camelcase-to-snake-case (s)
  "Convert a camelCase string S to snake_case."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2"
      s))))

(defun ccss-rename-camelcase-functions ()
  "Find and rename camelCase functions to snake_case in CCSS project."
  (interactive)
  (let ((continue t)
        (count 0)
        (search-pattern "\\b\\(ccss[A-Z][a-zA-Z0-9]*\\)\\s-*("))

    (while (and continue (re-search-forward search-pattern nil t))
      (let* ((match-pos (match-beginning 1))
             (old-name (match-string 1))
             (new-name (ccss-camelcase-to-snake-case old-name))
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

         (t (message "Skipped %s" old-name))))

      ;; Give LSP a moment to process the rename
      (sit-for 0.05))

    (message "Finished renaming. %d functions renamed in total." count)))

(defun run-shell-command-split-window (&optional command)
  "Run a shell command in a horizontal split window.
   The user can interact with the command and the output is
   visible in the split window. Delete the split window after
   the command is finished if the user responds with y.
   If COMMAND is provided, use it as the shell command to run."
  (interactive)
  (unless command
    (setq command (read-shell-command "Shell command: ")))
  (let* ((split-buffer (split-window-below))
         (output-buffer (get-buffer-create "*Shell Command Output*")))
    (set-window-buffer split-buffer output-buffer)
    (async-shell-command command output-buffer)
    (with-selected-window split-buffer
      (setq-local quit-window-timer
                  (run-with-timer 0.1 nil
                                  (lambda ()
                                    (when (y-or-n-p "Delete output window?")
                                      (delete-window split-buffer))))))))

;; Get the current Unix time
(defun my-get-current-unix-time ()
  "Return the current Unix time."
  (car (time-convert (car (current-time)) (cadr (current-time)))))

(defun my-open-vterm ()
  "Open a unique vterm window and resize it to take up 35% of the bottom of the
   screen."
  (interactive)
  (setq vterm-buffer-name (format "*vterm<%s>*" (my-get-current-unix-time)))
  (let* ((height (round (* 0.35 (window-total-height))))
         (my-vterm-buffer (get-buffer-create vterm-buffer-name)))
    (with-current-buffer my-vterm-buffer
      (vterm)
      (fit-window-to-buffer nil nil height)
      (set-process-sentinel vterm--process
                            (lambda (process event)
                              (let ((window (get-buffer-window my-vterm-buffer)))
                                (when window
                                  (delete-window window))))))))

(defun org-add-id-link ()
  "Convert the word under the cursor to a user-defined reference."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (if (not (string= word ""))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (format "[[#%s][%s]]" word word)))
      (message "Using \"%s\" as reference" word))))

(defun org-add-id-link-desc ()
  "Convert the word under the cursor to a user-defined reference."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (user-ref (read-string (format "Enter a reference for \"%s\": " word) nil nil word)))
    (if (not (string= user-ref ""))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (format "[[#%s][%s]]" user-ref word)))
      (message "Using \"%s\" as reference" word))))

(defun org-add-header-link ()
  "Convert the word under the cursor to a user-defined reference."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (if (not (string= word ""))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (format "[[%s][%s]]" word word)))
      (message "Using \"%s\" as reference" word))))

(defun org-add-header-link-desc ()
  "Convert the word under the cursor to a user-defined reference."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (user-ref (read-string (format "Enter a reference for \"%s\": " word) nil nil word)))
    (if (not (string= user-ref ""))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (format "[[%s][%s]]" user-ref word)))
      (message "Using \"%s\" as reference" word))))

(defun org-insert-link-from-clipboard ()
  "Replace the word at point with a link with the URL from the clipboard in an
   Org mode document.  If there is no word at point, prompt the user to enter a
   word."
  (interactive)
  (let ((url (gui-backend-get-selection 'CLIPBOARD 'TEXT)))
    (when url
      (let* ((bounds (bounds-of-thing-at-point 'word))
             (word (if bounds
                       (buffer-substring-no-properties (car bounds) (cdr bounds))
                     (read-string "Enter word: "))))
        (when bounds
          (delete-region (car bounds) (cdr bounds)))
        (org-insert-link nil url word)))))

(defun run-command-in-vterm (base-command)
  "Open a new vterm buffer and run a command with optional arguments.
   If BASE-COMMAND is not specified, prompt the user for a command.
   The command is run in the current directory of the file open in the
   current buffer, or $HOME if no directory is found."
  (interactive (list (read-string "Command: ")))
  (let ((default-directory (or (ignore-errors (file-name-directory buffer-file-name))
                               (getenv "HOME"))))
    (vterm-other-window)
    (vterm-send-string (concat base-command " " (read-string "Args: ") "\n"))))

;; (defun org-insert-link-from-clipboard ()
;;   "Replace the word at point with a link with the URL from the clipboard in an Org mode document."
;;   (interactive)
;;   (let ((url (gui-backend-get-selection 'CLIPBOARD 'TEXT)))
;;     (when url
;;       (let* ((bounds (bounds-of-thing-at-point 'word))
;;              (word (buffer-substring-no-properties (car bounds) (cdr bounds))))
;;         (delete-region (car bounds) (cdr bounds))
;;         (org-insert-link nil url word)))))

;; (defun org-insert-link-from-clipboard ()
;;   "Insert a link with the URL from the clipboard at point in an Org mode document."
;;   (interactive)
;;   (let ((url (gui-backend-get-selection 'CLIPBOARD 'TEXT)))
;;     (when url
;;       (org-insert-link nil url (thing-at-point 'word)))))

;; Does not work
(defun my/org-insert-last-link (description)
  "Insert the last stored link and prompt for a DESCRIPTION."
  (interactive "sDescription: ")
  (let* ((link-info (car org-stored-links))
         (link (org-link-make-string (nth 1 link-info))))
    (insert (format "[[%s][%s]]" link description))))

(defun add-header-to-org-files (&optional directory)
  "Add a header to every Org file in the given DIRECTORY.
   If DIRECTORY is not provided, prompt the user to select a directory."
  (interactive)
  (unless directory
    (setq directory (read-directory-name "Select directory: ")))
  (let ((files (directory-files directory t "\\.org$")))
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
                          (filename))))
            (insert (concat ":PROPERTIES:\n"
                            ":ID:       " (shell-command-to-string "uuidgen")
                            ":END:\n"
                            "#+TITLE: " title "\n"
                            "#+filetags: \"learncpp\" \"cpp\" \"programming\"\n\n"))
            (write-region (point-min) (point-max) file nil 'quiet)))))))

(defun add-header-to-org-files (&optional directory)
  "Add a header to every Org file in the given DIRECTORY.
   If DIRECTORY is not provided, prompt the user to select a directory."
  (interactive)
  (unless directory
    (setq directory (read-directory-name "Select directory: ")))
  (let* ((files (directory-files directory t "\\.org$"))
         (filetags '())
         (tag "[empty]"))
    (while (not (string-equal tag ""))
      (setq tag (read-string "Enter a file tag (leave blank to finish): "))
      (unless (string-equal tag "")
        (push (concat "\"" tag "\"") filetags)))
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
                          (filename))))
            (insert (concat ":PROPERTIES:\n"
                            ":ID:       " (shell-command-to-string "uuidgen")
                            ":END:\n"
                            "#+TITLE: " title "\n"
                            "#+filetags: " (mapconcat 'identity (reverse filetags) " ") "\n\n"))
            (write-region (point-min) (point-max) file nil 'quiet)))))))


(defun rename-org-file-based-on-header (directory)
  "Rename Org files in DIRECTORY based on the first header in the file."
  (interactive (list (read-directory-name "Directory: " default-directory)))
  (let ((files (directory-files directory nil "\\.org$")))
    (dolist (file files)
      (let* ((filepath (concat directory file))
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
              (message "%s\n%s" filepath new-filename)
              (rename-file
               (expand-file-name filepath)
               (expand-file-name new-filename) t))
        (kill-buffer buffer)))))))

(defun run-if-file-name-matches (file-pattern command)
  "Run COMMAND if file-name matches FILE-PATTERN"
  (when (buffer-file-name-match-p file-pattern)
    (add-hook! 'after-save-hook :local
      (run-shell-command-split-window command))))

(defun run-after-saving (mode-hook file-pattern command)
  "Add a hook for MODE-HOOK to run COMMAND afters saving files matching
   FILE-PATTERN."
  (add-hook mode-hook
            (lambda ()
              (run-if-file-name-matches file-pattern command))))

(defun run-after-saving-unix-mode (file-pattern command)
  "Run COMMAND after saving conf-unix-mode files matching FILE-PATTERN"
  (run-after-saving 'conf-unix-mode-hook file-pattern command))

(defun prompt-if-file-name-matches (file-pattern command)
  "Prompt user if COMMAND should be run after saving files matching
   FILE-PATTERN"
  ;; TODO turn (when (stringp ...) (when ((string-match-p ...)))) into a function
  (when (buffer-file-name-match-p file-pattern)
    (add-hook! 'after-save-hook :local
      (when (y-or-n-p (concat "Run " command " ?"))
        (run-shell-command-split-window command)))))

(defun maybe-run-after-saving (mode-hook file-pattern command)
  "Add a hook for MODE-HOOK to prompt user after saving files macthing
   FILE-PATTERN if COMMAND should be run."
  (add-hook mode-hook
            (lambda ()
              (prompt-if-file-name-matches file-pattern command))))

;; reload my theme for gay bullshit
;; (defvar my-theme-reloaded 0)
;; (defun my-reload-theme ()
;;   "Reload my theme if needed"
;;   (if (< my-theme-reloaded 2)
;;       ((lambda ()
;;          (doom/reload-theme)
;;          (setq my-theme-reloaded (1+ my-theme-reloaded))))))

;; (defun rename-org-file-based-on-header (directory)
;;   "Rename Org files in DIRECTORY based on the first header in the file."
;;   (interactive (list (read-directory-name "Directory: " default-directory)))
;;   (let ((files (directory-files directory nil "\\.org$")))
;;     (dolist (file files)
;;       (let* ((filepath (concat directory file))
;;              (buffer (find-file-noselect file))
;;              (filename (file-name-sans-extension
;;                         (file-name-nondirectory filepath)))
;;              (filedate (if (string-match "^\\([0-9]+\\)-" filename)
;;                            (match-string 1 filename)
;;                         (filename))))
;;         (with-current-buffer buffer
;;           (goto-char (point-min))
;;           (when (re-search-forward
;;                  "^\\* \\([0-9]+\\)\\.\\([0-9]+\\) -+ \\(.*\\)$"
;;                  nil t)
;;             (let ((new-filename
;;                    (format "%d-%02d.%02d___%s.org"
;;                            (string-to-number filedate)
;;                            (string-to-number (match-string 1))
;;                            (string-to-number (match-string 2))
;;                            (replace-regexp-in-string
;;                             " " "-"
;;                             (downcase (match-string 3))))))
;;               (message "%s\n%s" filepath new-filename)
;;               (sleep-for 2)
;;               (rename-file
;;                (expand-file-name filepath)
;;                (expand-file-name (concat directory new-filename)) t))))
;;         (kill-buffer buffer)))))

;; (defun add-header-to-org-files (&optional directory)
;;   "Add a header to every Org file in the given DIRECTORY.
;; If DIRECTORY is not provided, prompt the user to select a directory."
;;   (interactive)
;;   (unless directory
;;     (setq directory (read-directory-name "Select directory: ")))
;;   (let ((files (directory-files directory t "\\.org$")))
;;     (dolist (file files)
;;       (with-temp-buffer
;;         (insert-file-contents file)
;;         (goto-char (point-min))
;;         (unless (search-forward-regexp "^#\\+TITLE:" nil t)
;;           (goto-char (point-min))
;;           (insert (concat ":PROPERTIES:\n"
;;                           ":ID:       " (shell-command-to-string "uuidgen")
;;                           ":END:\n"
;;                           "#+TITLE: " (file-name-sans-extension (file-name-nondirectory file)) "\n\n"))
;;           (write-region (point-min) (point-max) file nil 'quiet))))))

;; (defun my-open-vterm ()
;;   "Open a unique vterm window and resize it to take up 35% of the bottom of the screen."
;;   (interactive)
;;   (let ((vterm-buffer-name (format "*vterm<%s>*" (my-get-current-unix-time))))
;;     (switch-to-buffer (get-buffer-create vterm-buffer-name))
;;     (vterm)
;;     (fit-window-to-buffer nil nil (round (* 0.35 (window-total-height))))
;;     (let ((vterm-buffer (current-buffer)))
;;       (set-process-sentinel (get-buffer-process vterm-buffer)
;;                             (lambda (process event)
;;                               (let ((window (get-buffer-window vterm-buffer)))
;;                                 (when window
;;                                   (delete-window window))))))))

;; (defun open-vterm ()
;;   "Open a vterm window that takes up 35% of the screen, opens it below the current window, and kills the window when vterm is exited."
;;   (interactive)
;;   (let* ((vterm-buffer (generate-new-buffer "*vterm*"))
;;          (vterm-window-height (round (* 0.35 (window-height))))
;;          (vterm-window (split-window-below vterm-window-height)))
;;     (with-selected-window vterm-window
;;       (set-window-buffer vterm-window vterm-buffer)
;;       (vterm)))
;;   (set-process-sentinel
;;    (get-buffer-process (current-buffer))
;;    (lambda (process event)
;;      (when (string= event "finished\n")
;;        (kill-buffer-and-window)))))

(defun vterm-send-shift-enter ()
  "Send Shift+Enter key sequence to vterm."
  (interactive)
  (vterm-send-key (kbd "RET") nil t nil))

(defun org-pdf-link-follow-link (link)
  "Where page number is 105, the link should look like:
   [[pdf:/path/to/file.pdf#page=105][File, page 105]]"
  (let* ((path+page (split-string link "#page="))
         (pdf-file (car path+page))
         (page (cadr path+page))
         (opener (mapconcat
                  #'identity
                  (append
                   (list "nohup" cb/pdf-viewer)
                   (if page (list cb/pdf-viewer-page-argument page))
                   cb/pdf-viewer-extra-arguments
                   (list (shell-quote-argument (file-truename pdf-file))
                         ">/dev/null"
                         "2>&1"))
                  " ")))
    (apply 'start-process "org-view-pdf" nil
                   (list "/usr/bin/env" "bash" "-c" opener))))


(defun cb/update-pdf-files-cache ()
  "Update the cache of PDF files from `cb/pdf-directories`."
  (interactive)
  (setq cb/pdf-files-cache
        (apply 'append
               (mapcar (lambda (dir)
                         (when (file-directory-p dir)
                           (directory-files-recursively dir "\\.pdf$")))
                       cb/pdf-directories))))

(defun cb/insert-org-pdf-link (&optional prompt-for-description)
  "Insert a PDF link into the current Org document.

PROMPT-FOR-DESCRIPTION is a flag. If true, prompt for a description
to add to the link."
  (interactive "P")
  (cb/update-pdf-files-cache)
  (let* ((pdf-file (completing-read "Select a PDF file: " cb/pdf-files-cache))
         (page (read-string "Page number (optional): "))
         (description (if prompt-for-description
                          (read-string "Description (optional): ")
                        ""))
         (link (concat "pdf:" (abbreviate-file-name pdf-file)
                       (when (and page (not (string-empty-p page)))
                         (concat "#page=" page)))))
    (insert (if (not (string-empty-p description))
                (format "[[%s][%s]]" link description)
              (format "[[%s]]" link)))))
