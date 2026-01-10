;;; efm-mode.el --- Emacs File Manager - Use Emacs as a standalone file manager -*- lexical-binding: t; -*-

;; Copyright (C) 2025 3L0C
;; Author: 3L0C <dotbox at mailbox dot org>
;; Version: 0.1.0
;; Package-Version: 20250109.1
;; Created: January 9, 2025
;; Keywords: files, convenience, dired, dirvish
;; Homepage: https://github.com/3L0C/efm-mode
;; URL: https://github.com/3L0C/efm-mode
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; License:

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
;; EFM (Emacs File Manager) mode transforms Emacs into a standalone file
;; manager with intuitive frame management.  It works with both dirvish
;; (preferred) and vanilla dired.
;;
;; Features:
;; - Quit file browser to exit frame (or Emacs if sole frame)
;; - Return to file browser after editing files
;; - Open files in separate frames
;; - Graceful fallback to dired if dirvish is unavailable
;;
;; Installation:
;;
;; Add efm-mode.el to your load-path and add:
;;   (require 'efm-mode)
;;
;; Or with use-package:
;;   (use-package efm-mode
;;     :commands (efm efm-mode))
;;
;; Usage:
;;
;;   M-x efm                              - Enable efm-mode and open file browser
;;   emacs --eval "(efm)"                 - Launch as file manager
;;   emacs --eval "(efm \"~/Documents\")" - Open specific directory
;;
;; Note: If your package manager generates autoloads (Doom, use-package with
;; :commands, etc.), the explicit require is not needed.
;;
;; This package does NOT bind any keys by default.  Configure keybindings
;; in your init file.
;;
;; Example keybindings with Evil:
;;
;;   (with-eval-after-load 'efm-mode
;;     (evil-define-key 'normal 'global
;;       (kbd "Z Z") #'efm-save-and-return
;;       (kbd "Z Q") #'efm-return)
;;     (evil-define-key 'normal dired-mode-map
;;       (kbd "O") #'find-file-other-frame))
;;
;; Example keybindings without Evil:
;;
;;   (with-eval-after-load 'efm-mode
;;     (global-set-key (kbd "C-c e z") #'efm-save-and-return)
;;     (global-set-key (kbd "C-c e q") #'efm-return)
;;     (define-key dired-mode-map (kbd "C-c o") #'find-file-other-frame))
;;
;; Available interactive functions:
;;
;;   `efm'                         - Entry point: enable mode and open browser
;;   `efm-save-and-return'         - Save buffer, kill it, return to browser
;;   `efm-return'                  - Kill buffer, return to browser
;;   `efm-toggle'                  - Toggle efm-mode on/off
;;   `efm-dired-find-file-other-frame' - Open file in other frame (for RET override)
;;
;; Customization:
;;
;;   M-x customize-group RET efm RET
;;

;;; Code:

(require 'dired)

;;; Customization

(defgroup efm nil
  "Emacs File Manager - Use Emacs as a standalone file manager."
  :group 'files
  :group 'convenience
  :prefix "efm-")

(defcustom efm-prefer-dirvish t
  "Whether to prefer dirvish over dired when available.
If non-nil and dirvish is available, use dirvish.
If nil or dirvish is unavailable, use dired."
  :type 'boolean
  :group 'efm)

(defcustom efm-quit-action 'smart
  "Action to take when quitting the file browser in EFM mode.
  `smart'        - Delete frame if multiple frames exist, otherwise kill Emacs
  `delete-frame' - Always attempt to delete frame
  `kill-emacs'   - Always kill Emacs
  `bury-buffer'  - Just bury the buffer (don't exit)"
  :type '(choice (const :tag "Smart (delete frame or kill Emacs)" smart)
                 (const :tag "Always delete frame" delete-frame)
                 (const :tag "Always kill Emacs" kill-emacs)
                 (const :tag "Just bury buffer" bury-buffer))
  :group 'efm)

(defcustom efm-return-opens-browser t
  "Whether return commands should open the file browser.
If non-nil, `efm-save-and-return' and `efm-return' will open the
file browser at the current file's directory.
If nil, they just save/kill the buffer without opening the browser."
  :type 'boolean
  :group 'efm)

(defcustom efm-open-files-in-other-frame t
  "Whether to open files in other frames when in dired.
When non-nil and `efm-mode' is active, opening files from dired
will use a new frame.  Directories are always opened in the same frame."
  :type 'boolean
  :group 'efm)

(defcustom efm-mode-lighter " EFM"
  "Mode line lighter for EFM mode."
  :type 'string
  :group 'efm)

;;; Internal Variables

(defvar efm--browser-function nil
  "Function to use for opening the file browser.
Set automatically based on available packages.")

(defvar efm--quit-function nil
  "Function to advise for quit behavior.
Set automatically based on available packages.")

(defvar efm--original-dired-find-file nil
  "Original function bound to RET in dired before EFM modified it.")

(defvar efm--advice-installed nil
  "Non-nil if quit advice has been installed.")

(defvar efm--advised-function nil
  "The function that currently has advice installed.
Tracked separately to ensure proper cleanup if configuration changes.")

;;; Browser Selection

(defun efm--select-browser-function ()
  "Return the appropriate function to open the file browser."
  (cond
   ((and efm-prefer-dirvish
         (require 'dirvish nil t)
         (fboundp 'dirvish))
    #'dirvish)
   (t
    (when efm-prefer-dirvish
      (message "EFM: Dirvish not available, using dired"))
    #'dired)))

(defun efm--select-quit-function ()
  "Return the appropriate function to advise for quit behavior."
  (cond
   ((and (eq efm--browser-function #'dirvish)
         (fboundp 'dirvish-quit))
    #'dirvish-quit)
   (t #'quit-window)))

;;; File Browser Functions

(defun efm-open-browser (&optional path)
  "Open file browser at PATH using the configured browser function.
If PATH is nil, use `default-directory'."
  (let ((dir (or path default-directory)))
    (funcall efm--browser-function dir)))

;;;###autoload
(defun efm-dired-find-file-other-frame ()
  "Open file at point in another frame, or directory in current frame.
Intended for use as dired RET binding when `efm-mode' is active."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        ;; Directories open in same frame
        (dired-find-file)
      ;; Files open in other frame
      (find-file-other-frame file))))

;;; Quit Handler

(defun efm--quit-handler (&rest _)
  "Handle quitting the file browser in EFM mode.
Behavior depends on `efm-quit-action' setting."
  (when efm-mode
    (pcase efm-quit-action
      ('smart
       (if (> (length (frame-list)) 1)
           (delete-frame)
         (save-buffers-kill-emacs)))
      ('delete-frame
       (when (> (length (frame-list)) 1)
         (delete-frame)))
      ('kill-emacs
       (save-buffers-kill-emacs))
      ('bury-buffer
       nil)))) ; quit-window already buries

;;; Mode Enable/Disable

(defun efm--enable ()
  "Enable EFM mode: setup browser, advice, and dired modifications."
  ;; Select browser and quit functions
  (setq efm--browser-function (efm--select-browser-function))
  (setq efm--quit-function (efm--select-quit-function))

  ;; Install quit advice
  (unless efm--advice-installed
    (advice-add efm--quit-function :after #'efm--quit-handler)
    (setq efm--advised-function efm--quit-function)
    (setq efm--advice-installed t))

  ;; Override dired RET if configured (only save original once)
  (when (and efm-open-files-in-other-frame
             (not efm--original-dired-find-file))
    (require 'dired)
    (setq efm--original-dired-find-file
          (lookup-key dired-mode-map (kbd "RET")))
    (define-key dired-mode-map (kbd "RET") #'efm-dired-find-file-other-frame)))

(defun efm--disable ()
  "Disable EFM mode: remove advice and restore dired bindings."
  ;; Remove quit advice from the function we actually advised
  (when (and efm--advice-installed efm--advised-function)
    (advice-remove efm--advised-function #'efm--quit-handler)
    (setq efm--advice-installed nil)
    (setq efm--advised-function nil))

  ;; Restore original dired RET
  (when efm--original-dired-find-file
    (define-key dired-mode-map (kbd "RET") efm--original-dired-find-file)
    (setq efm--original-dired-find-file nil)))

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode efm-mode
  "Toggle Emacs File Manager mode.

When enabled, EFM mode modifies Emacs behavior to act like a
standalone file manager:

- Quitting the file browser exits the frame (or Emacs)
- Return commands bring you back to the file browser
- Files can be opened in separate frames from dired

Key bindings are NOT set by default.  See the Commentary section
of efm-mode.el for example configurations."
  :global t
  :lighter efm-mode-lighter
  :group 'efm
  (if efm-mode
      (efm--enable)
    (efm--disable)))

;;; Interactive Commands

;;;###autoload
(defun efm (&optional path)
  "Start Emacs File Manager mode and open file browser at PATH.
If PATH is nil, opens current directory.
With prefix argument, prompt for directory.
This is the main entry point for EFM mode."
  (interactive
   (list (when current-prefix-arg
           (read-directory-name "Directory: "))))
  ;; Enable efm-mode if not already enabled
  (unless efm-mode
    (efm-mode 1))
  ;; Open the file browser
  (efm-open-browser (or path default-directory)))

;;;###autoload
(defun efm-save-and-return ()
  "Save current buffer and return to file browser.
If `efm-mode' is disabled or `efm-return-opens-browser' is nil,
just saves and kills the buffer without opening the browser."
  (interactive)
  (let ((dir (if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory)))
    ;; Save if modified and is a file
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))
    ;; Kill the buffer and return to browser if successful
    (when (kill-buffer (current-buffer))
      (when (and efm-mode efm-return-opens-browser)
        (efm-open-browser dir)))))

;;;###autoload
(defun efm-return ()
  "Kill current buffer (prompting if modified) and return to file browser.
If `efm-mode' is disabled or `efm-return-opens-browser' is nil,
just kills the buffer without opening the browser."
  (interactive)
  (let ((dir (if buffer-file-name
                 (file-name-directory buffer-file-name)
               default-directory)))
    ;; Kill buffer (will prompt if modified) and return to browser if successful
    (when (kill-buffer (current-buffer))
      (when (and efm-mode efm-return-opens-browser)
        (efm-open-browser dir)))))

;;;###autoload
(defun efm-toggle ()
  "Toggle EFM mode on or off."
  (interactive)
  (efm-mode (if efm-mode -1 1))
  (message "EFM mode %s" (if efm-mode "enabled" "disabled")))

(provide 'efm-mode)

;;; efm-mode.el ends here
