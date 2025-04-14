;;; slash-commands.el --- Simple slash commands with Corfu -*- lexical-binding: t -*-

;; Author: Claude
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (corfu "0.36"))

;;; Commentary:
;; A lightweight implementation of slash commands that integrates with Corfu.
;; Type a slash at the beginning of a line or after whitespace to trigger
;; completion for available commands in the current major mode.

;;; Code:

(require 'corfu)

;;;; Customization

(defgroup slash-commands nil
  "Slash commands for Emacs."
  :group 'convenience)

(defcustom slash-commands-alist nil
  "Alist of slash commands for different major modes.
Each element has the form (MAJOR-MODE . COMMANDS) where COMMANDS
is an alist with elements (NAME . FUNCTION)."
  :type '(alist :key-type symbol
                :value-type (alist :key-type string
                                   :value-type function))
  :group 'slash-commands)

;;;; Internal variables

(defvar-local slash-commands--active nil
  "Non-nil when slash commands are active in the current buffer.")

(defvar-local slash-commands--prefix nil
  "Current slash command prefix being completed.")

;;;; Core functions

(defun slash-commands-for-mode (mode commands)
  "Register COMMANDS for MODE.
COMMANDS is an alist where each element has the form (NAME . FUNCTION)."
  (setf (alist-get mode slash-commands-alist) commands))

(defun slash-commands--get-commands ()
  "Get slash commands for the current major mode."
  (alist-get major-mode slash-commands-alist))

(defun slash-commands--exit-function (candidate status)
  "Function called when completing a slash command.
CANDIDATE is the selected command name.
STATUS is the completion status."
  (when (eq status 'finished)
    ;; Get the command function
    (let* ((commands (slash-commands--get-commands))
           (command-fn (alist-get candidate commands nil nil #'string=)))
      ;; Delete the slash and command name
      (delete-region (- (point) (length candidate) 1) (point))
      ;; Execute the command function
      (when command-fn
        (funcall command-fn))))
  ;; Always deactivate slash commands
  (slash-commands--deactivate))

(defun slash-commands--deactivate ()
  "Deactivate slash commands."
  (setq-local slash-commands--active nil)
  (remove-hook 'completion-at-point-functions #'slash-commands--completion-at-point t)
  (remove-hook 'post-command-hook #'slash-commands--post-command-hook t))

(defun slash-commands--candidates ()
  "Return a list of slash command candidates."
  (mapcar #'car (slash-commands--get-commands)))

(defun slash-commands--completion-at-point ()
  "Completion at point function for slash commands."
  (when slash-commands--active
    (let* ((beg (+ slash-commands--prefix 1)) ; After the slash
           (end (point))
           (table (slash-commands--candidates))
           (exit-fn #'slash-commands--exit-function))
      (list beg end table
            :exclusive 'no
            :exit-function exit-fn))))

(defun slash-commands--post-command-hook ()
  "Function called after each command to manage slash command state."
  ;; If slash command is active and user types a space, deactivate it
  (when (and slash-commands--active
             (eq last-command 'self-insert-command)
             (eq (char-before) ?\s))
    (slash-commands--deactivate)
    (corfu-quit)))

(defun slash-commands--post-self-insert-hook ()
  "Function called after self-insertion."
  ;; Check if we just inserted a slash at beginning of line or after whitespace
  (when (and (eq (char-before) ?/)
             (or (= (point) 1)
                 (and (> (point) 1)
                      (memq (char-before (1- (point))) '(?\s ?\t ?\n)))))
    (let ((commands (slash-commands--get-commands)))
      (when commands
        ;; Remember the starting point
        (setq-local slash-commands--prefix (1- (point)))
        (setq-local slash-commands--active t)

        ;; Register our completion function
        (add-hook 'completion-at-point-functions #'slash-commands--completion-at-point nil t)

        ;; Add hook to detect space after slash
        (add-hook 'post-command-hook #'slash-commands--post-command-hook nil t)

        ;; Trigger completion using a safer method
        (corfu-mode 1)
        (completion-at-point)))))

;;;; User commands

(defun slash-commands-execute ()
  "Manually execute a slash command at point."
  (interactive)
  (let ((commands (slash-commands--get-commands)))
    (unless commands
      (user-error "No slash commands defined for %s" major-mode))

    (let* ((command-names (mapcar #'car commands))
           (selected (completing-read "Command: " command-names))
           (command-fn (alist-get selected commands nil nil #'string=)))
      (when command-fn
        (funcall command-fn)))))

;;;; Minor mode

(define-minor-mode slash-commands-mode
  "Minor mode for slash commands."
  :lighter " /"
  (if slash-commands-mode
      (add-hook 'post-self-insert-hook #'slash-commands--post-self-insert-hook nil t)
    (remove-hook 'post-self-insert-hook #'slash-commands--post-self-insert-hook t)
    (remove-hook 'post-command-hook #'slash-commands--post-command-hook t)
    (slash-commands--deactivate)))

(provide 'slash-commands)
;;; slash-commands.el ends here
