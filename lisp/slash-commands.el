;;; slash-commands.el --- User-configurable slash commands as a minor mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your-email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/yourusername/slash-commands

;;; Commentary:

;; A minor mode that provides configurable slash commands for any major mode.
;; Type a slash at the beginning of a line or after whitespace to trigger
;; custom commands specific to the current major mode.

;; Usage:
;;   (require 'slash-commands)
;;   (slash-commands-global-mode 1)
;;
;;   ;; Configure commands for specific modes:
;;   (slash-commands-for-mode 'org-mode
;;                           '(("todo" . (lambda () (insert "* TODO ")))
;;                             ("heading" . (lambda () (insert "* ")))))

;;; Code:

;; Customization Options

(defgroup slash-commands nil
  "User-configurable slash commands for any mode."
  :group 'convenience
  :prefix "slash-commands-")

(defcustom slash-commands-alist nil
  "Mode-specific slash command definitions.
An alist where each entry is (MAJOR-MODE . COMMANDS-ALIST).
COMMANDS-ALIST is itself an alist of (COMMAND-NAME . FUNCTION)."
  :type '(alist :key-type symbol
                :value-type (alist :key-type string
                                   :value-type function))
  :group 'slash-commands)

(defcustom slash-commands-delay 0.3
  "Delay in seconds before showing the slash command menu."
  :type 'number
  :group 'slash-commands)

(defcustom slash-commands-trigger-chars '(?\s ?\t)
  "Characters after which a slash will trigger commands.
By default, slash commands can be triggered at the beginning of a line
or after whitespace (space or tab)."
  :type '(repeat character)
  :group 'slash-commands)

;; Internal Variables

(defvar-local slash-commands--timer nil
  "Timer for delayed activation of slash command menu.")

(defvar slash-commands--minibuffer-initial-contents ""
  "Keep track of initial contents in minibuffer.")

(defvar slash-commands--should-insert-space nil
  "Flag to indicate whether to insert a space after aborting slash command.")

(defvar slash-commands--should-remove-slash nil
  "Flag to indicate whether to remove the slash after aborting command.")

(defvar slash-commands--command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "/") #'slash-commands-key-pressed)
    map)
  "Keymap for slash-commands minor mode.")

;; Core Functions

(defun slash-commands-for-mode (mode commands-alist)
  "Define slash commands for MODE.
COMMANDS-ALIST is an alist where each entry is (COMMAND-NAME . FUNCTION)."
  (setq slash-commands-alist
        (cons (cons mode commands-alist)
              (assq-delete-all mode slash-commands-alist))))

(defun slash-commands--get-for-current-mode ()
  "Get the slash commands for the current major mode."
  (cdr (assq major-mode slash-commands-alist)))

(defun slash-commands--can-trigger-p ()
  "Return non-nil if slash command can be triggered at point."
  (and (or (bolp)  ; beginning of line
           (and (> (point) 0)  ; not at beginning of buffer
                (memq (char-before) slash-commands-trigger-chars)))
       (slash-commands--get-for-current-mode)))  ; commands exist for this mode

(defun slash-commands--cancel-timer ()
  "Cancel the slash command timer if it's active."
  (when (timerp slash-commands--timer)
    (cancel-timer slash-commands--timer)
    (setq slash-commands--timer nil)))

(defun slash-commands--minibuffer-setup-hook ()
  "Setup hook for tracking initial state of minibuffer."
  (setq slash-commands--minibuffer-initial-contents (minibuffer-contents)))

;; Minibuffer Command Handling

(defun slash-commands--space ()
  "Handle space key in slash command minibuffer."
  (interactive)
  ;; If nothing has been typed yet (we're at initial state), set flag and abort
  (if (string= (minibuffer-contents) slash-commands--minibuffer-initial-contents)
      (progn
        (setq slash-commands--should-insert-space t)
        (abort-recursive-edit))
    ;; Otherwise, insert space as normal
    (insert " ")))

(defun slash-commands--backspace ()
  "Handle backspace key in slash command minibuffer.
If minibuffer is empty, abort and remove the slash character."
  (interactive)
  (if (string= (minibuffer-contents) "")
      (progn
        ;; Set a flag to indicate we should remove the slash
        (setq slash-commands--should-remove-slash t)
        (abort-recursive-edit))
    ;; Otherwise, perform normal backspace
    (delete-char -1)))

(defun slash-commands--post-command-hook ()
  "Hook to run after each command to cancel the timer if user typed something."
  ;; Cancel the timer if the user pressed any key after the slash
  (when (and (timerp slash-commands--timer)
             (or (not (eq (char-before) ?/))   ; If the char before is no longer slash
                 (and (> (point) 1)            ; Or if there are two slashes
                      (eq (char-before (1- (point))) ?/))))
    (slash-commands--cancel-timer)))

(defun slash-commands--timer-trigger ()
  "Function called when the timer expires to show the command menu."
  ;; Clear the timer since it has triggered
  (setq slash-commands--timer nil)
  ;; Only show the menu if we're still right after the slash
  (when (and (> (point) 0)
             (eq (char-before) ?/)
             (not (and (> (point) 1) (eq (char-before (1- (point))) ?/))))
    (slash-commands-execute)))

;; Command Execution

(defun slash-commands-execute ()
  "Show the slash command menu and handle command execution."
  (interactive)
  (let* ((map (make-sparse-keymap))
         (command nil)
         (slash-commands--should-insert-space nil) ; Reset the flag
         (slash-commands--should-remove-slash nil) ; Reset the flag
         (available-commands (slash-commands--get-for-current-mode)))

    ;; If no commands are available for this mode, just keep the slash
    (unless available-commands
      (message "No slash commands defined for %s" major-mode)
      (cl-return-from slash-commands-execute nil))

    ;; Add our special space and backspace handling
    (define-key map (kbd "SPC") #'slash-commands--space)
    (define-key map (kbd "DEL") #'slash-commands--backspace)

    ;; Set up the minibuffer with our tracking and keymap
    (minibuffer-with-setup-hook
        (lambda ()
          (slash-commands--minibuffer-setup-hook)
          (use-local-map (make-composed-keymap map (current-local-map))))
      ;; Show the command menu
      (condition-case nil
          (setq command (completing-read
                         (format "Slash command (%s): " major-mode)
                         (mapcar #'car available-commands)
                         nil t))
        (quit nil)))

    ;; Execute the selected command if one was chosen
    (cond
     ((and command (not (string= command "")))
      (delete-char -1) ; Delete the slash
      (let ((cmd-fn (cdr (assoc command available-commands))))
        (when (functionp cmd-fn)
          (funcall cmd-fn))))
     ;; If we should insert a space (set by special-space function)
     (slash-commands--should-insert-space
      (insert " "))
     ;; If we should remove the slash
     (slash-commands--should-remove-slash
      (delete-char -1)))))

(defun slash-commands-key-pressed ()
  "Handle the slash key being pressed with delayed command menu activation."
  (interactive)
  ;; Cancel any existing timer first
  (slash-commands--cancel-timer)

  (if (slash-commands--can-trigger-p)
      (progn
        (insert "/")
        ;; Set a timer to show the command menu after a delay
        (setq slash-commands--timer
              (run-with-timer slash-commands-delay nil #'slash-commands--timer-trigger)))
    ;; Otherwise, just insert a normal slash
    (insert "/")))

;; Minor Mode Definition

;;;###autoload
(define-minor-mode slash-commands-mode
  "Minor mode for triggering slash commands in the current buffer."
  :lighter " /Cmd"
  :keymap slash-commands--command-map
  (if slash-commands-mode
      (progn
        (add-hook 'post-command-hook #'slash-commands--post-command-hook nil t))
    (slash-commands--cancel-timer)
    (remove-hook 'post-command-hook #'slash-commands--post-command-hook t)))

;;;###autoload
(define-globalized-minor-mode slash-commands-global-mode
  slash-commands-mode
  (lambda ()
    (slash-commands-mode 1)))

(provide 'slash-commands)
;;; slash-commands.el ends here
