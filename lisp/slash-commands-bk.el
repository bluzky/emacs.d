;;; slash-commands-bk.el --- User-configurable slash commands for any mode
;;; Commentary:
;; Type a slash to trigger custom commands.
;; Configure commands per mode with `slash-commands-for-mode`.
;;; Code:

;; User configuration variable
(defvar slash-commands-alist nil
  "Mode-specific slash command definitions.
An alist where each entry is (MAJOR-MODE . COMMANDS-ALIST).
COMMANDS-ALIST is itself an alist of (COMMAND-NAME . FUNCTION).")

;; Timer variables
(defvar slash-command-timer nil
  "Timer for delayed activation of slash command menu.")

(defvar slash-command-delay 0.5
  "Delay in seconds before showing the slash command menu.")

;; Minibuffer configuration
(defvar slash-minibuffer-initial-contents ""
  "Keep track of initial contents in minibuffer.")

(defvar slash-should-insert-space nil
  "Flag to indicate whether to insert a space after aborting slash command.")

(defvar slash-should-remove-slash nil
  "Flag to indicate whether to insert a space after aborting slash command.")


;; Core functions
(defun slash-commands-for-mode (mode commands-alist)
  "Define slash commands for MODE.
COMMANDS-ALIST is an alist where each entry is (COMMAND-NAME . FUNCTION)."
  (setq slash-commands-alist
        (cons (cons mode commands-alist)
              (assq-delete-all mode slash-commands-alist))))

(defun slash-get-commands-for-current-mode ()
  "Get the slash commands for the current major mode."
  (cdr (assq major-mode slash-commands-alist)))

(defun slash-minibuffer-setup-hook ()
  "Setup hook for tracking initial state of minibuffer."
  (setq slash-minibuffer-initial-contents (minibuffer-contents)))

(defun slash-command-space ()
  "Handle space key in slash command minibuffer."
  (interactive)
  ;; If nothing has been typed yet (we're at initial state), set flag and abort
  (if (string= (minibuffer-contents) slash-minibuffer-initial-contents)
      (progn
        (setq slash-should-insert-space t)
        (abort-recursive-edit))
    ;; Otherwise, insert space as normal
    (insert " ")))

(defun slash-command-backspace ()
  "Handle backspace key in slash command minibuffer.
If minibuffer is empty, abort and remove the slash character."
  (interactive)
  (if (string= (minibuffer-contents) "")
      (progn
        ;; Set a flag to indicate we should remove the slash
        (setq slash-should-remove-slash t)
        (abort-recursive-edit))
    ;; Otherwise, perform normal backspace
    (delete-char -1)))

(defun slash-cancel-timer ()
  "Cancel the slash command timer if it's active."
  (when slash-command-timer
    (cancel-timer slash-command-timer)
    (setq slash-command-timer nil)))

(defun slash-post-command-hook ()
  "Hook to run after each command to cancel the timer if user typed something."
  ;; Cancel the timer if the user pressed any key after the slash
  (when (and slash-command-timer
             (or (not (eq (char-before) ?/))   ; If the char before is no longer slash
                 (and (> (point) 1)            ; Or if there are two slashes
                      (eq (char-before (1- (point))) ?/))))
    (slash-cancel-timer)))

(defun slash-timer-trigger ()
  "Function called when the timer expires to show the command menu."
  ;; Clear the timer since it has triggered
  (setq slash-command-timer nil)
  ;; Only show the menu if we're still right after the slash
  (message "Slash command menu triggered")
  (when (and (> (point) 0)
             (eq (char-before) ?/)
             (not (and (> (point) 1) (eq (char-before (1- (point))) ?/))))
    (slash-execute-command)))

(defun slash-execute-command ()
  "Show the slash command menu and handle command execution."
  (interactive)
  (let* ((map (make-sparse-keymap))
         (command nil)
         (slash-should-insert-space nil) ; Reset the flag
         (available-commands (slash-get-commands-for-current-mode)))

    ;; If no commands are available for this mode, just keep the slash
    (unless available-commands
      (message "No slash commands defined for %s" major-mode)
      (cl-return-from slash-execute-command nil))

    ;; Add our special space handling
    (define-key map (kbd "SPC") 'slash-command-space)
    (define-key map (kbd "DEL") 'slash-command-backspace)

    ;; Set up the minibuffer with our tracking and keymap
    (minibuffer-with-setup-hook
        (lambda ()
          (slash-minibuffer-setup-hook)
          (use-local-map (make-composed-keymap map (current-local-map))))
      ;; Show the command menu
      (condition-case nil
          (setq command (completing-read "Command: "
                                         (mapcar #'car available-commands)))
        (quit nil)))

    ;; Execute the selected command if one was chosen
    (cond
     ((and command (not (string= command "")))
      (delete-char -1) ; Delete the slash
      (funcall (cdr (assoc command available-commands))))
     ;; If we should insert a space (set by special-space function)
     (slash-should-insert-space
      (insert " "))

     (slash-should-remove-slash
      (delete-char -1))
     )))

(defun slash-key-pressed ()
  "Handle the slash key being pressed with delayed command menu activation."
  (interactive)
  ;; Cancel any existing timer first
  (slash-cancel-timer)

  (cond
   ;; Only show command menu when at beginning of line or after whitespace
   ;; And only if commands are defined for this mode
   ((and (or (bolp)  ; beginning of line
             (and (> (point) 0)  ; not at beginning of buffer
                  (memq (char-before) '(?\s ?\t))))  ; space or tab before point
         (slash-get-commands-for-current-mode))  ; commands exist for this mode
    (insert "/")
    ;; Set a timer to show the command menu after a delay
    (setq slash-command-timer
          (run-with-timer slash-command-delay nil #'slash-timer-trigger)))

   ;; Otherwise, just insert a normal slash
   (t
    (insert "/"))))

(defun slash-commands-enable ()
  "Enable the slash command system."
  (interactive)
  (define-key (current-local-map) (kbd "/") #'slash-key-pressed)
  ;; Add post-command hook to cancel timer if user types something else
  (add-hook 'post-command-hook #'slash-post-command-hook))

(defun slash-commands-disable ()
  "Disable the slash command system."
  (interactive)
  (define-key (current-local-map) (kbd "/") nil)
  (remove-hook 'post-command-hook #'slash-post-command-hook)
  (slash-cancel-timer))

(provide 'slash-commands-bk)
;;; slash-commands-bk.el ends here
