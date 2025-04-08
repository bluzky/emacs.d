;;; org-slash-commands.el --- Simple slash command system for Org mode

;;; Commentary:
;; Type a slash to trigger commands.
;; Press space immediately to cancel.
;; Type letters then space to continue normal completion.

;;; Code:
(require 'org)

(defvar org-slash-commands-alist
  '(("todo" . (lambda () (org-todo "TODO")))
    ("done" . (lambda () (org-todo "DONE")))
    ("heading" . (lambda () (org-insert-heading)))
    ("subheading" . (lambda () (org-insert-subheading nil)))
    ("checkbox" . (lambda () (insert "[ ] ")))
    ("table" . (lambda () (org-table-create "3x3")))
    ("link" . (lambda () (org-insert-link)))
    ("code" . (lambda ()
                (insert "#+BEGIN_SRC \n\n#+END_SRC")
                (forward-line -1)))
    ("today" . (lambda () (insert (format-time-string "%Y-%m-%d"))))
    ("time" . (lambda () (insert (format-time-string "%H:%M:%S")))))
  "Alist of slash commands and their actions.")

(defvar org-slash-minibuffer-initial-contents ""
  "Keep track of initial contents in minibuffer.")

(defun org-slash-minibuffer-setup-hook ()
  "Setup hook for tracking initial state of minibuffer."
  (setq org-slash-minibuffer-initial-contents (minibuffer-contents)))

(defvar org-slash-should-insert-space nil
  "Flag to indicate whether to insert a space after aborting slash command.")

(defun org-slash-special-space ()
  "Handle space key in slash command minibuffer."
  (interactive)
  ;; If nothing has been typed yet (we're at initial state), set flag and abort
  (if (string= (minibuffer-contents) org-slash-minibuffer-initial-contents)
      (progn
        (setq org-slash-should-insert-space t)
        (abort-recursive-edit))
    ;; Otherwise, insert space as normal
    (insert " ")))

(defun org-slash-execute-command ()
  "Show the slash command menu and handle command execution."
  (interactive)
  (let* ((map (make-sparse-keymap))
         (command nil)
         (org-slash-should-insert-space nil)) ; Reset the flag

    ;; Add our special space handling
    (define-key map (kbd "SPC") 'org-slash-special-space)

    ;; Set up the minibuffer with our tracking and keymap
    (minibuffer-with-setup-hook
        (lambda ()
          (org-slash-minibuffer-setup-hook)
          (use-local-map (make-composed-keymap map (current-local-map))))

      ;; Show the command menu
      (condition-case nil
          (setq command (completing-read "Command: "
                                         (mapcar #'car org-slash-commands-alist)))
        (quit nil)))

    ;; Execute the selected command if one was chosen
    (cond
     ((and command (not (string= command "")))
      (delete-char -1) ; Delete the slash
      (funcall (cdr (assoc command org-slash-commands-alist))))

     ;; If we should insert a space (set by special-space function)
     (org-slash-should-insert-space
      (insert " ")))))  ; Insert space in the org buffer

(defun org-slash-key-pressed ()
  "Handle the slash key being pressed."
  (interactive)
  (cond
   ;; Double slash: insert a literal slash
   ((and (> (point) 0) (eq (char-before) ?/))
    (delete-char -1)
    (insert "/"))

   ;; Normal slash: show command menu
   (t
    (insert "/")
    (org-slash-execute-command))))

(defun org-slash-commands-enable ()
  "Enable the slash command system."
  (interactive)
  (define-key org-mode-map (kbd "/") #'org-slash-key-pressed))

(provide 'org-slash-commands)

;;; org-slash-commands.el ends here
