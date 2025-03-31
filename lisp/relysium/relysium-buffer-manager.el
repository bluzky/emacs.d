;;; relysium-buffer-manager.el --- Buffer management for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains buffer management functionality for the relysium package.
;; It handles the creation and management of buffer-local chat buffers for code buffers.

;;; Code:

(require 'gptel)

(defvar-local relysium--buffer-chat nil
  "Buffer-local variable to store the associated chat buffer for the current code buffer.")

(defcustom relysium-window-size 0.33
  "Size of the elysium chat window as a fraction of the frame.
Must be a number between 0 and 1, exclusive."
  :type 'float
  :group 'relysium
  :set (lambda (symbol value)
         (if (and (numberp value)
                  (< 0 value 1))
             (set-default symbol value)
           (user-error "Relysium-window-size must be a number between 0 and 1, exclusive"))))

(defcustom relysium-window-style 'vertical
  "Specify the orientation.  It can be 'horizontal, 'vertical, or nil."
  :type '(choice (const :tag "Horizontal" horizontal)
                 (const :tag "Vertical" vertical)
                 (const :tag "None" nil)))

;;;###autoload
(defun relysium-buffer-get-chat-buffer ()
  "Get or create a chat buffer for the current code buffer."
  (unless (and relysium--buffer-chat
               (buffer-live-p relysium--buffer-chat))
    (let* ((code-buffer-name (buffer-name))
           (chat-buffer-name (format "*relysium:%s*" code-buffer-name)))
      ;; Create the chat buffer
      (setq relysium--buffer-chat (gptel chat-buffer-name))
      ;; Hide it from buffer list
      (with-current-buffer relysium--buffer-chat
        ;; Using buffer-list nil hides the buffer from buffer-list functions
        (setq-local buffer-list-update-hook
                    (cons (lambda () (setq list-buffers-directory nil))
                          buffer-list-update-hook))
        (set-buffer-modified-p nil)
        ;; Mark the buffer as auxiliary (hidden in many UIs)
        (when (fboundp 'doom-mark-buffer-as-real-h)
          (unwind-protect
              (doom-mark-buffer-as-real-h)
                                        ; avoid error if using doom
            (ignore-errors 'doom-mark-buffer-as-real-h)))
        (set-buffer-modified-p nil))
      ;; Return the newly created and configured buffer
      relysium--buffer-chat))
  relysium--buffer-chat)

;;;###autoload
(defun relysium-buffer-setup-windows (&optional keep-focus)
  "Set up the coding assistant layout with the buffer-local chat window.
When KEEP-FOCUS is non-nil, keep the focus on the code buffer after setup."
  (let* ((code-buffer (current-buffer))
         (chat-buffer (relysium-buffer-get-chat-buffer))
         (main-window (selected-window)))

    (when relysium-window-style
      (delete-other-windows)

      (let ((split-size (floor (* (if (eq relysium-window-style 'vertical)
                                      (frame-width)
                                    (frame-height))
                                  (- 1 relysium-window-size)))))
        (if (eq relysium-window-style 'vertical)
            (split-window-right split-size)
          (split-window-below split-size))
        (set-window-buffer main-window code-buffer)
        (other-window 1)
        (set-window-buffer (selected-window) chat-buffer)
        ;; Move focus back to the code buffer if requested
        (when keep-focus
          (other-window 1))))))

;;;###autoload
(defun relysium-buffer-toggle-window ()
  "Toggle the buffer-local elysium chat window."
  (interactive)
  (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
    (if (get-buffer-window chat-buffer)
        (delete-window (get-buffer-window chat-buffer))
      (relysium-buffer-setup-windows))))

;;;###autoload
(defun relysium-buffer-clear ()
  "Clear the buffer-local elysium buffer."
  (interactive)
  (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
    (with-current-buffer chat-buffer
      (erase-buffer)
      (insert (gptel-prompt-prefix-string)))))

;;;###autoload
(defun relysium-buffer-add-context (content)
  "Add CONTENT as context to the buffer-local elysium chat buffer."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (buffer-substring-no-properties (point-min) (point-max)))))

  (let* ((chat-buffer (relysium-buffer-get-chat-buffer))
         (code-buffer-language
          (string-trim-right
           (string-trim-right (symbol-name major-mode) "-ts-mode$") "-mode$")))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert "\n")
      (insert (format "```%s\n%s\n```" code-buffer-language content))
      (insert "\n"))))

;;;###autoload
(defun relysium-buffer-switch-to-chat ()
  "Switch to the buffer-local chat buffer for the current code buffer."
  (interactive)
  (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
    (switch-to-buffer chat-buffer)))

;;;###autoload
(defun relysium-buffer-append-user-message (message)
  "Append user MESSAGE to the current buffer's chat buffer."
  (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert "\n\n### USER:\n")
      (insert message)
      (insert "\n"))))

;;;###autoload
(defun relysium-buffer-append-assistant-message (message)
  "Append assistant MESSAGE to the current buffer's chat buffer."
  (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert "\n\n### ASSISTANT:\n")
      (insert message)
      (insert "\n\n### "))))

;;;###autoload
(defun relysium-buffer-prepare-request (message)
  "Prepare the chat buffer for an LLM request with MESSAGE.
Returns the chat buffer."
  (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
    (relysium-buffer-append-user-message message)
    chat-buffer))

(provide 'relysium-buffer-manager)
;;; relysium-buffer-manager.el ends here
