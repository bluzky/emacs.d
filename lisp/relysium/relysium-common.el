;;; relysium-common.el --- Common functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains common functions and variables shared between
;; different relysium modules.

;;; Code:

(require 'gptel)
(require 'smerge-mode)

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

(defvar relysium--chat-buffer nil
  "Buffer used for LLM interaction.")

(defun relysium-setup-windows ()
  "Set up the coding assistant layout with the chat window."
  (unless (buffer-live-p relysium--chat-buffer)
    (setq relysium--chat-buffer
          (gptel "*relysium*")))

  (when relysium-window-style
    (delete-other-windows)

    (let* ((main-buffer (current-buffer))
           (main-window (selected-window))
           (split-size (floor (* (if (eq relysium-window-style 'vertical)
                                     (frame-width)
                                   (frame-height))
                                 (- 1 relysium-window-size)))))
      (with-current-buffer relysium--chat-buffer)
      (if (eq relysium-window-style 'vertical)
          (split-window-right split-size)
        (split-window-below split-size))
      (set-window-buffer main-window main-buffer)
      (other-window 1)
      (set-window-buffer (selected-window) relysium--chat-buffer))))

(defun relysium-toggle-window ()
  "Toggle the elysium chat window."
  (interactive)
  (if (and (buffer-live-p relysium--chat-buffer)
           (get-buffer-window relysium--chat-buffer))
      (delete-window (get-buffer-window relysium--chat-buffer))
    (relysium-setup-windows)))

(defun relysium-clear-buffer ()
  "Clear the elysium buffer."
  (interactive)
  (when (buffer-live-p relysium--chat-buffer)
    (with-current-buffer relysium--chat-buffer
      (erase-buffer)
      (insert (gptel-prompt-prefix-string)))))

(defun relysium-add-context (content)
  "Add CONTENT as context to the elysium chat buffer."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (buffer-substring-no-properties (point-min) (point-max)))))
  ;; Ensure chat buffer exists
  (unless (buffer-live-p relysium--chat-buffer)
    (setq relysium--chat-buffer (gptel "*relysium*")))

  (let ((code-buffer-language
         (string-trim-right
          (string-trim-right (symbol-name major-mode) "-ts-mode$") "-mode$")))
    (with-current-buffer relysium--chat-buffer
      (goto-char (point-max))
      (insert "\n")
      (insert (format "```%s\n%s\n```" code-buffer-language content))
      (insert "\n"))))

(defun relysium-keep-all-suggested-changes ()
  "Keep all of the LLM suggestions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-lower))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-lower))
    (smerge-mode -1)
    (message "All suggested changes applied")))

(defun relysium-discard-all-suggested-changes ()
  "Discard all of the LLM suggestions."
  (interactive)
  (undo)
  (smerge-mode -1)
  (message "All suggested changes discarded"))

(defun relysium-navigate-next-change ()
  "Navigate to the next change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-next))
      (message "Navigated to next change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (relysium-transient-menu))

(defun relysium-navigate-prev-change ()
  "Navigate to the previous change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-prev))
      (message "Navigated to previous change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (relysium-transient-menu))

(defun relysium-keep-current-change ()
  "Keep the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-lower)
  (if (ignore-errors (not (smerge-next)))
      (progn
        (message "All changes reviewed - no more conflicts")
        (smerge-mode -1))
    (message "Applied change - move to next")
    ;; Keep the transient menu active if there are more changes
    (relysium-transient-menu)))

(defun relysium-reject-current-change ()
  "Reject the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-upper)
  (if (ignore-errors (not (smerge-next)))
      (progn
        (message "All changes reviewed - no more conflicts")
        (smerge-mode -1))
    (message "Rejected change - move to next")
    ;; Keep the transient menu active if there are more changes
    (relysium-transient-menu)))


(provide 'relysium-common)
;;; relysium-common.el ends here
