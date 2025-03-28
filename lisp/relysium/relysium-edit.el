;;; relysium-edit.el --- Edit functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions related to editing code through
;; LLM-generated suggestions using the relysium package.

;;; Code:

(require 'gptel)
(require 'smerge-mode)
(require 'relysium-utils)
(require 'relysium-common)

(defvar relysium--last-query nil
  "The last query sent to the LLM.")

(defvar relysium--last-code-buffer nil
  "The buffer that was last modified by Elysium.")

(defvar relysium-edit-prompt "Act as an expert software developer.
Always use best practices when coding.
Respect and use existing conventions, libraries, etc that are already present in the code base.

Make sure code comments are in English when generating them.

Your task is to modify the provided code according to the user's request. Follow these instructions precisely:

1. Response rules:
   - *DO NOT* include three backticks: ``` in your suggestion! Treat the suggested code AS IS.
   - The code you return must be wrapped in <code></code>, and cannot contain any other <code>.

2. Code modification rules:
   - *DO NOT* include any explanations, comments.
   - Ensure the returned code is complete and can be directly used as a replacement for the original code.
   - Preserve the original structure, indentation, and formatting of the code as much as possible.
   - Only modify the specific lines requested in the range - no more, no less
   - Maintain the *SAME INDENTATION* in the returned code as in the source code
   - *ONLY* return the new code snippets to be updated, *DO NOT* return the entire file content.
   - If no selected code is provided, *DO NOT* return any code from reference source code.

Remember that Your response SHOULD CONTAIN ONLY THE MODIFIED CODE to be used as DIRECT REPLACEMENT to the original file.

There is an example below:

Original code:
```python
def add(a, b):
    return a + b

result = add(2, 3)
print(result)
```

Selected code:
Line range: 1-2
```python
def add(a, b):
    return a + b
```

User request:
Print the result

Your response:
<code>
def add(a, b):
    print(a + b)
    return a + b
</code>
")

;;;###autoload
(defun relysium-query (user-query)
  "Send USER-QUERY to elysium from the current buffer."
  (interactive (list (read-string "User Query: ")))
  (unless (buffer-live-p relysium--chat-buffer)
    (setq relysium--chat-buffer (gptel "*elysium*")))

  (let* ((code-buffer (current-buffer))
         (chat-buffer relysium--chat-buffer)
         (using-region (use-region-p))
         (current-cursor-line (line-number-at-pos (point)))
         ;; Get buffer content (whole or region)
         (start-pos (if using-region
                        (region-beginning)
                      (point-min)))
         (end-pos (if using-region
                      (region-end)
                    (point-max)))
         ;; Get exact line positions
         (start-line-pos (save-excursion
                           (goto-char start-pos)
                           (line-beginning-position)))
         (end-line-pos (save-excursion
                         (goto-char end-pos)
                         (if (and (> end-pos start-pos)
                                  (= (line-beginning-position) end-pos))
                             ;; If at beginning of line, use previous line's end
                             (progn (backward-char 1)
                                    (line-end-position))
                           (line-end-position))))
         (selected-code (buffer-substring-no-properties start-line-pos end-line-pos))
         ;; Set start and end lines
         (start-line (if using-region
                         (line-number-at-pos start-line-pos)
                       current-cursor-line))
         (end-line (if using-region
                       (line-number-at-pos end-line-pos)
                     current-cursor-line))
         (file-type (symbol-name major-mode))
         ;; Apply trimming and line adjustment only for regions
         (adjustment-result (when using-region
                              (trim-empty-lines-and-adjust selected-code start-line end-line)))
         (final-code (if using-region (nth 0 adjustment-result) selected-code))
         (final-start-line (if using-region (nth 1 adjustment-result) start-line))
         (final-end-line (if using-region (nth 2 adjustment-result) end-line))
         ;; Include indentation info in the query (omit line range if no region)
         (full-query (if using-region
                         (format "\n\nFile type: %s\nLine range: %d-%d\nCursor line: %d\n%s\n\nSelected code:\n```\n%s\n```\n\n%s"
                                 file-type
                                 final-start-line
                                 final-end-line
                                 current-cursor-line
                                 ""
                                 final-code
                                 user-query)
                       (format "\n\nFile type: %s\nCursor line: %d\n%s\n\nWhole source code:\n```\n%s\n```\n\n%s"
                               file-type
                               current-cursor-line
                               "No code selected - using cursor line as context"
                               final-code
                               user-query))))

    (setq relysium--last-query user-query)
    (setq relysium--last-code-buffer code-buffer)

    ;; Store region info for later use
    (setq-local relysium--using-region using-region)
    (setq-local relysium--region-start-line final-start-line)
    (setq-local relysium--region-end-line final-end-line)

    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert "\n\n### USER:\n")
      (insert full-query)
      (insert "\n"))

    (gptel--update-status " Waiting..." 'warning)
    (message "Querying %s for lines %d-%d..."
             (gptel-backend-name gptel-backend)
             final-start-line final-end-line)
    (deactivate-mark)

    (gptel-request full-query
      :system relysium-edit-prompt
      :buffer chat-buffer
      :callback (apply-partially #'relysium-handle-response code-buffer))))

(defun relysium-handle-response (code-buffer response info)
  "Handle the RESPONSE from gptel.
The changes will be applied to CODE-BUFFER in a git merge format.
INFO is passed into this function from the `gptel-request' function."
  (when response
    ;; Log the full response if debug mode is enabled
    (relysium-debug-log "LLM Response:\n%s" response)

    ;; Add this section to show the full response in the chat buffer
    (with-current-buffer relysium--chat-buffer
      (goto-char (point-max))
      (insert "\n\n### ASSISTANT:\n")
      (insert response)
      (insert "\n\n### "))

    (let ((code-change (relysium-extract-edit-changes response))
          (using-region (buffer-local-value 'relysium--using-region code-buffer)))

      ;; Log the extracted code if debug mode is enabled
      (when relysium-debug-mode
        (relysium-debug-log "Extracted code change: %s"
                            (if code-change
                                (format "%s" code-change)
                              "None found")))

      ;; mark undo boundary
      (with-current-buffer code-buffer
        (undo-boundary))

      (when code-change
        ;; Apply change
        (with-current-buffer code-buffer
          ;; Create a change structure with the code
          (let* ((current-line (line-number-at-pos (point)))
                 (start-line (if using-region
                                 (buffer-local-value 'relysium--region-start-line code-buffer)
                               current-line))
                 (end-line (if using-region
                               (buffer-local-value 'relysium--region-end-line code-buffer)
                             current-line))
                 (change (list :start start-line
                               :end end-line
                               :code code-change)))

            (relysium-apply-code-changes code-buffer (list change))

            ;; Activate smerge mode and show transient menu
            (smerge-mode 1)
            (goto-char (point-min))
            (ignore-errors (smerge-next))
            (relysium-transient-menu))))

      ;; Update status
      (with-current-buffer relysium--chat-buffer
        (gptel--sanitize-model)
        (gptel--update-status " Ready" 'success)))))

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

(defun relysium-retry-query ()
  "Retry the last query with modifications, preserving the previously marked region."
  (interactive)
  (let ((new-query (read-string "Modify query: " relysium--last-query)))
    (when new-query
      (with-current-buffer relysium--last-code-buffer
        ;; Discard current suggestions
        (relysium-discard-all-suggested-changes)

        ;; Restore the region if a region was previously used
        (when (buffer-local-value 'relysium--using-region relysium--last-code-buffer)
          (let* ((point-min (point-min))
                 (start-line (buffer-local-value 'relysium--region-start-line relysium--last-code-buffer))
                 (end-line (buffer-local-value 'relysium--region-end-line relysium--last-code-buffer))
                 start-pos end-pos)
            ;; Set point to start line
            (setq start-pos (goto-char point-min))
            (forward-line (1- start-line))
            (setq start-pos (point))
            ;; Set mark to end line
            (goto-char point-min)
            (forward-line (1- end-line))
            (end-of-line)
            (setq end-pos (point))
            (set-mark start-pos)))

        ;; Execute the new query
        (relysium-query new-query)))))

(defun relysium-query-dwim ()
  "Query elysium with the region if active, otherwise prompt for a query."
  (interactive)
  (if (use-region-p)
      (call-interactively 'relysium-query)
    (let ((current-prefix-arg '(4))) ; Simulate C-u prefix to prompt for region
      (call-interactively 'relysium-query))))

(provide 'relysium-edit)
;;; relysium-edit.el ends here
