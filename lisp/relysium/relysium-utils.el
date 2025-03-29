;;; relysium-utils.el --- Utility functions for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains utility functions for the relysium package, including:
;; - Diff generation and application
;; - Code change handling
;; - Debug logging utilities

;;; Code:

(require 'simple-diff)

(defcustom relysium-debug-mode nil
  "When non-nil, log LLM responses and other debug information."
  :group 'relysium
  :type 'boolean)

(defcustom relysium-debug-buffer-name "*relysium-debug*"
  "Name of the buffer for debug logging."
  :group 'relysium
  :type 'string)

(defun relysium-debug-log (message &rest args)
  "Log MESSAGE with ARGS to the debug buffer if debug mode is enabled."
  (when relysium-debug-mode
    (let ((debug-buffer (get-buffer-create relysium-debug-buffer-name)))
      (with-current-buffer debug-buffer
        (goto-char (point-max))
        (let ((start (point)))
          (insert (format "[%s] " (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert (apply #'format message args))
          (insert "\n\n")
          ;; Add some properties to make it easier to read
          (add-text-properties start (point) '(face font-lock-comment-face)))))))

(defun relysium-toggle-debug-mode ()
  "Toggle elysium debug mode."
  (interactive)
  (setq relysium-debug-mode (not relysium-debug-mode))
  (message "Elysium debug mode %s" (if relysium-debug-mode "enabled" "disabled"))
  (when relysium-debug-mode
    (display-buffer (get-buffer-create relysium-debug-buffer-name))))

(defun relysium-clear-debug-buffer ()
  "Clear the elysium debug buffer."
  (interactive)
  (when-let ((buffer (get-buffer relysium-debug-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "[%s] Debug buffer cleared\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S"))))))

(defun relysium-extract-edit-changes (response)
  "Extract the code from the first <code> block in RESPONSE.
Returns the code as a string, or nil if no code block is found."
  (let ((code-block-regex
         "<code>\n\\(\\(?:.\\|\n\\)*?\\)</code>"))
    ;; Extract just the first code block
    (if (string-match code-block-regex response)
        ;; Return the extracted code as a string
        (match-string 1 response)
      ;; No code block found
      nil)))

(defun relysium--adjust-changes-for-region (changes)
  "Adjust CHANGES line numbers based on the selected region.
Makes sure changes are properly aligned with the actual lines in the buffer."
  (when (and (boundp 'relysium--region-start-line)
             (local-variable-p 'relysium--region-start-line)
             relysium--region-start-line)
    ;; We need to offset all line numbers by the start line of the region
    (let ((offset (1- relysium--region-start-line)))
      (mapcar (lambda (change)
                (list :start (+ (plist-get change :start) offset)
                      :end (+ (plist-get change :end) offset)
                      :code (plist-get change :code)))
              changes)))
  changes)

(defun trim-empty-lines-and-adjust (string start-line end-line)
  "Trim leading and trailing empty lines and adjust line numbers.
STRING is the text to trim, START-LINE and END-LINE are the line numbers."
  (with-temp-buffer
    ;; Insert the string into a temporary buffer
    (insert string)

    ;; Count leading empty lines
    (goto-char (point-min))
    (let ((leading-empty-lines 0)
          (trailing-empty-lines 0)
          (adjusted-start-line start-line)
          (adjusted-end-line end-line)
          (trimmed-string nil))

      ;; Count leading empty lines
      (while (and (not (eobp))
                  (looking-at "^\\s-*$"))
        (setq leading-empty-lines (1+ leading-empty-lines))
        (forward-line 1))

      ;; Count trailing empty lines
      (goto-char (point-max))
      (when (not (bobp))
        (forward-line -1))
      (while (and (not (bobp))
                  (looking-at "^\\s-*$"))
        (setq trailing-empty-lines (1+ trailing-empty-lines))
        (forward-line -1))

      ;; Adjust start and end line numbers
      (setq adjusted-start-line (+ start-line leading-empty-lines))
      (setq adjusted-end-line (- end-line trailing-empty-lines))

      ;; Extract the trimmed string
      (goto-char (point-min))
      (when (> leading-empty-lines 0)
        (forward-line leading-empty-lines)
        (delete-region (point-min) (point)))

      (goto-char (point-max))
      (when (> trailing-empty-lines 0)
        (forward-line (- trailing-empty-lines))
        (delete-region (point) (point-max)))

      (setq trimmed-string (buffer-string))

      ;; Return a list with the trimmed string and adjusted line numbers
      (list trimmed-string adjusted-start-line adjusted-end-line))))

(defun relysium-apply-code-changes (buffer code-changes)
  "Apply CODE-CHANGES to BUFFER in a git merge format.
Uses simple conflict markers to highlight the differences between
original and suggested code. Breaks down large changes into smaller chunks
for easier review.
Uses exclusive line ranges where end points to the line after the last line to change."
  (with-current-buffer buffer
    (save-excursion
      (let ((offset 0))
        (dolist (change code-changes)
          (let* ((start (plist-get change :start))
                 (end (plist-get change :end))
                 (new-code (plist-get change :code))
                 ;; Detect insert operations where start == end (exclusive range)
                 (is-insert (= start end))
                 (orig-code-start (progn
                                    (goto-char (point-min))
                                    (forward-line (1- (+ start offset)))
                                    (point)))
                 (orig-code-end (progn
                                  (goto-char (point-min))
                                  (forward-line (1- (+ end offset)))
                                  (point)))
                 (orig-code (buffer-substring-no-properties orig-code-start orig-code-end)))
            (when (string= orig-code "\n")
              -              (setq orig-code ""))

            ;; Apply the appropriate type of change
            (if is-insert
                ;; Insert-only change
                (relysium--apply-insert-change orig-code-start new-code)
              ;; Normal replacement change
              (if (< (- end start) 7)
                  (relysium--apply-simple-change orig-code-start orig-code-end orig-code new-code)
                (relysium--apply-refined-change orig-code-start orig-code-end orig-code new-code)))

            ;; Update offset - We need to recalculate the total lines now
            (let* ((new-line-count (count-lines orig-code-start (point)))
                   (original-line-count (- end start)) ;; For exclusive ranges
                   (line-diff (- new-line-count original-line-count)))
              (setq offset (+ offset line-diff)))))))
    (run-hooks 'relysium-apply-changes-hook)))

(defun relysium--apply-insert-change (position new-code)
  "Handle insert-only changes with conflict markers.
Insert at POSITION a conflict section containing just the NEW-CODE."
  (goto-char position)
  (insert (concat "<<<<<<< HEAD\n"
                  "=======\n"
                  new-code
                  "\n>>>>>>> Relysium \n")))

(defun relysium--apply-simple-change (start end orig-code new-code)
  "Apply a simple change with conflict markers.
Replace the region from START to END containing ORIG-CODE with conflict markers
containing both ORIG-CODE and NEW-CODE."
  (delete-region start end)
  (goto-char start)
  (insert (concat "<<<<<<< HEAD\n"
                  orig-code
                  "=======\n"
                  new-code
                  "\n>>>>>>> Relysium \n")))

(defun relysium--apply-refined-change (start end orig-code new-code)
  "Apply a refined change that breaks code into smaller conflict chunks.
Replace the region from START to END containing ORIG-CODE with a refined diff
against NEW-CODE, using conflict markers for each meaningful chunk."
  (delete-region start end)
  (goto-char start)

  (insert (simple-diff-merge-strings orig-code new-code "HEAD" "Relysium"))
  )

(provide 'relysium-utils)
;;; relysium-utils.el ends here
