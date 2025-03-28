;;; relysium-utils.el --- Utility functions for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains utility functions for the relysium package, including:
;; - Diff generation and application
;; - Code change handling
;; - Debug logging utilities

;;; Code:

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
for easier review."
  (with-current-buffer buffer
    (save-excursion
      (let ((offset 0))
        (dolist (change code-changes)
          (let* ((start (plist-get change :start))
                 (end (plist-get change :end))
                 (new-code (plist-get change :code))
                 (orig-code-start (progn
                                    (goto-char (point-min))
                                    (forward-line (1- (+ start offset)))
                                    (point)))
                 (orig-code-end (progn
                                  (goto-char (point-min))
                                  (forward-line (1- (+ end offset 1)))
                                  (point)))
                 (orig-code (buffer-substring-no-properties orig-code-start orig-code-end)))
            (when (string= orig-code "\n")
              (setq orig-code ""))

                (relysium--apply-refined-change orig-code-start orig-code-end orig-code new-code)

            ;; Update offset - We need to recalculate the total lines now
            (let* ((new-line-count (count-lines orig-code-start (point)))
                   (original-line-count (- end start -1)) ; -1 because line range is inclusive
                   (line-diff (- new-line-count original-line-count)))
              (setq offset (+ offset line-diff)))))))
    (run-hooks 'relysium-apply-changes-hook)))

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

  ;; Split both code blocks into lines
  (let* ((orig-lines (split-string orig-code "\n"))
         (new-lines (split-string new-code "\n"))
         (chunks (relysium--create-diff-chunks orig-lines new-lines)))

    ;; Insert each chunk with appropriate conflict markers
    (dolist (chunk chunks)
      (let ((chunk-type (car chunk))
            (orig-chunk-lines (nth 1 chunk))
            (new-chunk-lines (nth 2 chunk)))

        (cond
         ;; Lines that are the same in both versions - no conflict needed
         ((eq chunk-type 'same)
          (let ((text (string-join orig-chunk-lines "\n")))
            (insert text)
            (when (> (length text) 0)
              (insert "\n"))))

         ;; Lines that differ - add conflict markers
         ((eq chunk-type 'diff)
          (let ((orig-text (string-join orig-chunk-lines "\n"))
                (new-text (string-join new-chunk-lines "\n")))
            (insert "<<<<<<< HEAD\n")
            (when (> (length orig-text) 0)
              (insert orig-text "\n"))
            (insert "=======\n")
            (when (> (length new-text) 0)
              (insert new-text "\n"))
            (insert ">>>>>>> Relysium\n"))))))))

(defun relysium--create-diff-chunks (orig-lines new-lines)
  "Create a list of diff chunks between ORIG-LINES and NEW-LINES.
Each chunk is of the form (TYPE ORIG-CHUNK NEW-CHUNK) where:
- TYPE is either 'same or 'diff
- ORIG-CHUNK is a list of lines from the original text
- NEW-CHUNK is a list of lines from the new text

For 'same chunks, ORIG-CHUNK and NEW-CHUNK contain the same lines."
  (let ((chunks nil)
        (i 0)
        (j 0)
        (orig-len (length orig-lines))
        (new-len (length new-lines))
        (current-chunk-type nil)
        (current-orig-chunk nil)
        (current-new-chunk nil))

    ;; Compare lines and build chunks
    (while (or (< i orig-len) (< j new-len))
      (let ((orig-line (when (< i orig-len) (nth i orig-lines)))
            (new-line (when (< j new-len) (nth j new-lines)))
            (lines-match (and (< i orig-len)
                              (< j new-len)
                              (string= (nth i orig-lines) (nth j new-lines)))))

        (if lines-match
            ;; Lines match - they're part of a 'same' chunk
            (progn
              ;; If we were in a 'diff' chunk, finalize it
              (when (eq current-chunk-type 'diff)
                (push (list 'diff (reverse current-orig-chunk) (reverse current-new-chunk)) chunks)
                (setq current-orig-chunk nil
                      current-new-chunk nil))

              ;; Add to or start a 'same' chunk
              (if (eq current-chunk-type 'same)
                  (progn
                    (push orig-line current-orig-chunk)
                    (push new-line current-new-chunk))
                (setq current-chunk-type 'same
                      current-orig-chunk (list orig-line)
                      current-new-chunk (list new-line)))

              ;; Move to next lines
              (cl-incf i)
              (cl-incf j))

          ;; Lines don't match - they're part of a 'diff' chunk
          (progn
            ;; If we were in a 'same' chunk, finalize it
            (when (eq current-chunk-type 'same)
              ;; Reverse the lists to restore order
              (push (list 'same (reverse current-orig-chunk) (reverse current-new-chunk)) chunks)
              (setq current-orig-chunk nil
                    current-new-chunk nil))

            ;; Add to or start a 'diff' chunk
            (setq current-chunk-type 'diff)

            ;; The heuristic below finds the 'best' way to advance through the diff
            ;; Look ahead to find matching lines
            (let ((match-distance-i nil)
                  (match-distance-j nil))

              ;; Look ahead in orig-lines to find a match with current new-line
              (when (and new-line (< i orig-len))
                (let ((k 0))
                  (while (and (< (+ i k) orig-len)
                              (< k 10) ; Limit how far we look ahead
                              (not match-distance-i))
                    (when (string= (nth (+ i k) orig-lines) new-line)
                      (setq match-distance-i k))
                    (cl-incf k))))

              ;; Look ahead in new-lines to find a match with current orig-line
              (when (and orig-line (< j new-len))
                (let ((k 0))
                  (while (and (< (+ j k) new-len)
                              (< k 10) ; Limit how far we look ahead
                              (not match-distance-j))
                    (when (string= (nth (+ j k) new-lines) orig-line)
                      (setq match-distance-j k))
                    (cl-incf k))))

              ;; Decide which way to advance based on match distances
              (cond
               ;; If we're at the end of either list, consume the other
               ((>= i orig-len)
                (when new-line
                  (push new-line current-new-chunk)
                  (cl-incf j)))
               ((>= j new-len)
                (when orig-line
                  (push orig-line current-orig-chunk)
                  (cl-incf i)))

               ;; If we found a match in both directions, take the shortest path
               ((and match-distance-i match-distance-j)
                (if (< match-distance-i match-distance-j)
                    (progn
                      (push orig-line current-orig-chunk)
                      (cl-incf i))
                  (push new-line current-new-chunk)
                  (cl-incf j)))

               ;; If we found a match in just one direction, go that way
               (match-distance-i
                (push orig-line current-orig-chunk)
                (cl-incf i))
               (match-distance-j
                (push new-line current-new-chunk)
                (cl-incf j))

               ;; No match found, just advance both
               (t
                (when orig-line
                  (push orig-line current-orig-chunk))
                (when new-line
                  (push new-line current-new-chunk))
                (cl-incf i)
                (cl-incf j)))))))

      ;; End of main loop
      )

    ;; Finalize the last chunk
    (when current-chunk-type
      (if (eq current-chunk-type 'same)
          (push (list 'same (reverse current-orig-chunk) (reverse current-new-chunk)) chunks)
        (push (list 'diff (reverse current-orig-chunk) (reverse current-new-chunk)) chunks)))

    ;; Return the chunks in correct order
    (reverse chunks)))

(provide 'relysium-utils)
;;; relysium-utils.el ends here
