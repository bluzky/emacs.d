;;; elysium.el --- Automatically apply LLM-created code-suggestions -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Lance Bergeron

;; Author: Lance Bergeron <bergeron.lance6@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; URL: https://github.com/lanceberge/elysium/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package extends on gptel.el.  It uses that package to generate code
;; suggestions based on the user's request.  Those code suggestions will then
;; automatically be applied to the buffer in the format of a git merge.
;; After applying changes, it enters smerge-mode and provides a transient menu
;; to approve, reject, or retry with a new query.

;;; Code:
(require 'gptel)
(require 'smerge-mode)
(require 'transient)

(defgroup elysium nil
  "Apply code changes using gptel."
  :group 'hypermedia)

(defcustom elysium-apply-changes-hook nil
  "Hook run after code changes have been applied on a buffer."
  :group 'elysium
  :type 'hook)

(defvar elysium--chat-buffer nil
  "Buffer used for LLM interaction.")

(defvar elysium--last-query nil
  "The last query sent to the LLM.")

(defvar elysium--last-code-buffer nil
  "The buffer that was last modified by Elysium.")

(defvar elysium-base-prompt
  (concat
   ;; The prompt is originally from avante.nvim:
   ;; https://github.com/yetone/avante.nvim/blob/main/lua/avante/llm.lua
   "Your primary task is to suggest code modifications with precise line number ranges. Follow these instructions meticulously:\n"

   "1. Carefully analyze the original code, paying close attention to its structure and line numbers. Line numbers start from 1 and include ALL lines, even empty ones.\n"

   "2. When suggesting modifications:\n"
   "a. Use the language in the question to reply. If there are non-English parts in the question, use the language of those parts.\n"
   "c. If an image is provided, make sure to use the image in conjunction with the code snippet.\n"
   "d. Provide the exact code snippet to be replaced using this format:\n"

   "Replace lines: {{start_line}}-{{end_line}}\n"
   ;; We don't need the language here but if we don't specify it then some LLMs might provide it and some might not.
   ;; So it's better to mandate it and filter it later
   "```{{language}}\n"
   "{{suggested_code}}\n"
   "```\n"
   "{{Explanation of the changes}}"

   "3. Crucial guidelines for suggested code snippets:\n"
   "- Only apply the change(s) suggested by the most recent assistant message (before your generation).\n"
   "- Do not make any unrelated changes to the code.\n"
   "- Produce a valid full rewrite of the entire original file without skipping any lines. Do not be lazy!\n"
   "- Do not arbitrarily delete pre-existing comments/empty Lines.\n"
   "- Do not omit large parts of the original file for no reason.\n"
   "- Do not omit any needed changes from the requisite messages/code blocks.\n"
   "- If there is a clicked code block, bias towards just applying that (and applying other changes implied).\n"
   "- Please keep your suggested code changes minimal, and do not include irrelevant lines in the code snippet.\n"
   "- Maintain the SAME indentation in the returned code as in the source code\n"

   "4. Crucial guidelines for line numbers:\n"
   "- The content regarding line numbers MUST strictly follow the format Replace lines: {{start_line}}-{{end_line}}. Do not be lazy!\n"
   "- The range {{start_line}}-{{end_line}} is INCLUSIVE. Both start_line and end_line are included in the replacement.\n"
   "- Count EVERY line, including empty lines and comments lines, comments. Do not be lazy!\n"
   "- For single-line changes, use the same number for start and end lines.\n"
   "- For multi-line changes, ensure the range covers ALL affected lines, from the very first to the very last.\n"
   "- Double-check that your line numbers align perfectly with the original code structure.\n"

   "5. Final check:\n"
   "- Review all suggestions, ensuring each line number is correct, especially the start_line and end_line.\n"
   "- Confirm that no unrelated code is accidentally modified or deleted.\n"
   "- Verify that the start_line and end_line correctly include all intended lines for replacement.\n"
   "- Perform a final alignment check to ensure your line numbers haven't shifted, especially the start_line.\n"
   "- Double-check that your line numbers align perfectly with the original code structure.\n"
   "- Do not show the full content after these modifications.\n"

   "Remember: Accurate line numbers are CRITICAL. The range start_line to end_line must include ALL lines to be replaced, from the very first to the very last. Double-check every range before finalizing your response, paying special attention to the start_line to ensure it hasn't shifted down. Ensure that your line numbers perfectly match the original code structure without any overall shift.\n"))

;;;###autoload
(defun elysium-query (user-query)
  "Send USER-QUERY to elysium from the current buffer."
  (interactive (list (read-string "User Query: ")))
  (unless (buffer-live-p elysium--chat-buffer)
    (setq elysium--chat-buffer (gptel "*elysium*")))

  (let* ((code-buffer (current-buffer))
         (chat-buffer elysium--chat-buffer)
         (using-region (use-region-p))
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
         (start-line (line-number-at-pos start-line-pos))
         (end-line (line-number-at-pos end-line-pos))
         (selected-code (buffer-substring-no-properties start-line-pos end-line-pos))
         (file-type (symbol-name major-mode))
         ;; Include indentation info in the query
         (full-query (format "\n\nFile type: %s\nLine range: %d-%d\n%s\n\nCode:\n%s\n\n%s"
                             file-type
                             start-line
                             end-line
                             ""
                             selected-code
                             user-query)))

    (setq elysium--last-query user-query)
    (setq elysium--last-code-buffer code-buffer)

    ;; Store region info for later use
    (setq-local elysium--using-region using-region)
    (setq-local elysium--region-start-line start-line)
    (setq-local elysium--region-end-line end-line)

    (gptel--update-status " Waiting..." 'warning)
    (message "Querying %s for lines %d-%d..."
             (gptel-backend-name gptel-backend)
             start-line end-line)
    (deactivate-mark)
    (with-current-buffer chat-buffer
      (goto-char (point-max))
      (insert user-query)
      (insert "\n\n"))

    (gptel-request full-query
      :system elysium-base-prompt
      :buffer chat-buffer
      :callback (apply-partially #'elysium-handle-response code-buffer))))

(defun elysium-handle-response (code-buffer response info)
  "Handle the RESPONSE from gptel.
The changes will be applied to CODE-BUFFER in a git merge format.
INFO is passed into this function from the `gptel-request' function."
  (when response
    (let* ((extracted-data (elysium-extract-changes response))
           (changes (plist-get extracted-data :changes))
           (using-region (buffer-local-value 'elysium--using-region code-buffer)))

      (when changes
        ;; Apply changes
        (with-current-buffer code-buffer
          ;; Adjust changes if we're working with a region
          (when using-region
            (setq changes (elysium--adjust-changes-for-region changes)))

          (elysium-apply-code-changes code-buffer changes)

          ;; Activate smerge mode and show transient menu
          (smerge-mode 1)
          (goto-char (point-min))
          (ignore-errors (smerge-next))
          (elysium-transient-menu)))

      ;; Update status
      (with-current-buffer elysium--chat-buffer
        (gptel--sanitize-model)
        (gptel--update-status " Ready" 'success)))))

(defun elysium--adjust-changes-for-region (changes)
  "Adjust CHANGES line numbers based on the selected region.
Makes sure changes are properly aligned with the actual lines in the buffer."
  (when (and (boundp 'elysium--region-start-line)
             (local-variable-p 'elysium--region-start-line)
             elysium--region-start-line)
    ;; We need to offset all line numbers by the start line of the region
    (let ((offset (1- elysium--region-start-line)))
      (mapcar (lambda (change)
                (list :start (+ (plist-get change :start) offset)
                      :end (+ (plist-get change :end) offset)
                      :code (plist-get change :code)))
              changes)))
  changes)

(defun elysium-extract-changes (response)
  "Extract the code-changes and explanations from RESPONSE.
Explanations will be of the format:
{Initial explanation}

1st Code Change:
{Code Change}

2nd Code Change:
{Code Change}"
  (let ((changes '())
        (explanations '())
        (start 0)
        (change-count 0)
        (code-block-regex
         "Replace [Ll]ines:? \\([0-9]+\\)-\\([0-9]+\\)\n```\\(?:[[:alpha:]-]+\\)?\n\\(\\(?:.\\|\n\\)*?\\)```"))
    (while (string-match code-block-regex response start)
      (let ((change-start (string-to-number (match-string 1 response)))
            (change-end (string-to-number (match-string 2 response)))
            (code (match-string 3 response))
            (explanation-text (substring response start (match-beginning 0))))
        ;; the initial explanation won't be preceded by nth Code Change
        (when (not (string-empty-p explanation-text))
          (push (if (= 0 change-count)
                    explanation-text  ; For the first explanation, just use the text as is
                  (format "%s Code Change:\n%s"
                          (elysium--ordinal change-count)
                          explanation-text))
                explanations)
          (setq change-count (1+ change-count)))
        (push (list :start change-start
                    :end change-end
                    :code code)
              changes)

        ;; Update start index in the response string
        (setq start (match-end 0))))

    ;; Add any remaining text as the last explanation
    (let ((remaining-text (substring response start)))
      (when (not (string-empty-p remaining-text))
        (push (if (= 0 change-count)
                  remaining-text
                (format "%s Code Change:\n%s"
                        (elysium--ordinal change-count)
                        remaining-text))
              explanations)))
    (list :explanations (nreverse explanations)
          :changes (nreverse changes))))

(defun elysium-apply-code-changes (buffer code-changes)
  "Apply CODE-CHANGES to BUFFER in a git merge format.
Uses simple conflict markers to highlight the differences between
original and suggested code."
  (with-current-buffer buffer
    (save-excursion
      (let ((offset 0))
        (dolist (change code-changes)
          (let* ((start (plist-get change :start))
                 (end (plist-get change :end))
                 (new-code (string-trim-right (plist-get change :code)))
                 (orig-code-start (progn
                                    (goto-char (point-min))
                                    (forward-line (1- (+ start offset)))
                                    (point)))
                 (orig-code-end (progn
                                  (goto-char (point-min))
                                  (forward-line (1- (+ end offset 1)))
                                  (point)))
                 (orig-code (buffer-substring-no-properties orig-code-start orig-code-end)))

            ;; Delete the original code block
            (delete-region orig-code-start orig-code-end)

            ;; Insert the conflict markers with original and new code
            (goto-char orig-code-start)
            (insert (concat "<<<<<<< HEAD\n"
                           orig-code
                           "=======\n"
                           new-code
                           "\n>>>>>>> " (gptel-backend-name gptel-backend) "\n"))

            ;; Update offset - calculate how many lines we added/removed
            (let* ((orig-lines (split-string orig-code "\n" t))
                   (new-lines (split-string (concat new-code "\n") "\n" t))
                   (marker-lines 3) ; <<<<<<< HEAD, =======, and >>>>>>>
                   (original-line-count (- end start 1))
                   (new-line-count (+ (length new-lines) marker-lines))
                   (line-diff (- new-line-count original-line-count)))
              (setq offset (+ offset line-diff))))))))

(defun elysium-clear-buffer ()
  "Clear the elysium buffer."
  (interactive)
  (with-current-buffer elysium--chat-buffer
    (erase-buffer)
    (insert (gptel-prompt-prefix-string))))

(defun elysium-add-context (content)
  "Add CONTENT as context to the elysium chat buffer."
  (interactive
   (list (if (region-active-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (buffer-substring-no-properties (point-min) (point-max)))))
  (let ((code-buffer-language
         (string-trim-right
          (string-trim-right (symbol-name major-mode) "-ts-mode$") "-mode$")))
    (with-current-buffer elysium--chat-buffer
      (goto-char (point-max))
      (insert "\n")
      (insert (format "```%s\n%s\n```" code-buffer-language content)))))

(defun elysium-keep-all-suggested-changes ()
  "Keep all of the LLM suggestions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-lower))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-lower))
    (smerge-mode -1)
    (message "All suggested changes applied")))

(defun elysium-discard-all-suggested-changes ()
  "Discard all of the LLM suggestions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-upper))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-upper))
    (smerge-mode -1)
    (message "All suggested changes discarded")))

(defun elysium-navigate-next-change ()
  "Navigate to the next change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-next))
      (message "Navigated to next change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (elysium-transient-menu))

(defun elysium-navigate-prev-change ()
  "Navigate to the previous change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-prev))
      (message "Navigated to previous change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (elysium-transient-menu))

(defun elysium-keep-current-change ()
  "Keep the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-lower)
  (if (ignore-errors (not (smerge-next)))
      (progn
        (message "All changes reviewed - no more conflicts")
        (smerge-mode -1))
    (message "Applied change - move to next")
    ;; Keep the transient menu active if there are more changes
    (elysium-transient-menu)))

(defun elysium-reject-current-change ()
  "Reject the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-upper)
  (if (ignore-errors (not (smerge-next)))
      (progn
        (message "All changes reviewed - no more conflicts")
        (smerge-mode -1))
    (message "Rejected change - move to next")
    ;; Keep the transient menu active if there are more changes
    (elysium-transient-menu)))

(defun elysium-retry-query ()
  "Retry the last query with modifications."
  (interactive)
  (let ((new-query (read-string "Modify query: " elysium--last-query)))
    (when new-query
      (with-current-buffer elysium--last-code-buffer
        (elysium-discard-all-suggested-changes)
        (elysium-query new-query)))))

(defun elysium--ordinal (n)
  "Convert integer N to its ordinal string representation."
  (let ((suffixes '("th" "st" "nd" "rd" "th" "th" "th" "th" "th" "th")))
    (if (and (> n 10) (< n 14))
        (concat (number-to-string n) "th")
      (concat (number-to-string n)
              (nth (mod n 10) suffixes)))))

;; Define a transient menu for Elysium with compact layout
(transient-define-prefix elysium-transient-menu ()
  "Elysium actions menu."
  [["Navigate"
    ("n" "Next" elysium-navigate-next-change)
    ("p" "Prev" elysium-navigate-prev-change)]
   ["Current"
    ("y" "Accept" elysium-keep-current-change)
    ("N" "Reject" elysium-reject-current-change)]
   ["All"
    ("a" "Accept all" elysium-keep-all-suggested-changes)
    ("d" "Discard all" elysium-discard-all-suggested-changes)]
   ["Query"
    ("r" "Retry" elysium-retry-query)
    ("q" "New" elysium-query)]])


(provide 'elysium)

;;; elysium.el ends here
