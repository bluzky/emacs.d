;;; relysium-suggest.el --- Suggestions functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions related to getting and applying AI-generated
;; code suggestions to the current buffer using the relysium package.

;;; Code:

(require 'gptel)
(require 'json)
(require 'relysium-utils)
(require 'relysium-buffer-manager)
(require 'relysium-common)

(setq relysium-suggest-prompt "Act as an expert software developer.
Always use best practices when coding.
Respect and use existing conventions, libraries, etc that are already present in the code base.

Make sure code comments are in English when generating them.

Your task is to suggest code modifications at the cursor position. Follow these instructions meticulously:
  1. Carefully analyze the original code, paying close attention to its structure and the cursor position.
  2. Return a JSON array of suggestions wrapped in <suggestions> tags
    Each JSON array item is a map with 3 fields:
      start_row: The starting row of the code snippet you want to replace, start from 1, inclusive
      end_row: The ending row of the code snippet you want to replace, start from 1, exclusive
      content: The suggested code you want to replace the original code with, the content MUST BE encoded.
  3. JSON must be wrapped with <suggestions></suggestions> tags, for example:
    <suggestions>
      [
        {
          \"start_row\": 1,
          \"end_row\": 1,
          \"content\": \"added code here\"
        },
        {
          \"start_row\": 3,
          \"end_row\": 9,
          \"content\": \"Your suggested modifies code here\"
        }
      ]
    </suggestions>

CRITICAL INSTRUCTIONS ABOUT LINE NUMBERS:
- Line numbers in your response MUST start from 1 (not 0)
- The start_row is the first line to be replaced (inclusive)
- The end_row is one past the last line to be replaced (exclusive)
- Always double-check your line numbers before responding
- FOR INSERT ONLY USE start_row = end_row: insert at line 10, use start_row=10, end_row=10
- For modify existing source code: if replacing lines 10-12, use start_row=10, end_row=13

When determining line numbers:
- Count from the very beginning of the file
- Include blank lines in your count
- Verify line numbers by counting explicitly before responding

Guidelines:
  1. Make sure you have maintained the user's existing whitespace and indentation. This is REALLY IMPORTANT!
  2. Each code snippet returned in the list must not overlap, and together they complete the same task.
  3. The more code snippets returned at once, the better.
  4. If there is incomplete code on the current line where the cursor is located, prioritize completing the code on the current line.
  5. DO NOT include three backticks: {%raw%}```{%endraw%} in your suggestion. Treat the suggested code AS IS.
  6. Each element in the returned list is a COMPLETE code snippet.
  7. MUST be a valid JSON format. DO NOT be lazy!
  8. Only return the new code to be inserted. DO NOT be lazy!
  9. Please strictly check the code around the position and ensure that the complete code after insertion is correct. DO NOT be lazy!
  10. Do not return the entire file content or any surrounding code.
  11. Do not include any explanations, comments, or line numbers in your response.
  12. Ensure the suggested code fits seamlessly with the existing code structure and indentation.
  13. If there are no recommended modifications, return an empty list.
  14. Remember to ONLY RETURN the suggested code snippet, without any additional formatting or explanation.
  15. The returned code must satisfy the context, especially the context where the current cursor is located.
  16. Each line in the returned code snippet is complete code; do not include incomplete code.
  17. DO NOT return line number in the suggestion.
  18. Combine multiple siblings suggestions into one if possible.")


;;;###autoload
(defun relysium-suggest (additional-prompt)
  "Send whole buffer to LLM for code improvement suggestions.
The LLM will return suggestions in JSON format that will be applied to the buffer.
ADDITIONAL-PROMPT allows users to provide specific instructions."
  (interactive "sAdditional instructions (optional): ")

  (let* ((code-buffer (current-buffer))
         (chat-buffer (relysium-buffer-get-chat-buffer))
         (code-content (buffer-substring-no-properties (point-min) (point-max)))
         (current-cursor-line (line-number-at-pos (point)))
         (file-type (symbol-name major-mode))
         (lang-name (replace-regexp-in-string "-mode$\\|-ts-mode$" "" file-type))
         ;; Include the additional prompt if provided
         (instruction-text (if (string-empty-p additional-prompt)
                               "Please analyze the code and suggest improvements."
                             (concat "Please analyze the code and suggest improvements with these specific instructions: "
                                     additional-prompt)))
         ;; Annotated code with line numbers for better reference
         (annotated-code (relysium--add-line-numbers code-content))
         ;; Create the full prompt with entire buffer content
         (full-prompt (format "File type: %s\nCursor line: %d\n\nSource code:\n```%s\n%s\n```\n\n%s"
                              lang-name
                              current-cursor-line
                              lang-name
                              annotated-code
                              instruction-text)))

    (setq relysium--last-code-buffer code-buffer)
    (relysium-buffer-append-user-message full-prompt)

    (gptel--update-status " Waiting..." 'warning)
    (message "Requesting code suggestions from %s..." (gptel-backend-name gptel-backend))

    (gptel-request full-prompt
      :system relysium-suggest-prompt
      :buffer chat-buffer
      :callback (apply-partially #'relysium-handle-suggestions code-buffer))))

(defun relysium-handle-suggestions (code-buffer response info)
  "Handle the JSON suggestions RESPONSE from gptel.
The suggestions will be applied to CODE-BUFFER.
INFO is passed from the `gptel-request' function."
  (when response
    ;; Log the full response if debug mode is enabled
    (relysium-debug-log "LLM Suggestion Response:\n%s" response)

    ;; Add response to the chat buffer
    (relysium-buffer-append-assistant-message response)

    ;; Extract and process suggestions
    (let ((suggestions (relysium-extract-suggestions response)))
      ;; Log the extracted suggestions if debug mode is enabled
      (relysium-debug-log "Extracted suggestions: %s"
                          (if suggestions
                              (format "%s" suggestions)
                            "None found"))

      (if suggestions
          (with-current-buffer code-buffer
            ;; Mark undo boundary before making changes
            (undo-boundary)

            ;; Convert suggestions to format expected by relysium-apply-code-changes
            (let ((changes
                   (mapcar (lambda (suggestion)
                             (list :start (plist-get suggestion :start_row)
                                   :end (plist-get suggestion :end_row)
                                   :code (plist-get suggestion :content)))
                           suggestions)))

              ;; Apply the changes
              (relysium-apply-code-changes code-buffer changes)

              ;; Activate smerge mode and show transient menu
              (smerge-mode 1)
              (goto-char (point-min))
              (ignore-errors (smerge-next))
              (relysium-transient-menu)
              (message "Applied %d suggestion(s). Review with the merge menu." (length suggestions))))
        (message "No applicable suggestions found.")))

    ;; Update status

    (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
      (with-current-buffer chat-buffer
        (gptel--sanitize-model)
        (gptel--update-status " Ready" 'success)))
    ))

(defun relysium-extract-suggestions (response)
  "Extract JSON suggestions from LLM RESPONSE.
Returns a list of suggestion plists or nil if no suggestions found."
  (let ((start (string-match "<suggestions>" response))
        (end (string-match "</suggestions>" response)))
    (when (and start end (< start end))
      (let ((json-str (substring response
                                 (+ start (length "<suggestions>"))
                                 end)))
        (condition-case err
            (let ((json-array-type 'list)
                  (json-object-type 'plist))
              (json-read-from-string json-str))
          (error
           (message "Error parsing JSON suggestions: %s" (error-message-string err))
           nil))))))


(defun relysium--add-line-numbers (code-content)
  "Add line numbers to CODE-CONTENT for clearer reference."
  (let ((lines (split-string code-content "\n"))
        (result ""))
    (cl-loop for line in lines
             for i from 1
             do (setq result (concat result (format "%3d: %s\n" i line))))
    result))

(provide 'relysium-suggest)
;;; relysium-suggest.el ends here
