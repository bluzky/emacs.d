;;; relysium-ask.el --- Ask functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions related to asking questions about code
;; without making changes, using the relysium LLM integration.

;;; Code:

(require 'gptel)
(require 'relysium-utils)
(require 'relysium-buffer-manager)
(require 'relysium-common)

(defvar relysium-ask-prompt
  "You are an expert programmer and coding assistant.
Your task is to provide helpful, accurate, and relevant information about the code provided.
Be concise yet thorough in your explanations.
Your answers should be clear, informative, and directly related to the code provided.
Your answer should be short and focus ONLY on the questions asked.")

;;;###autoload
(defun relysium-ask (user-prompt)
  "Ask a question about the selected code region"
  (interactive "sAsk about code: ")
  (if (not (use-region-p))
      (message "Please select a region of code first")

    (let* ((chat-buffer (relysium-buffer-get-chat-buffer))
           (selected-code (buffer-substring-no-properties (region-beginning) (region-end)))
           (file-type (symbol-name major-mode))
           (lang-name (replace-regexp-in-string "-mode$\\|-ts-mode$" "" file-type))
           ;; Create the full prompt with code context
           (full-prompt (format "Code (%s):\n```%s\n%s\n```\n\nQuestion: %s"
                                lang-name
                                lang-name
                                selected-code
                                user-prompt)))

      ;; Update chat buffer with the query
      (relysium-buffer-append-user-message full-prompt)

      ;; Show the chat window, and focus back to the code buffer
      (relysium-buffer-setup-windows t)

      ;; Update status and send request
      (gptel--update-status " Waiting..." 'warning)
      (message "Asking LLM about selected code...")
      (deactivate-mark)

      (gptel-request full-prompt
        :system relysium-ask-prompt
        :buffer chat-buffer
        :callback 'relysium-ask-callback))))

(defun relysium-ask-callback (response _info)
  "Handle the RESPONSE from LLM for relysium-ask.
_INFO is unused but required by the gptel callback interface."
  (relysium-debug-log "LLM Response:\n%s" response)
  (when response
    ;; Append the response to the chat buffer
    (relysium-buffer-append-assistant-message response)

    ;; Update status in the chat buffer, not in the current buffer
    (let ((chat-buffer (relysium-buffer-get-chat-buffer)))
      (with-current-buffer chat-buffer
        (gptel--sanitize-model)
        (gptel--update-status " Ready" 'success)))

    (message "LLM response received")))



(provide 'relysium-ask)
;;; relysium-ask.el ends here
