;;; relysium-ask.el --- Ask functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains the functions related to asking questions about code
;; without making changes, using the relysium LLM integration.

;;; Code:

(require 'gptel)
(require 'relysium-utils)
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
    ;; Region is selected, proceed with LLM query
    (unless (buffer-live-p relysium--chat-buffer)
      (setq relysium--chat-buffer (gptel "*elysium*")))

    (let* ((chat-buffer relysium--chat-buffer)
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
      (with-current-buffer chat-buffer
        (goto-char (point-max))
        (insert "\n\n### USER:\n")
        (insert full-prompt)
        (insert "\n"))

      ;; Show the chat window
      (relysium-setup-windows)

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
  (when response
    (with-current-buffer relysium--chat-buffer
      (goto-char (point-max))
      (insert "\n\n### ASSISTANT:\n")
      (insert response)
      (insert "\n")

      ;; Update status
      (gptel--sanitize-model)
      (gptel--update-status " Ready" 'success))
    (message "LLM response received")))



(provide 'relysium-ask)
;;; relysium-ask.el ends here
