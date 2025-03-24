;;; init-ai.el --- Configuration for AI-related packages in Emacs

;;; Commentary:
;; This file contains the configuration for integrating various AI-related packages into Emacs.
;; The setup includes enabling AI modes in `org-mode`, configuring interactions with different AI models,
;; managing AI-generated content, and facilitating code merging.

;;; Table of Contents:
;; 1. Chat GPT Configuration
;; 2. GPTel Configuration
;; 3. Elysium Configuration
;; 4. Smerge-Mode Configuration

;;; Code:

(require 'variables)

(use-package gptel
  :init
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  :bind
  (("C-c g" . gptel)
   :map gptel-mode-map
   ("C-<return>" . gptel-send))
  :config
  (gptel-make-gemini "Gemini"
    :key ai-gemini-api-key
    :stream t)

  (gptel-make-openai "Github Models" ;Any name you want
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions?api-version=2024-05-01-preview"
    :stream t
    :key ai-github-api-key             ;can be a function that returns the key
    :models '(gpt-4o
              o1))

  ;; Groq offers an OpenAI compatible API

  (setq gptel-model  'gpt-4o
        gptel-backend
        (gptel-make-openai "Groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key groq-api-key
          :models '(qwen-2.5-coder-32b
                    llama-3.3-70b-versatile))
        )

  ;; (setq gptel-model   'deepseek-chat
  ;;       gptel-backend
  ;;       (gptel-make-openai "DeepSeek"     ;Any name you want
  ;;         :host "api.deepseek.com"
  ;;         :endpoint "/chat/completions"
  ;;         :stream t
  ;;         :key ai-deepseek-api-key             ;can be a function that returns the key
  ;;         :models '(deepseek-chat deepseek-coder)))
  )

(require 'elysium)

;; Add key binding for C-<return> (Ctrl+Enter) to trigger elysium-query in prog-mode
(defun elysium-query-dwim ()
  "Query elysium with the region if active, otherwise prompt for a query."
  (interactive)
  (if (use-region-p)
      (call-interactively 'elysium-query)
    (let ((current-prefix-arg '(4))) ; Simulate C-u prefix to prompt for region
      (call-interactively 'elysium-query))))

;; Define a keymap for programming modes
(defvar elysium-prog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") 'elysium-query-dwim)
    map)
  "Keymap for elysium in programming modes.")

;; Set up a minor mode to attach the keymap
(define-minor-mode elysium-prog-mode
  "Minor mode for elysium in programming modes."
  :lighter " Elysium"
  :keymap elysium-prog-mode-map)

;; Enable the minor mode in all programming modes
(add-hook 'prog-mode-hook 'elysium-prog-mode)

(provide 'init-ai)
;;; init-ai.el ends here
