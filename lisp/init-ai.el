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

  (gptel-make-openai "Github Models"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions?api-version=2024-05-01-preview"
    :stream t
    :key ai-github-api-key
    :models '(gpt-4o
              o1))

  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key ai-deepseek-api-key
    :models '(deepseek-chat deepseek-coder))

  ;; Groq offers an OpenAI compatible API
  (setq gptel-model  'qwen-2.5-coder-32b
        gptel-backend
        (gptel-make-openai "Groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key groq-api-key
          :models '(qwen-2.5-coder-32b
                    deepseek-r1-distill-llama-70b
                    deepseek-r1-distill-qwen-32b
                    llama-3.3-70b-versatile))
        )
  )

(defconst relysium-directory
  (expand-file-name "relysium"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the relysium component files.")

;; Add the directory to load path
(add-to-list 'load-path relysium-directory)

(require 'relysium)
(add-hook 'prog-mode-hook 'relysium-prog-mode)

;; (use-package relysium
;;   :quelpa (relysium :fetcher github
;;                     :repo "bluzky/relysium"
;;                     :branch "main")
;;   :hook (prog-mode . relysium-prog-mode))

(provide 'init-ai)
;;; init-ai.el ends here
