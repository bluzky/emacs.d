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
(require 'gptel-tools)

(use-package gptel
  :init
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  :hook
  (gptel-mode . (lambda ()
                  (display-line-numbers-mode 0)
                  (if (fboundp 'markdown-toggle-markup-hiding)
                      (markdown-toggle-markup-hiding)
                    (message "not markdown mode"))
                  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist) "--- \n### 🎙️ USER: ")
                  (setf (alist-get 'markdown-mode gptel-response-prefix-alist) "### 🤖 ASSISTANT:\n")))
  :bind

  (("C-c g" . gptel)
   :map gptel-mode-map
   ("C-<return>" . gptel-send))
  :custom
  (gptel-temperature 0.7)
  (gptel-cache t)
  :config
  (setq gptel-use-tools t)

  (add-to-list 'gptel-tools (use-tool-read-file))
  (add-to-list 'gptel-tools (use-tool-list-directory))
  (add-to-list 'gptel-tools (use-tool-make-directory))
  (add-to-list 'gptel-tools (use-tool-create-file))
  (add-to-list 'gptel-tools (use-tool-run-command))
  (add-to-list 'gptel-tools (use-tool-read-url))

  (add-to-list 'gptel-directives '(default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.

## Tools usage guidelines:

- DON'T be so aggressive in using tools, only used when necessary, as many tasks can be better completed without tools.
- Before using tools, explain shortly what you are going to do and why.
- After using tools, explain what you have done and what failed. And list the files locations if there are files/directories changes."))


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


  (gptel-make-anthropic "Claude"          ;Any name you want
    :stream t                             ;Streaming responses
    :key ai-anthropic-api-key
    :models '(claude-3-5-sonnet-20241022 claude-3-7-sonnet-20250219))

  ;; Groq offers an OpenAI compatible API
  (setq gptel-model  'moonshotai/kimi-k2-instruct
        gptel-backend
        (gptel-make-openai "Groq"
          :host "api.groq.com"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key groq-api-key
          :models '(moonshotai/kimi-k2-instruct
                    qwen-2.5-coder-32b
                    deepseek-r1-distill-llama-70b
                    deepseek-r1-distill-qwen-32b
                    llama-3.3-70b-versatile))
        )

  ;; (setq gptel-model 'claude-3-5-sonnet-20241022
  ;;       gpt-backend (gptel-make-openai "MistralCode"  ;Any name you want
  ;;                     :host "api.mistral.ai"
  ;;                     :endpoint "/v1/chat/completions"
  ;;                     :protocol "https"
  ;;                     :key mistral-api-key               ;can be a function that returns the key
  ;;                     :models '(codestral-latest mistral-large-latest))
  ;;       )
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
;;                     :branch "main"
;;                     :files ("*.el"))
;;   :hook (prog-mode . relysium-prog-mode))


(provide 'init-ai)
;;; init-ai.el ends here
