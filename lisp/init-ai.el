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
;; (require 'gptel-tools)

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
  ;; (setq gptel-use-tools t)

  ;; (add-to-list 'gptel-tools (use-tool-read-file))
  ;; (add-to-list 'gptel-tools (use-tool-list-directory))
  ;; (add-to-list 'gptel-tools (use-tool-make-directory))
  ;; (add-to-list 'gptel-tools (use-tool-create-file))
  ;; (add-to-list 'gptel-tools (use-tool-run-command))
  ;; (add-to-list 'gptel-tools (use-tool-read-url))

  (add-to-list 'gptel-directives '(default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.

## Tools usage guidelines:

- DON'T be so aggressive in using tools, only used when necessary, as many tasks can be better completed without tools.
- Before using tools, explain shortly what you are going to do and why.
- After using tools, explain what you have done and what failed. And list the files locations if there are files/directories changes."))


  ;; Groq offers an OpenAI compatible API
  (setq gptel-model  'hf:MiniMaxAI/MiniMax-M2.1
        gptel-backend
        (gptel-make-openai "Synthetic"
          :host "api.synthetic.new"
          :endpoint "/openai/v1/chat/completions"
          :stream t
          :key synthetic-api-key
          :models '(hf:MiniMaxAI/MiniMax-M2.1
                    hf:zai-org/GLM-4.7
                    hf:moonshotai/Kimi-K2.5))
        )
  )

(use-package ollama-buddy
  :ensure (:host github :repo "captainflasmr/ollama-buddy")
  :bind
  ("C-c o" . ollama-buddy-menu)
  ("C-c O" . ollama-buddy-transient-menu-wrapper)
  :custom
  (ollama-buddy-openai-api-endpoint "http://api.synthetic.new/openai/v1/chat/completions")
  (ollama-buddy-openai-default-model "hf:MiniMaxAI/MiniMax-M2.1")
  (ollama-buddy-openai-api-key synthetic-api-key)
  :config
  (require 'ollama-buddy-copilot nil t))

(use-package relysium
  :ensure (:host github :repo "bluzky/relysium")
  :hook (prog-mode . relysium-prog-mode)
  :commands (relysium-ask
             relysium-edit-dwim
             relysium-buffer-add-context
             relysium-buffer-clear
             relysium-debug-log
             relysium-toggle-debug-mode
             relysium-generate-from-comments
             relysium-suggest
             relysium-buffer-toggle-window)
  :config
  ;; Add any additional relysium configuration here
  )

;; (use-package shell-maker
;;   :ensure (:host github :repo "xenodium/shell-maker"))

;; (use-package acp
;;   :ensure (:host github :repo "xenodium/acp.el"))

;; (use-package agent-shell
;;   :ensure (:host github :repo "xenodium/agent-shell")
;;   :commands (agent-shell-anthropic-start-claude-code)
;;   :bind (:map prog-mode-map
;;               ("C-c C-c" . agent-shell-anthropic-start-claude-code)))

;; ;; Minuet - LLM-powered code completion with FIM support
;; (use-package minuet
;;   :bind (("TAB" . minuet-accept-suggestion)
;;          ("M-]" . minuet-next-suggestion)
;;          ("M-[" . minuet-previous-suggestion))
;;   :hook (prog-mode . minuet-auto-suggestion-mode)
;;   :init
;;   ;; Set provider to OpenAI FIM compatible
;;   (setq minuet-provider 'openai-fim-compatible)

;;   ;; Context window size (adjust based on your needs)
;;   (setq minuet-context-window 8192)

;;   ;; Request timeout in seconds
;;   (setq minuet-request-timeout 3)

;;   ;; Number of completion suggestions
;;   (setq minuet-n-completions 3)

;;   :config
;;   ;; Configure OpenAI FIM compatible options
;;   ;; Example for DeepSeek (default):
;;   (plist-put minuet-openai-fim-compatible-options :end-point "https://api.deepseek.com/beta/completions")
;;   (plist-put minuet-openai-fim-compatible-options :api-key (lambda() ai-deepseek-api-key))
;;   (plist-put minuet-openai-fim-compatible-options :model "deepseek-chat")

;;   ;; Example for Groq (uncomment to use):
;;   ;; (plist-put minuet-openai-fim-compatible-options :end-point "https://api.groq.com/openai/v1/completions")
;;   ;; (plist-put minuet-openai-fim-compatible-options :api-key "GROQ_API_KEY")
;;   ;; (plist-put minuet-openai-fim-compatible-options :model "llama-3.3-70b-versatile")

;;   ;; Example for local Ollama (uncomment to use):
;;   ;; (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
;;   ;; (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
;;   ;; (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")

;;   ;; Set optional parameters
;;   (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 256))

(provide 'init-ai)
;;; init-ai.el ends here
