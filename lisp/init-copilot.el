;;; init-ai.el --- Configuration for AI-related packages in Emacs

;;; Commentary:

;;; Code:

(require 'variables)

;; (use-package copilot
;;   :quelpa (copilot :fetcher github
;;                    :repo "zerolfx/copilot.el"
;;                    :branch "main"
;;                    :files ("dist" "*.el"))
;;   :bind (:map copilot-completion-map
;;               ("C-TAB" . copilot-accept-completion-by-word)
;;               ("C-<tab>" . copilot-accept-completion-by-word)
;;               :map copilot-mode-map
;;               ("M-TAB" . copilot-accept-completion)
;;               ("M-<tab>" . copilot-accept-completion)
;;               ("M-/" . completion-at-point)
;;               )
;;   :config
;;   (defun me/copilot-tab ()
;;     "Tab command that will complet with copilot if a completion is
;; available. Otherwise will try company, yasnippet or normal
;; tab-indent."
;;     (interactive)
;;     (or (yas-expand)
;;         (copilot-accept-completion)
;;         (indent-for-tab-command)))

;;   (define-key copilot-completion-map (kbd "TAB") 'me/copilot-tab)

;;   :hook
;;   (prog-mode . copilot-mode)
;;   (magit-mode . copilot-mode))

(use-package minuet
  :quelpa (minuet :fetcher github
                  :repo "milanglacier/minuet-ai.el"
                  :branch "main"
                  :files ("*.el"))
  :bind (
         ("C-TAB" . minuet-accept-suggestion-line)
         ("C-<tab>" . minuet-accept-suggestion-line)
         ("M-TAB" . minuet-accept-suggestion)
         ("M-<tab>" . minuet-accept-suggestion)
         ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
         ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
         ("M-/" . minuet-show-suggestion)
         )

  :init
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)

  :config
  (setq minuet-provider 'openai-fim-compatible)
  ;; (setq minuet-provider 'codestral)
  (plist-put minuet-codestral-options :api-key (defun my-fireworks-api-key () codestral-api-key))
  (minuet-set-optional-options minuet-codestral-options :stop ["\n\n"])
  (minuet-set-optional-options minuet-codestral-options :max_tokens 256)

  ;; using ollama
  (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
  ;; I recommend beginning with a small context window size and incrementally
  ;; expanding it, depending on your local computing power. A context window
  ;; of 512, serves as an good starting point to estimate your computing
  ;; power. Once you have a reliable estimate of your local computing power,
  ;; you should adjust the context window to a larger value.
  (setq minuet-context-window 512)
  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  ;; an arbitrary non-null environment variable as placeholder.
  ;; For Windows users, TERM may not be present in environment variables.
  ;; Consider using APPDATA instead.
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56)
  )

(provide 'init-copilot)
