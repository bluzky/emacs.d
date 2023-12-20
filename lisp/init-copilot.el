
;; -- COPILOT --
(use-package copilot
  :quelpa (copilot  :fetcher github :repo "zerolfx/copilot.el" :files ("*.el" "dist"))
  :hook
  (prog-mode . copilot-mode)
  (copilot-mode . (lambda()(local-set-key (kbd "M-TAB") 'copilot-accept-completion)))
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word))


;; -- CHAT GPT --
;; (use-package org-ai
;;   :commands (org-ai-mode
;;              org-ai-global-mode)
;;   :init
;;   (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
;;   (org-ai-global-mode) ; installs global keybindings on C-c M-a
;;   :config
;;   (setq org-ai-default-chat-model "gpt-3.5-turbo") ; if you are on the gpt-4 beta:
;;   (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

;; (setq org-ai-openai-api-token "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")

(provide 'init-copilot)
