
;; -- Chat GPT --
(use-package org-ai
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-3.5-turbo") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)
  (setq org-ai-openai-api-token "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")
  )


(provide 'init-ai)
