
;; -- COPILOT --

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el")))

;; ;; accept completion from copilot and fallback to company
;; (with-eval-after-load 'company
;;   (delq 'company-preview-if-just-one-frontend company-frontends))

(with-eval-after-load 'copilot
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word))

(add-hook 'prog-mode-hook 'copilot-mode)
(add-hook 'copilot-mode-hook (lambda() (local-set-key (kbd "M-TAB") 'copilot-accept-completion)))

;; -- CHAT GPT --
(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-3.5-turbo") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

(setq org-ai-openai-api-token "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")

(provide 'init-copilot)
