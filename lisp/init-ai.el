
;; -- Chat GPT --
;; (use-package org-ai
;;   :commands (org-ai-mode
;;              org-ai-global-mode)
;;   :init
;;   (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
;;   (org-ai-global-mode) ; installs global keybindings on C-c M-a
;;   :config
;;   (setq org-ai-default-chat-model "gpt-3.5-turbo") ; if you are on the gpt-4 beta:
;;   (org-ai-install-yasnippets)
;;   )

;; Config chat gpt
;; Storing in ~/.authinfo. By default, “api.openai.com” is used as HOST and “apikey” as USER.
;; machine api.openai.com login apikey password TOKEN
;; use gemini instead 
(use-package gptel
  :config
  (setq gptel-model "gemini-pro"
        gptel-backend (gptel-make-gemini "Gemini"
                 :key ""
                 :stream t))
  )

(provide 'init-ai)
