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
  (setq gptel-model "gemini-pro"
        gptel-backend (gptel-make-gemini "Gemini"
                 :key ai-gemini-api-key
                 :stream t))
 ;;  (setq
 ;; gptel-model 'claude-3-sonnet-20240229 ;  "claude-3-opus-20240229" also available
 ;; gptel-backend (gptel-make-anthropic "Claude"
 ;;                 :stream t :key ai-anthropic-api-key))
  )

(use-package elysium
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)) ; Can be customized to horizontal

;;Use smerge-mode to then merge in the changes
(use-package smerge-mode
  :ensure nil
  :hook
  (prog-mode . smerge-mode))

(provide 'init-ai)
