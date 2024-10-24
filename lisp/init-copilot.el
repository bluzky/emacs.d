;; -- COPILOT --


(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :bind (:map copilot-completion-map
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)
              :map copilot-mode-map
              ("M-TAB" . copilot-accept-completion)
              ("M-<tab>" . copilot-accept-completion)
              ("M-/" . completion-at-point)
              )
  :config
  (defun me/copilot-tab ()
    "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
    (interactive)
    (or (yas-expand)
        (copilot-accept-completion)
        (indent-for-tab-command)))

  (define-key copilot-completion-map (kbd "TAB") 'me/copilot-tab)

  :hook
  (prog-mode . copilot-mode)
  (magit-mode . copilot-mode))


(use-package copilot-chat
  :after (request)
  :bind (:map prog-mode-map
              ("C-c C-c" . copilot-chat-ask-and-insert))
  :custom
  (copilot-chat-backend 'request)
  (copilot-chat-frontend 'markdown))

;; (use-package codeium
;;   :quelpa (codeium :fetcher github
;;                    :repo "Exafunction/codeium.el"
;;                    :branch "main")
;;   :init
;;   ;; use globally
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;    ;; (add-hook 'prog-mode-hook
;;    ;;       (lambda ()
;;    ;;           (setq-local completion-at-point-functions
;;    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))

;;   ;; (add-hook 'prog-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

;;   ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local completion-at-point-functions
;;   ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
;;   ;; an async company-backend is coming soon!

;;   ;; codeium-completion-at-point is autoloaded, but you can
;;   ;; optionally set a timer, which might speed up things as the
;;   ;; codeium local language server takes ~0.2s to start up
;;   ;; (add-hook 'emacs-startup-hook
;;   ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

;;   ;; :defer t ;; lazy loading, if you want
;;   :config
;;   (setq use-dialog-box nil) ;; do not use popup boxes

;;   ;; if you don't want to use customize to save the api-key
;;   ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

;;   ;; get codeium status in the modeline
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;   ;; alternatively for a more extensive mode-line
;;   ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;   (setq codeium-api-enabled
;;         (lambda (api)
;;           (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;   ;; you can also set a config for a single buffer like this:
;;   ;; (add-hook 'python-mode-hook
;;   ;;     (lambda ()
;;   ;;         (setq-local codeium/editor_options/tab_size 4)))
;;   )

;; (use-package tabnine
;;   :commands (tabnine-start-process)
;;   :hook (prog-mode . tabnine-mode)
;;   :diminish "⌬"
;;   :custom
;;   (tabnine-wait 1)
;;   (tabnine-minimum-prefix-length 0)
;;   :hook (kill-emacs . tabnine-kill-process)
;;   :config
;;   (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
;;   (tabnine-start-process)
;;   :bind
;;   (:map  tabnine-completion-map
;;   ("<tab>" . tabnine-accept-completion)
;;   ("TAB" . tabnine-accept-completion)
;;   ("M-f" . tabnine-accept-completion-by-word)
;;   ("M-<return>" . tabnine-accept-completion-by-line)
;;   ("C-g" . tabnine-clear-overlay)
;;   ("M-[" . tabnine-previous-completion)
;;   ("M-]" . tabnine-next-completion)))

;; (use-package ellama
;;   :init
;;   ;; setup key bindings
;;   (setopt ellama-keymap-prefix "C-c e")
;;   ;; (require 'llm-ollama)
;;   ;; (setopt ellama-provider
;;  ;;   (make-llm-ollama
;;  ;;    ;; this model should be pulled to use it
;;  ;;    ;; value should be the same as you print in terminal during pull
;;  ;;    :chat-model "rouge/yi-coder-9b-chat"
;;  ;;    :default-chat-non-standard-params '(("num_ctx" . 8192))))
;;   )

(provide 'init-copilot)
