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
   :quelpa (copilot-chat :fetcher github
                   :repo "chep/copilot-chat.el"
                   :branch "master"
                   :files ("*.el"))

   :bind (("C-c C-c" . copilot-chat-display))
  :custom
  (copilot-chat-frontend 'markdown)
  (copilot-chat-model "claude-3.7-sonnet"))

(provide 'init-copilot)
