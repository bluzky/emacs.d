
;; -- COPILOT --


(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :bind (:map copilot-completion-map
              ("C-TAB" . copilot-accept-completion-by-word)
              :map copilot-mode-map
              ("M-TAB" . copilot-accept-completion)
              ("M-<tab>" . copilot-accept-completion)
              )
  :config
    (defun me/copilot-tab ()
    "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
    (interactive)
    (or (copilot-accept-completion)
        (yas-expand)
        (indent-for-tab-command)))

    (define-key copilot-completion-map (kbd "TAB") 'me/copilot-tab)
    
  :hook
  (prog-mode . copilot-mode))

(provide 'init-copilot)
