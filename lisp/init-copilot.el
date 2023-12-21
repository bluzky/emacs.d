
;; -- COPILOT --
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :bind (:map copilot-completion-map
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("M-TAB" . copilot-accept-completion)
              )
  :hook
  (prog-mode . copilot-mode))

(provide 'init-copilot)
