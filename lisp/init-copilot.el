;;; init-ai.el --- Configuration for AI-related packages in Emacs

;;; Commentary:

;;; Code:

;; (require 'variables)

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el")
  :bind (:map copilot-completion-map
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)
              :map copilot-mode-map
              ("M-TAB" . copilot-accept-completion)
              ("M-<tab>" . copilot-accept-completion)
              ("M-/" . completion-at-point)
              )
  :config
  ;; Set fallback indentation offset to avoid warnings
  (setq copilot-indent-offset-warning-disable t)
  
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



(provide 'init-copilot)
