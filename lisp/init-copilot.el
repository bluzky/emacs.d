;;; init-ai.el --- Configuration for AI-related packages in Emacs

;;; Commentary:

;;; Code:

;; (require 'variables)

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
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

;; (use-package claude-code-ide
;;   :quelpa (claude-code-ide :fetcher github :repo "manzaltu/claude-code-ide.el")
;;   :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
;;   :custom
;;   (claude-code-ide-terminal-backend 'eat)
;;   :config
;;   (claude-code-ide-emacs-tools-setup))


(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; install claude-code.el
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(provide 'init-copilot)
