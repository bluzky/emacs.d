
;; Override Evil key binding
(defun better-key-bindings()
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-u") 'kill-whole-line)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  (define-key evil-insert-state-map (kbd "M-d") 'kill-word)
  (define-key evil-insert-state-map (kbd "C-w") 'kill-region)
  (define-key evil-insert-state-map (kbd "C-@") 'set-mark-command)
  )

;; The Emacs default split doesn't seem too intuitive for most users.
(use-package emacs
  :ensure nil
  :after evil
  :preface
  (defun ian/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun ian/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  (better-key-bindings)
  (global-set-key (kbd "C-x 2") #'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'ian/split-and-follow-vertically))

(provide 'init-better-emacs)
