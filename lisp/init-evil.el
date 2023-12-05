(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-shift-width ian/indent-width)
  :hook (after-init . evil-mode)
  (after-save . evil-normal-state)
  :config
  (setq evil-emacs-state-cursor    '("red" box)
        evil-normal-state-cursor   '("orange1" box)
        evil-visual-state-cursor   '("orange" hollow)
        evil-insert-state-cursor   '("deep sky blue" (bar . 2))
        evil-replace-state-cursor  '("red" bar)
        evil-operator-state-cursor '("red" hollow))
  (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil)
    ;; we don't need evil-commentary
    (define-key evil-insert-state-map (kbd "s-/") 'comment-line)
    (define-key evil-normal-state-map (kbd "s-/") 'comment-line)
    (define-key evil-normal-state-map (kbd "gc") 'comment-line)
    (define-key evil-visual-state-map (kbd "gc") 'comment-line)
    ))

;; Config term cursor for terminal
(unless (display-graphic-p)
  (use-package term-cursor
  :quelpa (term-cursor :repo "h0d/term-cursor.el" :fetcher github)
  :hook
  (after-init . global-term-cursor-mode)
  :config
  (blink-cursor-mode 0))
  )

;; Evil-collection covers more parts of Emacs that the original Evil doesn’t support (e.g. Packages buffer, eshell, calendar etc.)
(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :hook
  (evil-mode . global-evil-surround-mode))

(provide 'init-evil)
