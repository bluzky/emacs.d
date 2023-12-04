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
    (define-key evil-insert-state-map (kbd "C-p") nil)))


(unless (display-graphic-p)
  (require 'term-cursor)
  (global-term-cursor-mode)
  (blink-cursor-mode 0))

;; Evil-collection covers more parts of Emacs that the original Evil doesn’t support (e.g. Packages buffer, eshell, calendar etc.)
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

;; Emulates tpope’s vim commentary package (Use gcc to comment out a line, gc to comment out the target of a motion (for example, gcap to comment out a paragraph), gc in visual mode to comment out the selection etc.)
(use-package evil-commentary
  :after evil
  :diminish
  :hook
  (evil-mode . evil-commentary-mode))

(use-package evil-surround
  :after evil
  :hook
  (evil-mode . global-evil-surround-mode))

(provide 'init-evil)
