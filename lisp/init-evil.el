;; Changed the default :q and :wq to be killing current buffer, instead of killing the frame or subsequently killing Emacs.

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-want-C-u-scroll t
        evil-normal-state-cursor '("orange" box)
        evil-want-keybinding nil
        evil-shift-width ian/indent-width)
  :hook (after-init . evil-mode)
  (after-save . evil-normal-state)
  :config
  (with-eval-after-load 'evil-maps ; avoid conflict with company tooltip selection
    (define-key evil-insert-state-map (kbd "C-n") nil)
    (define-key evil-insert-state-map (kbd "C-p") nil)))


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
  :config (evil-commentary-mode +1))

(provide 'init-evil)
