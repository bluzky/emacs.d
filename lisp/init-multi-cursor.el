;; (use-package evil-mc :after evil :config (global-evil-mc-mode 1))
(use-package evil-mc
  :after evil
  ;; :hook
  ;; (prog-mode . turn-on-evil-mc-mode)
  ;; (text-mode . turn-on-evil-mc-mode)
  :init
  (global-evil-mc-mode 1)
  :bind
  ("C-c m c" . evil-mc-make-all-cursors)
  ("C-c m n" . evil-mc-make-and-goto-next-match)
  ("C-c m p" . evil-mc-make-and-goto-prev-match)
  ("C-c m j" . evil-mc-make-cursor-move-next-line)
  ("C-c m k" . evil-mc-make-cursor-move-prev-line)
  ("C-c m u" . evil-mc-undo-last-added-cursor)
  ("C-c m q" . evil-mc-undo-all-cursors)
  :config
  (add-hook 'magit-mode-hook 'turn-off-evil-mc-mode)
  (setq-default evil-mc-one-cursor-show-mode-line-text nil)
  ;; (evil-define-key* '(normal insert) 'global
  ;;   (kbd "C-;")   #'evil-mc-make-cursor-here
  ;;   )
  (evil-define-key* '(normal visual) evil-mc-key-map
    (kbd "<escape>")   #'evil-mc-undo-all-cursors)
  ;; (evil-define-key* '(normal) evil-mc-key-map
  ;;   ;; (kbd "R")   #'evil-mc-make-all-cursors
  ;;   (kbd "C-n")   #'evil-mc-make-and-goto-next-match
  ;;   (kbd "C-p")   #'evil-mc-make-and-goto-prev-match
  ;;   (kbd "C-<down>")   #'evil-mc-make-cursor-move-next-line
  ;;   (kbd "C-<up>")   #'evil-mc-make-cursor-move-prev-line)
  )

(use-package evil-iedit-state)

;; (use-package evil-multiedit
;;   :after evil 
;;   :config
;;   (evil-define-key* 'visual 'global
;;     "R"           #'evil-multiedit-match-all
;;     (kbd "M-d")   #'evil-multiedit-match-and-next
;;     (kbd "M-D")   #'evil-multiedit-match-and-prev
;;     (kbd "C-M-d") #'evil-multiedit-restore)
;;   (evil-define-key* '(normal) 'global
;;     (kbd "M-d")   #'evil-multiedit-match-symbol-and-next
;;     (kbd "M-D")   #'evil-multiedit-match-symbol-and-prev))

(provide 'init-multi-cursor)
