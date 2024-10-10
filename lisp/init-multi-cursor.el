;; (use-package evil-mc :after evil :config (global-evil-mc-mode 1))
(use-package evil-mc
  :after evil
  ;; :hook
  ;; (prog-mode . turn-on-evil-mc-mode)
  ;; (text-mode . turn-on-evil-mc-mode)
  :init
  (global-evil-mc-mode 1)

  :pretty-hydra
  ((:quit-key "q" :color "red" :idle 0.5)
   ("Match symbol"
    (("m" evil-mc-make-all-cursors "make all")
     ("n" evil-mc-make-and-goto-next-match "add next match")
     ("N" evil-mc-skip-and-goto-next-match "skip and go to next match")
     ("p" evil-mc-make-and-goto-prev-match "add prev match")
     ("P" evil-mc-skip-and-goto-prev-match "skip and go to prev match"))

    "Make cursor"
    (("j" evil-mc-make-cursor-move-next-line "add next line")
     ("k" evil-mc-make-cursor-move-prev-line "add prev line")
     ("A" evil-mc-make-cursor-in-visual-selection-end "make all cursor end")
     ("I" evil-mc-make-cursor-in-visual-selection-beg "make all cursor beginning"))

    "Action"
    (("u" evil-mc-undo-last-added-cursor "undo last added cursor")
     ("Q" evil-mc-undo-all-cursors "clear cursor" :exit t))
    ))
  :bind
  ("C-," . evil-mc-hydra/body)
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
