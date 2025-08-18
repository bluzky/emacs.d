;; (use-package vterm
;;   :defer t)

;; (use-package multi-vterm
;;   :defer t)

;; (use-package vterm-toggle
;;   :bind
;;   (("C-c t"        . vterm-toggle)
;;    :map vterm-mode-map
;;    ("<C-return>" . vterm-toggle-insert-cd)
;;    ("s-n" . vterm-toggle-forward)
;;    ("s-p" . vterm-toggle-backward))
;;   :config
;;   (add-to-list 'display-buffer-alist
;;      '("\*vterm\*"
;;        (display-buffer-in-side-window)
;;        (window-height . 0.3)
;;        (side . bottom)
;;        (slot . 0)))
;;   ) ;; end vterm-toggle.el

(use-package eat
  :config
  (add-hook 'eat-mode-hook
            (lambda ()
              (run-with-idle-timer 0.1 nil 'turn-off-evil-mode)))
  :bind
  (("C-c t" . eat-project)
   ("C-c T" . eat-project-other-window)))

(provide 'init-term)
