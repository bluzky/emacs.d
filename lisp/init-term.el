;; Terminal emulator using eat (Emulate A Terminal)
(use-package eat
  :ensure t
  :hook
  ;; Enable eat in eshell for better terminal emulation
  (eshell-load . eat-eshell-mode)
  :config
  ;; Use char-mode by default for better terminal feel
  (setq eat-term-name "xterm-256color")

  ;; Display eat terminal in right side window
  (add-to-list 'display-buffer-alist
               '("\\*eat\\*"
                 (display-buffer-in-side-window)
                 (window-width . 80)
                 (side . right)
                 (slot . 0))))

;; Key bindings for quick terminal access
(defun eat-toggle ()
  (interactive)
  (let ((buf (get-buffer "*eat*")))
    (if (and buf (get-buffer-window buf))
        (delete-window (get-buffer-window buf))
      (eat))))

(global-set-key (kbd "C-c t") 'eat-toggle)

;; Optional: Keep old vterm config commented out for reference
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


(provide 'init-term)
