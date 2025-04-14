(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current t)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.2 . 0.1))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :init
  (global-corfu-mode))

;; (unless (display-graphic-p)
;;   (use-package corfu-terminal
;;     :hook (global-corfu-mode . corfu-terminal-mode)))

;; (use-package nerd-icons-corfu
;;   :after corfu
;;   :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
;; (use-package cape
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   (add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'init-corfu)
