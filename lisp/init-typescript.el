
(use-package
    prisma-ts-mode
    :hook (prisma-ts-mode . eglot-ensure)
    :config
    (add-to-list 'auto-mode-alist '("\\.prisma\\'" . prisma-ts-mode)))


(use-package
  typescript-ts-mode
  :hook (typescript-ts-mode . eglot-ensure)
  (before-save . eglot-format)
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode)))

 (setq-default typescript-indent-level 2)

(use-package
  eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode) "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(prisma-ts-mode . ("prisma-language-server" "--" "--stdio"))))

(provide 'init-typescript)
