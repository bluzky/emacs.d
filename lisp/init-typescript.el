
(use-package
  prisma-ts-mode
  :mode "\\.prisma\\'"
  :hook (prisma-ts-mode . eglot-ensure))


(use-package
  typescript-ts-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook (typescript-ts-mode . eglot-ensure)
  (before-save . eglot-format))

(setq-default typescript-indent-level 2)

(use-package
  eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode) "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(prisma-ts-mode . ("prisma-language-server" "--" "--stdio"))))

(provide 'init-typescript)
