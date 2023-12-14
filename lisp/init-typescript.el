
(use-package
  prisma-ts-mode
  :mode "\\.prisma\\'"
  :hook (prisma-ts-mode . eglot-ensure))


(setq-default typescript-indent-level 2)
(setq-default js-indent-level 2)
(use-package
  typescript-ts-mode
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :hook (typescript-ts-mode . eglot-ensure)
  (before-save . eglot-format)
  (typescript-ts-mode .(lambda () (setq ian/indent-width 2))))

(use-package js2-mode
  :mode ("\\.js\\'" "\\.jsx\\'")
  :hook (js2-mode . eglot-ensure)
  (before-save . eglot-format)
  (js2-mode .(lambda () (setq ian/indent-width 2))))

(use-package
  eglot
  :ensure nil
  :config
  (setq-default eglot-workspace-configuration
                '(:typescript-language-server (:format (:indentSize 2 :baseIndentSize 2 :tabSize 2 :tabSize 2))))
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode) "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(prisma-ts-mode . ("prisma-language-server" "--" "--stdio"))))

(provide 'init-typescript)
