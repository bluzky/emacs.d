
(use-package
  prisma-ts-mode
  :mode "\\.prisma\\'"
  :hook (prisma-ts-mode . eglot-ensure)
  (before-save . eglot-format)
  :config
  (add-to-list 'eglot-server-programs '(prisma-ts-mode . ("prisma-language-server" "--stdio")))
  )

(use-package
  typescript-ts-mode
  :ensure nil
  :mode ("\\.ts\\'" "\\.tsx\\'" "\\.js\\'" "\\.jsx\\'")
  :hook (typescript-ts-mode . eglot-ensure)
  (before-save . eglot-format)
  ;; (typescript-ts-mode .(lambda () (setq ian/indent-width 2)))
  :custom
  (typescript-indent-level 2)
  :config
  (add-to-list 'treesit-language-source-alist '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode) "typescript-language-server" "--stdio"))
  )

;; (use-package
;;   eglot
;;   :ensure nil
;;   :config
;;   (setq-default eglot-workspace-configuration
;;                 '(:typescript-language-server (:format (:indentSize 2 :baseIndentSize 2 :tabSize 2 :tabSize 2))))
;;   (add-to-list 'eglot-server-programs
;;                '((typescript-ts-mode) "typescript-language-server" "--stdio"))
;;   (add-to-list 'eglot-server-programs '(prisma-ts-mode . ("prisma-language-server" "--" "--stdio"))))

(provide 'init-typescript)
