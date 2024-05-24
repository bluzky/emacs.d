;;; config ts-mode for common languages


(use-package dockerfile-ts-mode
  :ensure nil
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerignore\\'" . dockerfile-ts-mode))
  :config
    (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src"))
  )

(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'"
  :defer t
  :config
    (add-to-list 'treesit-language-source-alist '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src"))
  )

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'"
  :config
    (add-to-list 'treesit-language-source-alist '(yaml "https://github.com/ikatyang/tree-sitter-yaml" "master" "src"))
  )


(provide 'init-common)
