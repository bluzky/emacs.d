;;; config ts-mode for common languages


(use-package dockerfile-ts-mode
  :ensure nil
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerignore\\'" . dockerfile-ts-mode))
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")))
  )

(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'"
  :defer t
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist '(toml "https://github.com/ikatyang/tree-sitter-toml" "master" "src")))
  )

(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist '(yaml "https://github.com/ikatyang/tree-sitter-yaml" "master" "src")))
  )

;; typing game
(use-package speed-type
  :commands (speed-type-text))

(provide 'init-common)
