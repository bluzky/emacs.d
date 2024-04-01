;; Slightly shorten eldoc display delay.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-idle-delay 0.4))

;; (use-package eldoc-box
;;   :diminish eldoc-box-hover-mode
;;   :hook (prog-mode . eldoc-box-hover-mode))


(setq treesit-language-source-alist
      '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))
        (heex . ("https://github.com/phoenixframework/tree-sitter-heex"))
        ))

(dolist (source treesit-language-source-alist)
  (unless (treesit-language-available-p (car source))
    (treesit-install-language-grammar (car source))))

(use-package eglot
  :ensure nil)

(provide 'init-prog)
