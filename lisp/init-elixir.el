(use-package
  heex-ts-mode
  :after eglot
  :mode "\\.heex\\'"
  ;; :hook (heex-ts-mode . eglot-ensure)
  ;; (before-save . eglot-format)
  :hook
  (heex-ts-mode . lsp-bridge-mode)
  :config
  (add-to-list 'treesit-language-source-alist '(heex "https://github.com/phoenixframework/tree-sitter-heex" "master" "src"))
  )

(use-package
  elixir-ts-mode
  :after eglot
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.eex\\'")
  ;; :hook (elixir-ts-mode . eglot-ensure)
  ;; (before-save . eglot-format)
  :hook
  (elixir-ts-mode . lsp-bridge-mode)
  :config
  (add-to-list 'treesit-language-source-alist '(elixir "https://github.com/elixir-lang/tree-sitter-elixir" "master" "src"))
  ;; (add-to-list 'eglot-server-programs '((heex-ts-mode elixir-ts-mode elixir-mode) . ("nextls" "--stdio=true")))
  ;; (add-to-list 'eglot-server-programs '((heex-ts-mode elixir-ts-mode elixir-mode) . ("language_server.sh" "--stdio")))
  (add-to-list 'eglot-server-programs '((heex-ts-mode elixir-ts-mode elixir-mode) . ("start_lexical.sh")))
  )

(provide 'init-elixir)
