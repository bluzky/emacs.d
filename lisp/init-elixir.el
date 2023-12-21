(use-package
  heex-ts-mode
  :mode "\\.heex\\'"
  :hook (heex-ts-mode . eglot-ensure)
  (before-save . eglot-format)
  )

(use-package
  elixir-ts-mode
  :after consult
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.eex\\'")
  :hook (elixir-ts-mode . eglot-ensure)
  (before-save . eglot-format)
  )
  ;; :config
  ;; (add-to-list 'consult-imenu-config
  ;;               (elixir-ts-mode :toplevel "Module"
  ;;                               :types ((?f "Functions" font-lock-function-name-face)
  ;;                                       (?m "Module"    font-lock-function-name-face))))
  ;; )

(use-package
  eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((heex-ts-mode elixir-ts-mode elixir-mode) . ("language_server.sh"))))


(provide 'init-elixir)
