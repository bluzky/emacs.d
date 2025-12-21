(use-package
  heex-ts-mode
  :mode "\\.heex\\'"
  :config
  ;; Only configure apheleia if it's loaded
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-mode-alist
                 '(heex-ts-mode . mix-format)))
  )

(use-package
  elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.eex\\'")
  :hook (elixir-ts-mode . eglot-ensure)
  :config
  ;; Configure Elixir LS server for eglot
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode elixir-mode) . ("/Users/flex/workspace/expert/apps/expert/burrito_out/expert_darwin_arm64")))

  :init
  ;; Only setup tree-sitter if available
  (when (and (fboundp 'treesit-available-p)
             (boundp 'treesit-language-source-alist))
    ;; Add grammar sources
    (dolist (grammar
             '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
               (heex "https://github.com/phoenixframework/tree-sitter-heex")))
      (add-to-list 'treesit-language-source-alist grammar))

    ;; Map major modes to their tree-sitter modes
    (setq major-mode-remap-alist
          '((elixir-mode . elixir-ts-mode)
            (heex-mode . heex-ts-mode))))

  :config
  ;; Install grammars on first use if needed
  (when (and (fboundp 'treesit-install-language-grammar)
             (fboundp 'treesit-language-available-p))
    (dolist (grammar '(elixir heex))
      (unless (treesit-language-available-p grammar)
        (treesit-install-language-grammar grammar))))
  )

(provide 'init-elixir)
