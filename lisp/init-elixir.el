(use-package
  heex-ts-mode
  :mode "\\.heex\\'"
  :config
  (add-to-list 'apheleia-mode-alist
               '(heex-ts-mode . mix-format))
  )

(use-package
  elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.eex\\'")
  :hook (elixir-ts-mode . eglot-ensure)
  :config
  ;; Configure Elixir LS server for eglot
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode elixir-mode) . ("/Users/flex/workspace/expert/apps/expert/burrito_out/expert_darwin_arm64")))

  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
               (heex "https://github.com/phoenixframework/tree-sitter-heex")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install if not already installed
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  :init
  (mp-setup-install-grammars)
  ;; Map major modes to their tree-sitter modes
  (setq major-mode-remap-alist
        '((elixir-mode . elixir-ts-mode)
          (heex-mode . heex-ts-mode)))
  )

(provide 'init-elixir)
