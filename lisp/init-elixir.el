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
  (with-eval-after-load 'eglot
    (setf (alist-get '(elixir-mode elixir-ts-mode heex-ts-mode)
                     eglot-server-programs
                     nil nil #'equal)
          (eglot-alternatives '(("expert" "--stdio")))))
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
    (add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))
    (add-to-list 'major-mode-remap-alist '(heex-mode . heex-ts-mode)))

  :config
  ;; Install grammars on first use if needed
  (when (and (fboundp 'treesit-install-language-grammar)
             (fboundp 'treesit-language-available-p))
    (dolist (grammar '(elixir heex))
      (unless (treesit-language-available-p grammar)
        (treesit-install-language-grammar grammar))))
  )

(use-package po-mode
  :ensure t
  :mode ("\\.po\\(t\\)?\\'" . po-mode)
  :hook (po-subedit-mode . meow-insert)
  :bind (:map po-subedit-mode-map
              ("C-c C-c" . po-subedit-exit)
              ("C-c C-k" . po-subedit-abort)))

(provide 'init-elixir)
