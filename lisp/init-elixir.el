
(use-package
  heex-ts-mode
  :mode "\\.heex\\'"
  :hook (heex-ts-mode . eglot-ensure)
  (before-save . eglot-format))

(use-package
  elixir-ts-mode
  :hook (elixir-ts-mode . eglot-ensure)
  (elixir-ts-mode
   .
   (lambda ()
     (push '(">=" . ?\u2265) prettify-symbols-alist)
     (push '("<=" . ?\u2264) prettify-symbols-alist)
     (push '("!=" . ?\u2260) prettify-symbols-alist)
     (push '("==" . ?\u2A75) prettify-symbols-alist)
     (push '("=~" . ?\u2245) prettify-symbols-alist)
     (push '("<-" . ?\u2190) prettify-symbols-alist)
     (push '("->" . ?\u2192) prettify-symbols-alist)
     (push '("<-" . ?\u2190) prettify-symbols-alist)
     (push '("|>" . ?\u25B7) prettify-symbols-alist)))
  (before-save . eglot-format))

(use-package
  eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((heex-ts-mode elixir-ts-mode elixir-mode) . ("language_server.sh"))))

(provide 'init-elixir)
