
(use-package
  heex-ts-mode
  :hook (heex-ts-mode . eglot-ensure))

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

(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))
(add-hook 'heex-ts-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

(use-package
  eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((heex-ts-mode elixir-ts-mode elixir-mode) . ("language_server.sh"))))
  ;; (add-to-list 'eglot-server-programs
  ;;              '((typescript-ts-mode) "typescript-language-server" "--stdio"))
  ;; (add-to-list 'eglot-server-programs '(prisma-ts-mode . ("prisma-language-server" "--" "--stdio"))))


(provide 'init-elixir)
