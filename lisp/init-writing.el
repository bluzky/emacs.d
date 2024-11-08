(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc -f markdown -t html -s --mathjax --highlight-style pygments")
  (add-to-list 'markdown-code-lang-modes '("elixir" . elixir-ts-mode)))

(use-package d2-mode
  :mode "\\.d2\\'"
  :config
  (setenv "D2_LAYOUT" "dagre")
  (setq d2-output-format ".png"))

(provide 'init-writing)
