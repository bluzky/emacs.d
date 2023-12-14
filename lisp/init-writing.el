
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc -f markdown -t html -s --mathjax --highlight-style pygments"))

(use-package d2-mode
  :mode "\\.d2\\'"
  :config
  (setenv "D2_LAYOUT" "dagre")
  (setq d2-output-format ".png"))

(provide 'init-writing)
