(use-package eww
  :ensure nil
  :custom
  (eww-search-prefix "https://duckduckgo.com/html/?q=")
  :config
  (add-to-list 'meow-mode-state-list '(eww-mode . insert)))

(provide 'init-browser)
