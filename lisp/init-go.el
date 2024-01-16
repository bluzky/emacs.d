(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . eglot-ensure)
  (before-save . gofmt-before-save)
  :custom (gofmt-command "goimports")
  :config
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")))))
  )

(provide 'init-go)
