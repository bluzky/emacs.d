(use-package swift-mode
  :mode "\\.swift\\'"
  :hook (swift-mode . (lambda ()
                        (setq swift-basic-offset 4)
                        (setq swift-mode-hook nil)))
  :config
  (setq swift-mode:basic-offset 4))

(provide 'init-swift)
