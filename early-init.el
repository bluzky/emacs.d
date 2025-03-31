(setq package-enable-at-startup nil)

;; tell lsp-mode to use plists
;; after setting this variable, you may need to remove and reinstall the lsp-mode package
(setenv "LSP_USE_PLISTS" "true")

;; hide title bar
;; (add-to-list 'default-frame-alist '(undecorated . t))
