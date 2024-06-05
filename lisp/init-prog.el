;; Slightly shorten eldoc display delay.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-idle-delay 0.4))

;; (use-package eldoc-box
;;   :diminish eldoc-box-hover-mode
;;   :hook (prog-mode . eldoc-box-hover-mode))

;; Highlight indentions
(use-package highlight-indent-guides
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  ;; (highlight-indent-guides-mode . (lambda ()
  ;;                                   ))
  :config
  (setq highlight-indent-guides-method 'character)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "gray90")
  )

;; Flycheck
;; A modern on-the-fly syntax checking extension – absolute essential
(use-package flycheck
  :hook (prog-mode . flycheck-mode))
  ;; :config
  ;; (global-flycheck-mode +1))

(use-package imenu-list
  :bind
  ("C-c i" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t))


(setq treesit-language-source-alist
      '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))
        (heex . ("https://github.com/phoenixframework/tree-sitter-heex"))
        ))

(dolist (source treesit-language-source-alist)
  (unless (treesit-language-available-p (car source))
    (treesit-install-language-grammar (car source))))

(use-package eglot
  :ensure nil)


;; code folding
(use-package origami
  :hook (prog-mode . origami-mode))

;; jump to definition
(use-package dumb-jump
  :bind
  (:map prog-mode-map
        (("C-c C-o" . dumb-jump-go-other-window)
         ("C-c C-j" . dumb-jump-go)
         ("C-c C-i" . dumb-jump-go-prompt)))
  )

(provide 'init-prog)
