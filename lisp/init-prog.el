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
  ;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "gray90")
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

;; (use-package eglot
;;   :ensure nil)

;; (use-package eglot-booster
;;   :quelpa (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster")
;;   :after eglot
;;   :config	(eglot-booster-mode))

;; lSP bridge
(use-package lsp-bridge
  :quelpa (lsp-bridge :fetcher github :repo "manateelazycat/lsp-bridge"
                      :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources"))
  :bind (:map evil-normal-state-map
              ;; ("gd" . lsp-bridge-find-def)
              ;; ("gD" . lsp-bridge-find-def-other-window)
              ;; ("gr" . lsp-bridge-find-references)
              ("K" . lsp-bridge-show-documentation))
  :hook
  (prog-mode . lsp-bridge-mode)
  (gfm-view-mode . (lambda ()
                     (evil-define-key* '(normal visual) gfm-view-mode-map
                       (kbd "q") '(lambda()
                                    (interactive)
                                     (kill-current-buffer)
                                     (ace-delete-window))
                       )))
  :custom
  (lsp-bridge-elixir-lsp-server 'lexical)
  (lsp-bridge-get-project-path-by-filepath #'lsp-bridge-get-project-path-by-filepath)
  (acm-enable-search-file-words nil)
  (acm-enable-tabnine nil)
  (acm-enable-copilot nil)
  (acm-enable-citre nil)

  :config
  (defun lsp-bridge-get-project-path-by-filepath (filename)
    (if-let ((project (project-current filename)))
        (expand-file-name (project-root project))))
  )


(use-package format-all
  :hook (prog-mode . format-all-mode))

(unless (display-graphic-p)
  (quelpa '(popon :fetcher git :url "https://codeberg.org/akib/emacs-popon.git"))
  (quelpa '(acm-terminal :fetcher github :repo "twlz0ne/acm-terminal")))


;; quick evaluate code
(use-package quickrun
  :defer t)

;; ---

;; code folding
(use-package origami
  :hook (prog-mode . origami-mode))

;;
(use-package citre
  :defer t
  :init
  (require 'citre-config)
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; Set these if readtags/ctags is not in your PATH.
   ;; citre-readtags-program "/path/to/readtags"
   citre-ctags-program "/opt/homebrew/bin/ctags"
   ;; Set these if gtags/global is not in your PATH (and you want to use the
   ;; global backend)
   ;; citre-gtags-program "/path/to/gtags"
   ;; citre-global-program "/path/to/global"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function (lambda ()
                                 (interactive)
                                 (let ((project (project-current (buffer-file-name))))
                                     (expand-file-name (project-root project))))

   ;; Set this if you want to always use one location to create a tags file.
   ;; citre-default-create-tags-file-location 'global-cache
   ;; Set this if you'd like to use ctags options generated by Citre
   ;; directly, rather than further editing them.
   ;; citre-edit-ctags-options-manually nil
   ;; If you only want the auto enabling citre-mode behavior to work for
   ;; certain modes (like `prog-mode'), set it like this.
   ;; citre-auto-enable-citre-mode-modes '(prog-mode)
   ))


(provide 'init-prog)
