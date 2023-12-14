(use-package emacs
  :preface
  (defvar ian/indent-width 2) ; change this value to your preferred width
  :config
  (setq frame-title-format '("Yay-Evil") ; Yayyyyy Evil!
        ring-bell-function 'ignore       ; minimize distraction
        frame-resize-pixelwise t
        default-directory "~/")

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode 0)
  (global-display-line-numbers-mode)

  ;; better scrolling experience
  (setq scroll-margin 0
        scroll-conservatively 101 ; > 100
        scroll-preserve-screen-position t
        auto-window-vscroll nil)

  ;; Always use spaces for indentation
  (setq-default indent-tabs-mode nil
                tab-width ian/indent-width)

  ;; Omit default startup screen
  (setq inhibit-startup-screen t))


;; Dired tweaks
;; Delete intermediate buffers when navigating through dired.
(use-package dired
  :ensure nil
  :defer t
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq delete-by-moving-to-trash t))

;; Dump custom-set-variables to a garbage file and don’t load it
(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file (concat user-emacs-directory "to-be-dumped.el")))

;; Configure PATH on macOS

(use-package exec-path-from-shell
  :config (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize)))

;; enable system clipboard
(use-package xclip
  :config
  (xclip-mode 1))

(provide 'init-base)
