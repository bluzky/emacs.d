;; Modernize selection behavior
;; Replace the active region just by typing text, just like modern editors.
(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

;; Disable scroll-bar
(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

;; Enable column numbers
(use-package simple
  :ensure nil
  :config (column-number-mode +1))



;; By default, the scrolling is way too fast to be precise and helpful, let’s tune it down a little bit.

(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
                mouse-wheel-progressive-speed nil))


;; Maximize the frame by default on start-up. Set the font to size 12.
;; Setup font
(use-package frame
  :preface
  (defun ian/set-default-font ()
    (interactive)
    (when (member "Source Code Pro" (font-family-list))
      (set-face-attribute 'default nil :family "Source Code Pro"))
    (set-face-attribute 'default nil
                        :height 140
                        :weight 'normal))
  :ensure nil
  :config
  (setq initial-frame-alist '((fullscreen . maximized)))
  (ian/set-default-font))

;; custom theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(load-theme 'wilmersdorf t) ; an orginal theme created by me.

;; Dashboard welcome page
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Yay Evil!"
        dashboard-items nil
        dashboard-set-footer nil))

;; The diminish package is used to hide unimportant minor modes in the modeline. It provides the :diminish keyword we’ve been using in other use-package declarations.
(use-package diminish
  :demand t)

(provide 'init-ui)
