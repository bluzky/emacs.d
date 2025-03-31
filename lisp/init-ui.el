;; The diminish package is used to hide unimportant minor modes in the modeline. It provides the :diminish keyword we’ve been using in other use-package declarations.
(use-package diminish
  :demand t)

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
      (set-face-attribute 'default nil :family "JetBrains Mono NL" :height 150 :weight 'regular)))
  :ensure nil
  :config
  (setq initial-frame-alist '((fullscreen . maximized)))
  (ian/set-default-font))

;; custom theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))


;; Or if you have use-package installed
(use-package kaolin-themes
  :config
  (load-theme 'ellas t))


;; auto dark theme
(defun me/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'ellas t))
    ('dark (load-theme 'kaolin-dark t))))

(add-hook 'ns-system-appearance-change-functions #'me/apply-theme)


;; Dashboard welcome page
(use-package dashboard
  :diminish dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Shut up and do something good!"
        dashboard-center-content t
        dashboard-display-icons-p t
        dashboard-items '(
                          (bookmarks . 5)
                          (projects . 5))
        dashboard-path-max-length 60
        dashboard-set-navigator t
        dashboard-set-footer nil)
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
        `(;; line1
          (("⚡" "Opollo"
            "GH Opollo"
            (lambda (&rest _) (browse-url "https://github.com/onpointvn/opollo")))
           )))
  )


;; show icons for dired mode
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))


;; Add doom fancy modeline
(use-package doom-modeline
  :hook (emacs-startup . doom-modeline-mode))

;; hydra context menu
(use-package hydra)
(use-package pretty-hydra
  :after hydra)

(provide 'init-ui)
