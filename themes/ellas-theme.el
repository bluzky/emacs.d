;;; ellas-theme.el --- A trip to Greece
;;; Version: 1.0
;;; Commentary:
;;; Ellas is an Emacs theme inspired by a trip made to Greece
;;; Code:

(let ((main-bg              "#f8ede4")
      (main-fg              "#3d3b23")

      (bleached-silk        "#f6f6f5")

      (athens-sculpture-1   "#d3c3b6")
      (athens-sculpture-2   "#f1e7de")

      (warm-grey            "#59574c")
      (street-black         "#2a241f")

      (street-pink          "#be8a90")
      (charming-red         "#c24639")
      (agora-tile-red       "#b45952")
      (tourist-shoes-brown  "#3c2a19")
      (aged-jade            "#6b6757")
      (dark-sunshine        "#b09a49")
      (olive-green          "#898442")
      (olive-green-darker   "#665f18")
      (blueish-grey         "#607a96")
      (blueish-grey-lighter "#97c0eb")
      (ellas-blue           "#2b324c")
      (dark-neon-blue       "#001e86")
      (sky-blue-athens      "#074a96")
      (nice-light-blue      "#497fc1")

      (modeline-padding 6))

  (deftheme ellas "A trip to Greece")

  (custom-theme-set-faces
   'ellas

   `(default ((t (:foreground ,main-fg :background ,main-bg ))))
   `(cursor ((t (:background ,olive-green ))))

   `(region ((t (:background ,athens-sculpture-1 ))))

   `(ansi-color-black ((t (:foreground ,street-black :background ,street-black))))
   `(ansi-color-red ((t (:foreground ,charming-red :background ,charming-red))))
   `(ansi-color-green ((t (:foreground ,olive-green :background ,olive-green))))
   `(ansi-color-yellow ((t (:foreground ,dark-sunshine :background ,dark-sunshine))))
   `(ansi-color-blue ((t (:foreground ,sky-blue-athens :background ,blueish-grey-lighter))))
   `(ansi-color-magenta ((t (:foreground ,street-pink :background ,street-pink))))
   `(ansi-color-cyan ((t (:foreground ,dark-neon-blue :background ,dark-neon-blue))))
   `(ansi-color-white ((t (:foreground ,bleached-silk :background ,bleached-silk)))) ; This may have a VERY bad costrast

   `(success ((t (:foreground ,olive-green :weight bold))))
   `(warning ((t (:foreground ,agora-tile-red :weight bold))))

   `(line-number ((t (:inherit default :foreground ,warm-grey ))))
   `(line-number-current-line ((t (:inherit default :foreground ,street-black ))))

   `(highlight ((t (:background ,athens-sculpture-1))))
   `(secondary-selection ((t (:background ,blueish-grey-lighter ))))

   `(fringe ((t (:background ,main-bg))))

   `(mode-line-active ((t (:foreground ,main-fg :background ,athens-sculpture-1
                                       :box (:line-width ,modeline-padding :color ,athens-sculpture-1 :style nil)
                                       ))))
   `(mode-line-inactive ((t (:background ,athens-sculpture-2
                                         :box (:line-width ,modeline-padding :color ,athens-sculpture-2 :style nil)
                                         ;; :overline ,athens-sculpture-1
                                         ;; :underline ,athens-sculpture-1
                                         ))))
   `(mode-line-buffer-id ((t (:foreground ,ellas-blue :bold t ))))

   `(vertical-border ((t (:foreground ,athens-sculpture-1 ))))

   `(font-lock-builtin-face ((t (:foreground ,tourist-shoes-brown :bold t ))))
   `(font-lock-preprocessor-face ((t (:foreground ,tourist-shoes-brown :bold t ))))
   `(font-lock-comment-face ((t (:foreground ,olive-green :slant italic ))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,aged-jade :slant italic ))))
   `(font-lock-doc-face ((t (:foreground ,agora-tile-red ))))
   `(font-lock-doc-markup-face ((t (:foreground ,aged-jade ))))
   `(font-lock-punctuation-face ((t (:foreground ,aged-jade ))))
   `(font-lock-type-face ((t (:foreground ,olive-green-darker ))))
   `(font-lock-function-name-face ((t (:foreground ,sky-blue-athens :bold t ))))
   `(font-lock-variable-name-face ((t (:foreground ,sky-blue-athens ))))
   `(font-lock-keyword-face ((t (:foreground ,dark-neon-blue ))))
   `(font-lock-string-face ((t (:foreground ,agora-tile-red ))))
   `(font-lock-escape-face ((t (:foreground ,olive-green ))))
   `(font-lock-negation-char-face ((t (:foreground ,agora-tile-red ))))
   `(font-lock-number-face ((t (:foreground ,sky-blue-athens ))))
   `(font-lock-constant-face ((t (:foreground ,charming-red ))))
   `(font-lock-regexp-face ((t (:foreground ,agora-tile-red ))))
   `(font-lock-warning-face ((t (:foreground ,charming-red :bold t ))))

   `(minibuffer-prompt ((t (:foreground ,sky-blue-athens :bold t ))))

   '(italic ((t (:slant italic :underline nil))))

   ;; Matches
   '(match ((t nil)))
   '(show-paren-match ((t (:inverse-video t))))

   ;; Clickable elements
   `(link ((t (:foreground ,warm-grey :underline t ))))
   `(custom-button ((t (:foreground ,warm-grey :box (:line-width 3 :style ,warm-grey) :height 0.9 ))))
   `(custom-button-mouse ((t (:foreground ,bleached-silk :box (:line-width 3 :style ,bleached-silk) :height 0.9 ))))
   `(custom-button-mouse ((t (:foreground ,bleached-silk :box (:line-width 3 :style ,bleached-silk) :height 0.9 ))))

   ;; Org mode
   `(org-code ((t (:foreground ,sky-blue-athens ))))
   `(org-verbatim ((t (:foreground ,olive-green ))))
   `(org-block ((t (:background ,athens-sculpture-2 ))))

   `(org-todo ((t (:foreground ,charming-red ))))
   `(org-done ((t (:foreground ,olive-green ))))

   ;; Org agenda
   `(org-agenda-structure ((t (:foreground ,dark-neon-blue :height 1.4 ))))
   `(org-agenda-date ((t (:inherit nil :foreground ,ellas-blue ))))
   `(org-agenda-date-today ((t (:inherit nil :underline t :foreground ,ellas-blue ))))
   `(org-agenda-diary ((t (:foreground ,olive-green-darker ))))

   ;; Eshell
   `(eshell-prompt ((t (:foreground ,charming-red :bold t ))))

   ;; Key cast package
   `(keycast-key ((t (:box nil :foreground ,bleached-silk :background ,warm-grey ))))

   ;; Corfu package
   `(corfu-current ((t (:background ,athens-sculpture-1 ))))
   `(corfu-border ((t (:background ,athens-sculpture-1 ))))
   `(corfu-bar ((t (:background ,tourist-shoes-brown ))))

   `(trailing-whitespace ((t (:background ,agora-tile-red ))))

   `(hl-line ((t (:background ,athens-sculpture-2 ))))

   ;; Tab bars
   `(tab-bar ((t (:background ,main-bg :foreground ,main-fg
                  :underline ,athens-sculpture-1
                  :height 1.0))))
   `(tab-bar-tab ((t (:background ,athens-sculpture-1 :box ,athens-sculpture-1 ))))
   `(tab-bar-tab-inactive ((t (:background ,main-bg :box nil ))))
   )


;;;###autoload
  (and load-file-name
       (boundp 'custom-theme-load-path)
       (add-to-list 'custom-theme-load-path
                    (file-name-as-directory
                     (file-name-directory load-file-name))))

  (provide-theme 'ellas))

;;; ellas-theme.el ends here
