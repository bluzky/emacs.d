;;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-
;;; Code:

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; (use-package fussy
;;   :config
;;   (push 'fussy completion-styles)
;;   (setq
;;    ;; For example, project-find-file uses 'project-files which uses
;;    ;; substring completion by default. Set to nil to make sure it's using
;;    ;; flx.
;;    completion-category-defaults nil
;;    completion-category-overrides nil))

(use-package vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-count 17)                    ; Number of candidates to display
  ;; (vertico-resize t)
  (vertico-cycle t) ; Go from last to first candidate and first to last (cycle)?
  :hook ((emacs-startup . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; Scroll vertico minibuffer with mouse wheel
(use-package vertico-mouse
  :ensure nil
  :after vertico
  :hook (vertico-mode . vertico-mouse-mode))

;; Repeat last autocomplete command
(use-package vertico-repeat
  :after vertico
  :ensure nil
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; Setup consult
(use-package consult
  :bind (([remap Info-search]        . consult-info)
         ([remap imenu]              . consult-imenu)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

         ;; use current symbol as search string
         :map minibuffer-local-map
         ("M-r" . consult-history))

  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  (setq consult-preview-key '("S-<down>"))

  ;; auto show preview for consult-line and consult-theme
  (consult-customize
   consult-line :preview-key '(:debounce 0.1 any)
   consult-theme :preview-key '(:debounce 0.4 any))

  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<")

  ;; Optionally make narrowing help available in the minibuffer.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )

;; Search for symbol at point with ripgrep
(defun me/consult-ripgrep-symbol-at-point ()
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))


;; (use-package consult-flyspell
;;   :bind ("M-g s" . consult-flyspell))

;; yasnippet support for consult
(use-package consult-yasnippet
  :bind ("M-g y" . consult-yasnippet)
  :hook ((prog-mode org-mode) . yas-global-mode))

;; Embark provides a sort of context sensitive mini interface to act on
(use-package embark
  :after evil
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ([remap describe-bindings] . embark-bindings)
         :map evil-normal-state-map
         ("C-." . embark-act))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :config
  (with-eval-after-load 'which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))

    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator)))

;; use embark with consult
(use-package embark-consult
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export)))

;; Swiper for better search within buffer
(use-package swiper :defer t)

;; Edit search result buffer directly
(use-package wgrep
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
         :map ivy-occur-mode-map
              ("e" . wgrep-change-to-wgrep-mode)))


(provide 'init-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
