;; Enable mouse and clipboard
(use-package emacs
  :ensure nil
  :init
  ;; Settings that must be available immediately
  (setq initial-scratch-message "")
  ;; enable system clipboard
  (setq select-enable-clipboard t)
  ;; enable mouse selection clipboard support in terminal emacs
  (setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))

  ;; disable auto-save
  (setq auto-save-default nil)

  ;; config answer y/n
  (setq use-short-answers t)

  ;; auto select help window
  (setq help-window-select t)

  ;; prefer vertical split
  (setq split-height-threshold 60)
  (setq split-width-threshold 106)

  ;; Enable system clipboard in terminal mode on macOS
  ;; Must be in :init to work before any copy/paste operations
  (unless (display-graphic-p)
    (when (eq system-type 'darwin)
      ;; Use pbcopy/pbpaste for clipboard integration
      (defun my/paste-from-osx ()
        (shell-command-to-string "pbpaste"))

      (defun my/copy-to-osx (text &optional push)
        (let ((process-connection-type nil))
          (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
            (process-send-string proc text)
            (process-send-eof proc))))

      (setq interprogram-cut-function 'my/copy-to-osx)
      (setq interprogram-paste-function 'my/paste-from-osx)))

  :config
  ;; Disable focus event reporting in terminal to prevent I/O characters
  (unless (display-graphic-p)
    (defun disable-focus-reporting ()
      "Disable terminal focus event reporting."
      (when (fboundp 'send-string-to-terminal)
        (send-string-to-terminal "\e[?1004l")))

    ;; Disable immediately
    (disable-focus-reporting)

    ;; Disable after a delay to override any re-enabling
    (run-with-timer 0.1 nil #'disable-focus-reporting)
    (run-with-timer 0.5 nil #'disable-focus-reporting)

    ;; Make Emacs ignore focus in/out escape sequences
    (define-key input-decode-map "\e[I" [ignore])
    (define-key input-decode-map "\e[O" [ignore]))

  :hook
  (elpaca-after-init . global-hl-line-mode)
  (elpaca-after-init . (lambda ()
                         ;; Mouse disabled to prevent movement tracking issues in terminal
                         ;; (when (fboundp 'xterm-mouse-mode)
                         ;;   (xterm-mouse-mode 1))

                         (when (fboundp 'auto-save-mode)
                           (auto-save-mode -1))))
  )


;; Don't bother confirming killing processes and don't let backup~ files scatter around.
(use-package files
  :ensure nil
  :init
  (setq confirm-kill-processes nil
        create-lockfiles nil ; don't create .# files (crashes 'npm start')
        make-backup-files nil))

;; Enable recentf mode
(use-package recentf
  :ensure nil
  :hook
  (elpaca-after-init . recentf-mode)
  :config
  (run-at-time nil (* 5 60) 'recentf-save-list) ;; auto save every 5 minutes
  )

;; Automatically refreshes the buffer for changes outside of Emacs
;; Auto refreshes every 2 seconds. Don't forget to refresh the version control status as well.
(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :init
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Show matching parentheses
;; Reduce the highlight delay to instantly.
(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-mode)
  :init (setq show-paren-delay 0))

;; Enter ediff with side-by-side buffers to better compare the differences.
(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

;; Auto-pairing quotes and parentheses etc.
;; Electric-pair-mode has improved quite a bit in recent Emacs versions. No longer need an extra package for this. It also takes care of the new-line-and-push-brace feature.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

;; Syntax highlighting improvement
(use-package highlight-numbers
  :defer t
  :hook (prog-mode . highlight-numbers-mode))

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

;; Emacs Anywhere - Edit text in any app using Emacs
(use-package emacs-anywhere
  :ensure (:host github :repo "nohzafk/emacs-anywhere")
  :custom
  ;; Custom frame size and position (position is overridden by mouse location)
  (emacs-anywhere-frame-parameters
   '((name . "emacs-anywhere")
     (width . 80)
     (height . 20)))
  :config
  (when (display-graphic-p)
    (require 'emacs-anywhere))
  (server-start)
  )

(provide 'init-editor)
