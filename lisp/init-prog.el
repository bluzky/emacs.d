(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; Slightly shorten eldoc display delay.
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-idle-delay 0.4))

;; Highlight indentions
(use-package highlight-indent-guides
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  )

;; Flycheck
;; A modern on-the-fly syntax checking extension – absolute essential
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Auto format code on save
(use-package apheleia
  :init
  (apheleia-global-mode +1)
  )

;; (unless (display-graphic-p)
;;   (quelpa '(popon :fetcher git :url "https://codeberg.org/akib/emacs-popon.git"))
;;   (quelpa '(acm-terminal :fetcher github :repo "twlz0ne/acm-terminal")))


(use-package lsp-mode
  :diminish "LSP"
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ;; (lsp-mode . lsp-completion-mode)
         )
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable nil)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet nil)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind nil)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-eldoc-enable-hover nil)
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  (lsp-ui-doc-mode nil)
  (lsp-ui-doc-glance nil)
  (lsp-ui-doc-enable nil)
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t)
  (setq lsp-log-io t)
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)

       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  :init
  (setq lsp-use-plists t)
  ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands
;;   (lsp-ui-doc-show
;;    lsp-ui-doc-glance)
;;   :after (lsp-mode evil)
;;   :config (setq lsp-ui-doc-enable t
;;                 evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
;;                 lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
;;                 lsp-ui-doc-include-signature t       ; Show signature
;;                 lsp-ui-doc-position 'at-point))

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

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))


(provide 'init-prog)
