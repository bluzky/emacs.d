;; Configure whichkey and setup keybindings ; Code:

;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)

(require 'functions)

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4
        which-key-add-column-padding 1
        which-key-allow-multiple-replacements t
        which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-max-display-columns 4
        which-key-min-display-lines 4
        which-key-separator " : "
        which-key-prevent-C-h-from-cycling t
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-special-keys nil
        which-key-use-C-h-for-paging t))


;; Create a direct leader keymap
(defvar my-leader-map (make-sparse-keymap)
  "Direct leader keymap for SPC bindings")

(defun setup-meow-key-bindings()
  "Setup custom keybindings for meow with direct SPC leader"

  ;; Define which-key groups
  (which-key-add-keymap-based-replacements my-leader-map
    "f" "files"
    "a" "AI"
    "b" "buffers"
    "c" "code"
    "e" "relysium"
    "j" "jump"
    "g" "git"
    "h" "help"
    "n" "notes"
    "i" "insert"
    "s" "search"
    "o" "open"
    "l" "links"
    "x" "extra"
    "w" "windows")

  ;; Top level bindings
  (define-key my-leader-map (kbd "'") 'vterm-toggle)
  (define-key my-leader-map (kbd "Q") 'save-buffers-kill-emacs)
  (define-key my-leader-map (kbd "R") 'restart-emacs)

  ;; File operations
  (define-key my-leader-map (kbd "f f") 'project-find-file)
  (define-key my-leader-map (kbd "f F") 'me/find-file-at-point-with-line)
  (define-key my-leader-map (kbd "f g") 'me/find-file-with-line)
  (define-key my-leader-map (kbd "f r") 'consult-recent-file)
  (define-key my-leader-map (kbd "f D") 'me/delete-buffer-file)
  (define-key my-leader-map (kbd "f R") 'rename-visited-file)
  (define-key my-leader-map (kbd "f S") 'write-file)
  (define-key my-leader-map (kbd "f y") 'me/copy-buffer-relative-path)
  (define-key my-leader-map (kbd "f Y") 'me/copy-buffer-abs-path)

  ;; AI operations
  (define-key my-leader-map (kbd "a a") 'gptel)
  (define-key my-leader-map (kbd "a m") 'gptel-menu)

  ;; Relysium operations (moved from "a e" to "e")
  (define-key my-leader-map (kbd "e a") 'relysium-ask)
  (define-key my-leader-map (kbd "e e") 'relysium-edit-dwim)
  (define-key my-leader-map (kbd "e b") 'relysium-buffer-add-context)
  (define-key my-leader-map (kbd "e c") 'relysium-buffer-clear)
  (define-key my-leader-map (kbd "e d") 'relysium-debug-log)
  (define-key my-leader-map (kbd "e D") 'relysium-toggle-debug-mode)
  (define-key my-leader-map (kbd "e g") 'relysium-generate-from-comments)
  (define-key my-leader-map (kbd "e s") 'relysium-suggest)
  (define-key my-leader-map (kbd "e t") 'relysium-buffer-toggle-window)

  ;; Buffer operations
  (define-key my-leader-map (kbd "b b") 'consult-buffer)
  (define-key my-leader-map (kbd "b d") 'kill-buffer)
  (define-key my-leader-map (kbd "b s") (lambda () (interactive) (find-file "~/.scratch")))
  (define-key my-leader-map (kbd "b S") 'save-some-buffers)
  (define-key my-leader-map (kbd "b n") 'evil-buffer-new)
  (define-key my-leader-map (kbd "b t") 'tab-new)

  ;; Code operations
  (define-key my-leader-map (kbd "c f") 'lsp-bridge-code-format)
  (define-key my-leader-map (kbd "c a") 'lsp-bridge-code-action)
  (define-key my-leader-map (kbd "c x") 'quickrun)
  (define-key my-leader-map (kbd "c X") 'quickrun-region)
  (define-key my-leader-map (kbd "c i") 'imenu-list-smart-toggle)
  (define-key my-leader-map (kbd "c e") 'flymake-goto-next-error)
  (define-key my-leader-map (kbd "c E") 'flymake-goto-prev-error)

  ;; Jump operations
  (define-key my-leader-map (kbd "j d") 'citre-jump)
  (define-key my-leader-map (kbd "j r") 'xref-find-references)
  (define-key my-leader-map (kbd "j i") 'consult-imenu)
  (define-key my-leader-map (kbd "j j") 'avy-goto-char-timer)
  (define-key my-leader-map (kbd "j l") 'avy-goto-line)
  (define-key my-leader-map (kbd "j w") 'avy-goto-word-0)

  ;; Git operations
  (define-key my-leader-map (kbd "g s") 'magit-status)
  (define-key my-leader-map (kbd "g p") 'me/visit-pull-request-url)
  (define-key my-leader-map (kbd "g b") 'magit-blame-addition)
  (define-key my-leader-map (kbd "g l") 'magit-log-buffer-file)
  (define-key my-leader-map (kbd "g d") 'magit-diff-unstaged)
  (define-key my-leader-map (kbd "g r") 'pr-review-search)
  (define-key my-leader-map (kbd "g R") 'pr-review)

  ;; Help operations
  (define-key my-leader-map (kbd "h f") 'describe-function)
  (define-key my-leader-map (kbd "h k") 'describe-key)
  (define-key my-leader-map (kbd "h t") 'consult-theme)
  (define-key my-leader-map (kbd "h v") 'describe-variable)
  (define-key my-leader-map (kbd "h l") (lambda () (interactive) (setq display-line-numbers t)))
  (define-key my-leader-map (kbd "h L") (lambda () (interactive) (setq display-line-numbers 'relative)))

  ;; Note operations
  (define-key my-leader-map (kbd "n n") 'denote-open-or-create)
  (define-key my-leader-map (kbd "n c") 'denote)
  (define-key my-leader-map (kbd "n f") 'denote-notes-find-file)
  (define-key my-leader-map (kbd "n j") 'denote-journal-extras-new-or-existing-entry)
  (define-key my-leader-map (kbd "n s") 'denote-notes-search)

  ;; Insert operations
  (define-key my-leader-map (kbd "i r") 'yank-from-kill-ring)
  (define-key my-leader-map (kbd "i y") 'consult-yasnippet)

  ;; Search operations
  (define-key my-leader-map (kbd "s s") 'swiper)
  (define-key my-leader-map (kbd "s S") 'swiper-thing-at-point)
  (define-key my-leader-map (kbd "s p") 'consult-ripgrep)
  (define-key my-leader-map (kbd "s P") 'me/search-project)
  (define-key my-leader-map (kbd "s f") 'me/search-dir)
  (define-key my-leader-map (kbd "s F") 'me/search-dir-with-input)
  (define-key my-leader-map (kbd "s r") 'vertico-repeat)
  (define-key my-leader-map (kbd "s w") 'me/search-web)

  ;; Open operations
  (define-key my-leader-map (kbd "o p") 'project-switch-project)
  (define-key my-leader-map (kbd "o b") 'list-bookmarks)

  ;; Link operations
  (define-key my-leader-map (kbd "l a") (lambda () (interactive) (browse-url "https://github.com/onpointvn/opollo/actions")))
  (define-key my-leader-map (kbd "l p") (lambda () (interactive) (browse-url "https://github.com/onpointvn/opollo/pulls?q=sort%3Aupdated-desc+is%3Apr+is%3Aopen+author%3Abluzky")))
  (define-key my-leader-map (kbd "l s") (lambda () (interactive) (browse-url "https://duckduckgo.com")))

  ;; Extra operations
  (define-key my-leader-map (kbd "x m") 'consult-kmacro)

  ;; Window operations
  (define-key my-leader-map (kbd "w t") 'tab-switch)
  (define-key my-leader-map (kbd "w w") 'ace-select-window)

  ;; Bind SPC directly to the leader map
  (define-key meow-normal-state-keymap (kbd "SPC") my-leader-map)
  (define-key meow-motion-state-keymap (kbd "SPC") my-leader-map))

;; Setup key bindings when meow is loaded
(with-eval-after-load 'meow
  (setup-meow-key-bindings))

(provide 'init-whichkey)
