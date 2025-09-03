(use-package meow
  :demand t
  :custom
  (meow-use-clipboard t)
  :hook (after-save . (lambda ()
                        (when (and (bound-and-true-p meow-mode)
                                   (meow-insert-mode-p))
                          (meow-insert-exit))))
  :config
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("h" . meow-left)
   '("l" . meow-right))

  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)


   ;; Top level bindings
   '("'" . vterm-toggle)
   '("Q" . save-buffers-kill-emacs)
   '("R" . restart-emacs)

   ;; File operations (f)
   '("f f" . project-find-file)
   '("f F" . me/find-file-at-point-with-line)
   '("f g" . me/find-file-with-line)
   '("f r" . consult-recent-file)
   '("f D" . me/delete-buffer-file)
   '("f R" . rename-visited-file)
   '("f S" . write-file)
   '("f y" . me/copy-buffer-relative-path)
   '("f Y" . me/copy-buffer-abs-path)

   ;; AI operations (a)
   '("a a" . gptel)
   '("a m" . gptel-menu)

   ;; Relysium operations (e)
   '("e a" . relysium-ask)
   '("e e" . relysium-edit-dwim)
   '("e b" . relysium-buffer-add-context)
   '("e c" . relysium-buffer-clear)
   '("e d" . relysium-debug-log)
   '("e D" . relysium-toggle-debug-mode)
   '("e g" . relysium-generate-from-comments)
   '("e s" . relysium-suggest)
   '("e t" . relysium-buffer-toggle-window)

   ;; Buffer operations (b)
   '("b b" . consult-buffer)
   '("b d" . kill-buffer)
   '("b s" . (lambda () (interactive) (find-file "~/.scratch")))
   '("b S" . save-some-buffers)
   '("b n" . evil-buffer-new)
   '("b t" . tab-new)

   ;; Code operations (c)
   '("c f" . eglot-format)
   '("c a" . eglot-code-actions)
   '("c r" . eglot-rename)
   '("c x" . quickrun)
   '("c X" . quickrun-region)
   '("c i" . imenu-list-smart-toggle)
   '("c e" . flymake-goto-next-error)
   '("c E" . flymake-goto-prev-error)

   ;; Jump operations (j)
   '("j d" . citre-jump)
   '("j r" . xref-find-references)
   '("j i" . consult-imenu)
   '("j j" . avy-goto-char-timer)
   '("j l" . avy-goto-line)
   '("j w" . avy-goto-word-0)

   ;; Git operations (g)
   '("g s" . magit-status)
   '("g p" . me/visit-pull-request-url)
   '("g b" . magit-blame-addition)
   '("g l" . magit-log-buffer-file)
   '("g d" . magit-diff-unstaged)
   '("g r" . pr-review-search)
   '("g R" . pr-review)

   ;; Help operations (h)
   '("h f" . describe-function)
   '("h k" . describe-key)
   '("h t" . consult-theme)
   '("h v" . describe-variable)
   '("h l" . (lambda () (interactive) (setq display-line-numbers t)))
   '("h L" . (lambda () (interactive) (setq display-line-numbers 'relative)))

   ;; Note operations (n)
   '("n n" . denote-open-or-create)
   '("n c" . denote)
   '("n f" . denote-notes-find-file)
   '("n j" . denote-journal-extras-new-or-existing-entry)
   '("n s" . denote-notes-search)

   ;; Insert operations (i)
   '("i r" . yank-from-kill-ring)
   '("i y" . consult-yasnippet)

   ;; Search operations (s)
   '("s s" . swiper)
   '("s S" . swiper-thing-at-point)
   '("s p" . consult-ripgrep)
   '("s P" . me/search-project)
   '("s f" . me/search-dir)
   '("s F" . me/search-dir-with-input)
   '("s r" . vertico-repeat)
   '("s w" . me/search-web)

   ;; Open operations (o)
   '("o p" . project-switch-project)
   '("o b" . list-bookmarks)

   ;; Extra operations (x)
   '("x m" . consult-kmacro)

   ;; Window operations (w)
   '("w t" . tab-switch)
   '("w w" . ace-select-window))

  ;; Define goto keymap for g prefix
  (defvar meow-goto-keymap (make-sparse-keymap)
    "Keymap for goto commands in meow normal state.")
  
  (define-key meow-goto-keymap (kbd "d") 'xref-find-definitions)
  (define-key meow-goto-keymap (kbd "D") 'xref-find-definitions-other-window)
  (define-key meow-goto-keymap (kbd "r") 'xref-find-references)
  
  ;; Navigation bindings
  (define-key meow-goto-keymap (kbd "g") 'beginning-of-buffer)  ; gg - go to beginning of buffer
  (define-key meow-goto-keymap (kbd "e") 'end-of-buffer)       ; ge - go to end of buffer
  (define-key meow-goto-keymap (kbd "s") 'back-to-indentation) ; gs - go to first non whitespace character
  (define-key meow-goto-keymap (kbd "h") 'move-beginning-of-line) ; gh - go to beginning of line
  (define-key meow-goto-keymap (kbd "l") 'move-end-of-line)    ; gl - go to end of line
  (define-key meow-goto-keymap (kbd "c") 'comment-line)        ; gc - comment current line or selected region
  
  ;; Smerge mode bindings (uppercase to avoid conflicts)
  (define-key meow-goto-keymap (kbd "J") 'smerge-next)
  (define-key meow-goto-keymap (kbd "K") 'smerge-prev)
  (define-key meow-goto-keymap (kbd "U") 'smerge-keep-upper)
  (define-key meow-goto-keymap (kbd "L") 'smerge-keep-lower)
  (define-key meow-goto-keymap (kbd "A") 'smerge-keep-all)
  
  (fset 'meow-goto-keymap meow-goto-keymap)
  
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . meow-find-expand)
   '("g" . meow-goto-keymap)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("M" . meow-delete-join)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("S" . meow-kill-append)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-kmacro-lines)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-cancel-selection)
   '("'" . repeat)
   '("\"" . meow-comment)
   '("%" . meow-query-replace)
   '("&" . meow-query-replace-regexp)
   '("<escape>" . ignore))

  ;; Enable meow-mode
  (meow-global-mode 1)

  ;; Set up which-key for meow leader prefixes
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "SPC a" "AI"
      "SPC b" "Buffers"
      "SPC c" "Code"
      "SPC e" "Relysium"
      "SPC f" "Files"
      "SPC g" "Git"
      "SPC h" "Help"
      "SPC i" "Insert"
      "SPC j" "Jump"
      "SPC l" "Links"
      "SPC n" "Notes"
      "SPC o" "Open"
      "SPC s" "Search"
      "SPC w" "Windows"
      "SPC x" "Extra")))

;; Configure C-[ to exit insert state only when in meow insert mode
;; (global-set-key (kbd "C-[")      'meow-insert-exit))


(provide 'init-meow)
