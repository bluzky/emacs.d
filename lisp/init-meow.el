(require 'functions)

(use-package meow
  :demand t
  :custom
  (meow-use-clipboard t)
  :hook (after-save . (lambda ()
                        (when (and (bound-and-true-p meow-mode)
                                   (meow-insert-mode-p))
                          (meow-insert-exit))))
  :config
  (meow-setup-indicator)
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
   '("?" . meow-cheatsheet))

  ;; Define goto keymap for g prefix
  (defvar meow-goto-keymap (make-sparse-keymap)
    "Keymap for goto commands in meow normal state.")

  (me/define-keys meow-goto-keymap
                  "d" 'xref-find-definitions
                  "D" 'xref-find-definitions-other-window
                  "r" 'xref-find-references
                  "g" 'beginning-of-buffer        ; gg - go to beginning of buffer
                  "e" 'end-of-buffer             ; ge - go to end of buffer
                  "s" 'back-to-indentation       ; gs - go to first non whitespace character
                  "h" 'move-beginning-of-line    ; gh - go to beginning of line
                  "l" 'move-end-of-line          ; gl - go to end of line
                  "c" 'comment-line              ; gc - comment current line or selected region
                  "J" 'smerge-next               ; smerge mode bindings
                  "K" 'smerge-prev
                  "U" 'smerge-keep-upper
                  "L" 'smerge-keep-lower
                  "A" 'smerge-keep-all)

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

  (global-set-key (kbd "C-u") #'kill-whole-line)
  ;; Enable meow-mode
  (meow-global-mode 1)

  )

;; Configure C-[ to exit insert state only when in meow insert mode
;; (global-set-key (kbd "C-[")      'meow-insert-exit))

(define-key meow-insert-state-keymap [escape] 'meow-insert-exit)
(provide 'init-meow)
