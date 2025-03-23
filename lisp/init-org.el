;; Some minimal org mode tweaks: org-bullets gives our headings (h1, h2, h3…) a more visually pleasing look.


(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/notes")

;; Denote
(use-package denote
  :init
  (require 'denote-org-extras)
    (require 'denote-journal-extras)

  (denote-rename-buffer-mode 1)
  :custom
  (denote-directory (concat org-directory "/denote"))
  :hook
  (dired-mode . denote-dired-mode)
  :config
  (define-key global-map (kbd "C-c n") (make-sparse-keymap))
  (let ((map global-map))
  (define-key map (kbd "C-c n n") #'denote)
  (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
  (define-key map (kbd "C-c n N") #'denote-type)
  (define-key map (kbd "C-c n d") #'denote-date)
  (define-key map (kbd "C-c n z") #'denote-signature) ; "zettelkasten" mnemonic
  (define-key map (kbd "C-c n s") #'denote-subdirectory)
  (define-key map (kbd "C-c n t") #'denote-template)
  (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c n I") #'denote-add-links)
  (define-key map (kbd "C-c n b") #'denote-backlinks)
  (define-key map (kbd "C-c n f f") #'denote-find-link)
  (define-key map (kbd "C-c n f b") #'denote-find-backlink)
  (define-key map (kbd "C-c n j") #'denote-journal-extras-new-or-existing-entry)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c n r") #'denote-rename-file)
  (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))
  :custom-face
  (denote-faces-link ((t (:slant italic)))))

;; Denote extensions
(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("denote" ?d ,org-directory)))
  :config
  (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  )

(use-package org
  :after
  denote
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :init
  (add-to-list 'org-src-lang-modes '("elixir" . elixr-ts))
  :custom
  (org-default-notes-file (concat org-directory "/Inbox.org"))
  (org-capture-bookmark nil)
  (denote-journal-extras-directory "Journal")
  ;; Capture templates
  (org-capture-templates
   '(("f" "Fleeting note" item
      (file+headline org-default-notes-file "Notes")
      "- %?")
     ("p" "Permanent note" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?")
     ("j" "Journal" entry
                 (file denote-journal-extras-path-to-new-or-existing-entry)
                 "* %U %?\n%i\n%a"
                 :kill-buffer t
                 :empty-lines 1)))
  )



(use-package org-bullets :hook (org-mode . org-bullets-mode))

(provide 'init-org)
