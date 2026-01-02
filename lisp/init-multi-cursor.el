(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-l" . mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :custom
  (mc/always-run-for-all t))


(use-package transient
  :config
  ;; Wrapper functions that quit the transient menu
  (defun mc/mark-all-and-quit ()
    "Mark all like this and quit transient."
    (interactive)
    (mc/mark-all-like-this)
    (transient-quit-all))

  (defun mc/edit-lines-and-quit ()
    "Edit lines and quit transient."
    (interactive)
    (mc/edit-lines)
    (transient-quit-all))

  ;; Define transient menu
  (transient-define-prefix mc/transient-menu ()
    "Multiple cursors transient menu."
    ["Multiple Cursors"
     ["Mark"
      ("n" "Mark next" mc/mark-next-like-this :transient t)
      ("p" "Mark previous" mc/mark-previous-like-this :transient t)
      ("m" "Mark all" mc/mark-all-and-quit)
      ("l" "Edit lines" mc/edit-lines-and-quit)]
     ["Skip"
      ("N" "Skip next" mc/skip-to-next-like-this :transient t)
      ("P" "Skip previous" mc/skip-to-previous-like-this :transient t)]
     ["Edit"
      ("i" "Insert mode" meow-insert)
      ("a" "Append mode" meow-append)
      ("q" "Quit" transient-quit-one)]]))

(provide 'init-multi-cursor)
