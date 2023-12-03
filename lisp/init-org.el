;; Some minimal org mode tweaks: org-bullets gives our headings (h1, h2, h3…) a more visually pleasing look.
(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)))

(use-package org-bullets :hook (org-mode . org-bullets-mode))

(provide 'init-org)
