;;; Update load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'bootstrap)
(require 'init-base)
(require 'init-ui)
(require 'init-treemacs)
(require 'init-editor)
(require 'init-completion)
(require 'init-corfu)
(require 'init-evil)
(require 'init-better-emacs)
(require 'init-org)
(require 'init-git)
(require 'init-multi-cursor)
(require 'init-copilot)
(require 'init-whichkey)

;; programming language
(require 'init-elixir)
(require 'init-typescript)

(provide 'init)
