;;; Update load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'bootstrap-elpaca)
(require 'init-base)
(require 'init-ui)
(require 'init-editor)
(require 'init-completion)
(require 'init-corfu)
(require 'init-treemacs)
(require 'init-meow)
(require 'init-better-emacs)
(require 'init-git)
(require 'init-multi-cursor)
(require 'init-copilot)
(require 'init-term)
(require 'init-whichkey)
(require 'init-ai)
(require 'init-utils)
(require 'my-macros)

;; programming language
(require 'init-prog)
(require 'init-common)
(require 'init-elixir)
;; (require 'init-typescript)
(require 'init-web)
;; (require 'init-go)
(require 'init-writing)
(require 'init-swift)

(provide 'init)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(package-selected-packages
   '(agent-shell doric-themes ef-themes indent-bars spacious-padding))
 '(package-vc-selected-packages
   '((agent-shell :url "https://github.com/xenodium/agent-shell"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:background "#FDF6E3" :extend t)))))
