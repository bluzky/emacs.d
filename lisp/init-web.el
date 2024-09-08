;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2016-2023 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Web configurations.
;;

;;; Code:


(use-package prettier
  :hook ((js-mode js2-mode css-mode sgml-mode web-mode rjsx-mode) . prettier-mode))

;; CSS
;; (use-package css-mode
;;   :init (setq css-indent-offset 2))

;; SCSS
;; (use-package scss-mode
;;   :init (setq scss-compile-at-save nil))


;; JavaScript
(use-package js
  :init (setq js-indent-level 2))

(use-package rjsx-mode 
  :mode (("\\.js\\'" . js-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-minor-mode)
         (js2-mode . js2-highlight-unused-variables-mode)))

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
;; (when (executable-find "prettier")
;;   (use-package prettier
;;     :diminish
;;     :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
;;     :init (setq prettier-pre-warm 'none)))

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\|svelte\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Adds node_modules/.bin directory to `exec_path'
;; (use-package add-node-modules-path
;;   :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

;; REST
;; (use-package restclient
;;   :mode ("\\.http\\'" . restclient-mode)
;;   :config
;;   (use-package restclient-test
;;     :diminish
;;     :hook (restclient-mode . restclient-test-mode)))

;; (use-package web-mode
;;   :mode ("\\.js\\'" "\\.jsx?$"))


;; (use-package js2-mode
;;   :mode ("\\.js\\'" "\\.jsx\\'")
;;   :hook (js2-mode . eglot-ensure)
;;   (before-save . eglot-format)
;;   (js2-mode .(lambda () (setq ian/indent-width 2))))


;; Support functions
(defun me/search-web ()
  "If selected region, or thing at point, is a url, go there. Otherwise,
use region/thing as a keyword for a google search."
  (interactive)
  (let ((target
         (if (use-region-p)
             (buffer-substring (region-beginning) (region-end))
           (thing-at-point 'symbol))))
    (setq target (read-string "search for: " target))
    (if (ffap-url-p target)
        (browse-url target)
      (browse-url (concat "https://duckduckgo.com/?q="
                          (url-hexify-string target))))))



(provide 'init-web)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
