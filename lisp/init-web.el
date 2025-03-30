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

;; install tree-sitter language grammars
(let ((treesit-language-source-alist
       '(
         (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
         (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
         (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
         (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
         (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
         (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
         (prisma "https://github.com/victorhqc/tree-sitter-prisma")
         )))
  (dolist (source treesit-language-source-alist)
    (unless (treesit-language-available-p (car source))
      (treesit-install-language-grammar (car source)))))

;; Remap major modes
(dolist (mapping
         '((css-mode . css-ts-mode)
           (typescript-mode . typescript-ts-mode)
           (js-mode . typescript-ts-mode)
           (js2-mode . typescript-ts-mode)
           (css-mode . css-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))


;; Add item to major-mode-remap-alist
(let ((my-mode-alist
       '(("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         )))
  (dolist (association my-mode-alist)
    (add-to-list 'auto-mode-alist association)))

(add-hook 'typescript-ts-mode-hook #'lsp-deferred)
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)
(add-hook 'json-ts-mode-hook #'lsp-deferred)


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
