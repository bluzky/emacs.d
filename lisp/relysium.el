;;; relysium.el --- Automatically apply LLM-created code-suggestions -*- lexical-binding: t; -*-

;; No Copyright for my changes :v

;; Author: Daniel Nguyen <bluesky.1289@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; URL: https://github.com/bluzky/relysium/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package extends on gptel.el.  It uses that package to generate code
;; suggestions based on the user's request.  Those code suggestions will then
;; automatically be applied to the buffer in the format of a git merge.
;; After applying changes, it enters smerge-mode and provides a transient menu
;; to approve, reject, or retry with a new query.

;;; Code:

(require 'gptel)
(require 'smerge-mode)
(require 'transient)

;; Determine the directory containing the relysium package files
(defconst relysium-directory
  (expand-file-name "relysium"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the relysium component files.")

;; Add the directory to load path
(add-to-list 'load-path relysium-directory)

;; Load all the relysium modules
(require 'relysium-utils)
(require 'relysium-common)
(require 'relysium-ask)
(require 'relysium-edit)
(require 'relysium-suggest)

(defgroup relysium nil
  "Apply code changes using gptel."
  :group 'hypermedia)

(defcustom relysium-apply-changes-hook nil
  "Hook run after code changes have been applied on a buffer."
  :group 'relysium
  :type 'hook)

;; Define a transient menu for Elysium with compact layout
(transient-define-prefix relysium-transient-menu ()
  "Elysium actions menu."
  ["Actions"
   :class transient-row
   ("n" "Next" relysium-navigate-next-change)
   ("p" "Prev" relysium-navigate-prev-change)
   ("a" "Accept" relysium-keep-current-change)
   ("d" "Reject" relysium-reject-current-change)
   ("RET" "Accept all" relysium-keep-all-suggested-changes)
   ("x" "Discard all" relysium-discard-all-suggested-changes)
   ("r" "Retry" relysium-retry-query)
   ("q" "Quit" transient-quit-one)])


;;;###autoload
(define-minor-mode relysium-prog-mode
  "Minor mode for elysium in programming modes.
Provides keybindings and integration for elysium code assistance."
  :lighter " Elysium"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-<return>") 'relysium-edit-dwim)
            (define-key map (kbd "C-c a") 'relysium-ask)
            (define-key map (kbd "C-c e t") 'relysium-toggle-window)
            (define-key map (kbd "C-c e b") 'relysium-add-context)
            (define-key map (kbd "C-c e a") 'relysium-ask)
            (define-key map (kbd "C-c e c") 'relysium-clear-buffer)
            (define-key map (kbd "C-c e D") 'relysium-toggle-debug-mode)
            (define-key map (kbd "C-c e d") 'relysium-debug-log)
            (define-key map (kbd "C-c e m") 'relysium-transient-menu)
            (define-key map (kbd "C-c e s") 'relysium-suggest)
            map))

(provide 'relysium)

;;; relysium.el ends here
