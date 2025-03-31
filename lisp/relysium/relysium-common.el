;;; relysium-common.el --- Common functionality for relysium -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file contains common functions and variables shared between
;; different relysium modules.

;;; Code:

(require 'smerge-mode)

(defun relysium-keep-all-changes ()
  "Keep all of the LLM suggestions."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (funcall #'smerge-keep-lower))
    (while (ignore-errors (not (smerge-next)))
      (funcall #'smerge-keep-lower))
    (smerge-mode -1)
    (message "All suggested changes applied")))

(defun relysium-discard-all-changes ()
  "Discard all of the LLM suggestions."
  (interactive)
  (undo)
  (smerge-mode -1)
  (message "All suggested changes discarded"))

(defun relysium-navigate-next-change ()
  "Navigate to the next change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-next))
      (message "Navigated to next change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (relysium-transient-menu))

(defun relysium-navigate-prev-change ()
  "Navigate to the previous change suggestion and keep the transient menu active."
  (interactive)
  (if (ignore-errors (smerge-prev))
      (message "Navigated to previous change")
    (message "No more changes"))
  ;; Keep the transient menu active
  (relysium-transient-menu))

(defun relysium-keep-current-change ()
  "Keep the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-lower)
  (if (ignore-errors (smerge-next))
      (progn
        (message "Applied change - move to next")
        ;; Keep the transient menu active if there are more changes
        (relysium-transient-menu))
    (message "All changes reviewed - no more conflicts")
    (smerge-mode -1)))

(defun relysium-discard-current-change ()
  "Reject the current suggested change and move to the next one."
  (interactive)
  (smerge-keep-upper)
  (if (ignore-errors (smerge-next))
      (progn
        (message "Rejected change - move to next")
        ;; Keep the transient menu active if there are more changes
        (relysium-transient-menu))
    (message "All changes reviewed - no more conflicts")
    (smerge-mode -1)))


(provide 'relysium-common)
;;; relysium-common.el ends here
