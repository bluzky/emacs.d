(defadvice find-file-noselect (around find-file-noselect-at-line
                                      (filename &optional nowarn rawfile wildcards)
                                      activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename))
           (buffer-name ad-do-it))
      (when line-number
        (with-current-buffer buffer-name
          (goto-char (point-min))
          (forward-line (1- line-number)))))))


;; custom functions
(defun me/delete-buffer-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(defun me/find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
         (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file-at-point)
  (if (not (equal line-num 0))
      (goto-line line-num)))


(defun me/open-file-read-args (prompt mustmatch)
  "Read a file name, prompting with PROMPT and requiring it to exist if MUSTMATCH is non-nil."
  (let* ((pr (project-current t))
         (root (project-root pr))
         (result
          (list (read-file-name prompt root nil mustmatch)
                t)))
    (list (car result))
    ))

(defun me/find-file-with-line(filepath)
  "Open a file and go to a line number if it is specified"
  (interactive (me/open-file-read-args "Find file: " nil))
  (find-file filepath))

(defun me/copy-buffer-abs-path ()
  "Copy the absolute file path of the current buffer to the kill ring."
  (interactive)
  (if-let ((file-path (buffer-file-name)))
      (progn
        (kill-new file-path)
        (message "Copied file path: %s" file-path))
    (message "Buffer is not associated with a file.")))

(defun me/copy-buffer-relative-path ()
  "Get the relative path of the current buffer file with respect to the project root."
  (interactive)
  (if-let ((file-path (buffer-file-name))
          (project-root (project-root (project-current))))
      (let ((relative-path (file-relative-name file-path project-root)))
        (kill-new relative-path)
        (message "Relative path copied: %s" relative-path))
    (message "Buffer is not associated with a file or not in a project.")))

(provide 'functions)
;;; functions.el ends here
