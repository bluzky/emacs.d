;;; use-tool-read-file.el --- Example tool to read file contents -*- lexical-binding: t -*-

;;; Commentary:
;; This script provides an example of using `gptel-make-tool` to create a tool
;; named "read_file". The tool reads and returns the contents of a specified file
;; when the filepath is provided as an argument. It supports relative paths and tilde (~).

;;; Code:

(require 'gptel)

(defun use-tool-read-file ()
  "Create a GPTel tool to read and display the contents of a file.
The tool takes a single argument `filepath` which specifies the path of the file to be read."
  (gptel-make-tool
   :function (lambda (filepath)
               (with-temp-buffer
                 (insert-file-contents (expand-file-name filepath))
                 (buffer-string)))
   :name "read_file"
   :description "Read and display the contents of a file."
   :args (list '(:name "filepath"
                       :type string
                       :description "Path to the file to read. Supports relative paths and ~."))
   :category "filesystem"))

(defun use-tool-list-directory ()
  "Create a GPTel tool to list the contents of a directory."
  (gptel-make-tool
   :function (lambda (directory)
               (mapconcat #'identity
                          (directory-files directory)
                          "\n"))
   :name "list_directory"
   :description "List the contents of a given directory"
   :args (list '(:name "directory"
                       :type string
                       :description "The path to the directory to list"))
   :category "filesystem"))

(defun use-tool-make-directory ()
  "Create a GPTel tool to make a new directory with the given name in the specified parent directory."
  (gptel-make-tool
   :function (lambda (parent name)
               (condition-case nil
                   (progn
                     (make-directory (expand-file-name name parent) t)
                     (format "Directory %s created/verified in %s" name parent))
                 (error (format "Error creating directory %s in %s" name parent))))
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :args (list '(:name "parent"
                       :type string
                       :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                       :type string
                       :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem"))

(defun use-tool-create-file ()
  "Create a GPTel tool to create a new file with specific content in a specified directory."
  (gptel-make-tool
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
                       :type string
                       :description "The directory where to create the file")
               '(:name "filename"
                       :type string
                       :description "The name of the file to create")
               '(:name "content"
                       :type string
                       :description "The content to write to the file"))
   :category "filesystem")
  )

(defun use-tool-edit-file ()
  "Create a GPTel tool to edit a file by applying a list of edits, each specifying a line number, old string, and new string."
  (defun my-gptel--edit_file (file-path file-edits)
    "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
    (with-current-buffer (find-file (expand-file-name file-path))
      (let ((inhibit-read-only t)
            (case-fold-search nil)
            (edit-success nil)
            (file-name (expand-file-name file-path))
            (yes-to-all nil)
            (ask-once-done nil))
        (dolist (file-edit (seq-into file-edits 'list))
          (when-let ((line-number (plist-get file-edit :line_number))
                     (old-string (plist-get file-edit :old_string))
                     (new-string (plist-get file-edit :new_string))
                     (is-valid-old-string (not (string= old-string ""))))
            (goto-char (point-min))
            (forward-line (1- line-number))
            (when (and (search-forward old-string nil t)
                       (or yes-to-all (y-or-n-p (format "Replacing \"%s\" with \"%s\" ?" old-string new-string))))
              (replace-match new-string t t)
              (setq edit-success t)
              (unless (or yes-to-all ask-once-done)
                (setq yes-to-all (y-or-n-p (format "Say yes to all the following replacement?"))
                      ask-once-done t)))))
        (if (and edit-success (y-or-n-p (format "Finished replacing, save changes to file?")))
            (progn
              (write-file file-name)
              (format "Successfully edited %s" file-name))
          (format "Failed to edited %s" file-name)))))

  (gptel-make-tool
   :function #'my-gptel--edit_file
   :name "edit_file"
   :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
   :args (list '(:name "file-path"
                       :type "string"
                       :description "The full path of the file to edit")
               '(:name "file-edits"
                       :type array
                       :items (:type object
                                     :properties
                                     (:line_number
                                      (:type integer :description "The line number of the file where edit starts.")
                                      :old_string
                                      (:type string :description "The old-string to be replaced.")
                                      :new_string
                                      (:type string :description "The new-string to replace old-string.")))
                       :description "The list of edits to apply on the file"))
   :category "filesystem")
  )

(defun use-tool-run-command ()
  "Create a GPTel tool to execute a shell command and return its output."
  (gptel-make-tool
   :function (lambda (command)
               (with-temp-message (format "Running command: %s" command)
                 (shell-command-to-string command)))
   :name "run_command"
   :description "Run a command."
   :args (list
          '(:name "command"
                  :type "string"
                  :description "Command to run."))
   :category "command")
  )

(defun use-tool-read-url()
  "Create a GPTel tool to fetch and parse the contents of a given URL."
  (gptel-make-tool
   :function (lambda (url)
               (with-current-buffer (url-retrieve-synchronously url)
                 (goto-char (point-min))
                 (forward-paragraph)
                 (let ((dom (libxml-parse-html-region (point) (point-max))))
                   (run-at-time 0 nil #'kill-buffer (current-buffer))
                   (with-temp-buffer
                     (shr-insert-document dom)
                     (buffer-substring-no-properties (point-min) (point-max))))))
   :name "read_url"
   :description "Fetch and read the contents of a URL"
   :args (list '(:name "url"
                       :type string
                       :description "The URL to read"))
   :category "web"))





(provide 'gptel-tools)
;;; gptel-tools.el ends here
