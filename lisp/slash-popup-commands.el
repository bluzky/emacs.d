;;; slash-popup-commands.el --- Popup slash commands using posframe -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your-email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (posframe "1.0.0"))
;; Keywords: convenience
;; URL: https://github.com/yourusername/slash-popup-commands

;;; Commentary:

;; A minor mode that provides modern document editor-like slash commands
;; with a popup interface using posframe.
;; Type a slash at the beginning of a line or after whitespace to trigger
;; the popup command interface.

;;; Code:

(require 'posframe)
(require 'cl-lib)
(require 'face-remap)  ;; For face color access

;; Customization Options

(defgroup slash-popup-commands nil
  "Popup slash commands configuration options."
  :group 'convenience
  :prefix "slash-popup-")

(defcustom slash-popup-commands-alist nil
  "Mode-specific slash command definitions.
An alist where each entry is (MAJOR-MODE . COMMANDS-ALIST).
COMMANDS-ALIST is itself an alist of (COMMAND-NAME . FUNCTION)."
  :type '(alist :key-type symbol
                :value-type (alist :key-type string
                                   :value-type function))
  :group 'slash-popup-commands)

(defcustom slash-popup-max-items 10
  "Maximum number of items to show in the popup."
  :type 'integer
  :group 'slash-popup-commands)

(defcustom slash-popup-width 40
  "Width of the popup frame in characters."
  :type 'integer
  :group 'slash-popup-commands)

(defcustom slash-popup-trigger-chars '(?\s ?\t)
  "Characters after which a slash will trigger commands.
By default, slash commands can be triggered at the beginning of a line
or after whitespace (space or tab)."
  :type '(repeat character)
  :group 'slash-popup-commands)

(defcustom slash-popup-face-command
  '(:inherit default :weight bold)
  "Face for command names in the popup."
  :type 'face
  :group 'slash-popup-commands)

(defcustom slash-popup-face-description
  '(:inherit default :slant italic)
  "Face for command descriptions in the popup."
  :type 'face
  :group 'slash-popup-commands)

(defcustom slash-popup-face-selected
  '(:inherit highlight)
  "Face for the selected command in the popup."
  :type 'face
  :group 'slash-popup-commands)

(defcustom slash-popup-border-width 2
  "Width of the border around the popup frame."
  :type 'integer
  :group 'slash-popup-commands)

(defcustom slash-popup-border-color "#5a5a5a"
  "Border color for the popup frame."
  :type 'color
  :group 'slash-popup-commands)

;; Internal Variables

(defvar-local slash-popup--display-commands nil
  "Non-nil means we're currently displaying the command popup.")

(defvar-local slash-popup--command-start-point nil
  "Position where the slash command starts.")

(defvar-local slash-popup--current-input ""
  "Current input text for filtering commands.")

(defvar-local slash-popup--current-commands nil
  "List of currently displayed commands.")

(defvar-local slash-popup--selected-index 0
  "Index of the currently selected command in the popup.")

(defvar-local slash-popup--buffer-name " *slash-popup*"
  "Name of the posframe buffer for slash commands.")

(defvar slash-popup--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [down] #'slash-popup-next-command)
    (define-key map [up] #'slash-popup-prev-command)
    (define-key map [return] #'slash-popup-select-command)
    (define-key map (kbd "RET") #'slash-popup-select-command)
    (define-key map [tab] #'slash-popup-select-command)
    (define-key map (kbd "TAB") #'slash-popup-select-command)
    (define-key map [escape] #'slash-popup-cancel)
    (define-key map (kbd "ESC") #'slash-popup-cancel)
    (define-key map "\C-g" #'slash-popup-cancel)
    map)
  "Keymap active when slash popup is displayed.")

;; Theme Integration Functions

(defun slash-popup--get-face-attribute (face attribute &optional inherit)
  "Get FACE's ATTRIBUTE value, with INHERIT option."
  (face-attribute face attribute nil inherit))

(defun slash-popup--get-background-color ()
  "Get the background color from the current theme."
  (slash-popup--get-face-attribute 'default :background t))

(defun slash-popup--get-foreground-color ()
  "Get the foreground color from the current theme."
  (slash-popup--get-face-attribute 'default :foreground t))

;; Core Functions

(defun slash-popup-commands-for-mode (mode commands-alist)
  "Define slash commands for MODE.
COMMANDS-ALIST is an alist where each entry is (COMMAND-NAME . PLIST).
PLIST should include :function and :description keys, and optionally :icon."
  (setq slash-popup-commands-alist
        (cons (cons mode commands-alist)
              (assq-delete-all mode slash-popup-commands-alist))))

(defun slash-popup--get-for-current-mode ()
  "Get the slash commands for the current major mode."
  (cdr (assq major-mode slash-popup-commands-alist)))

(defun slash-popup--can-trigger-p ()
  "Return non-nil if slash command can be triggered at point."
  (and (or (bolp)  ; beginning of line
           (and (> (point) 0)  ; not at beginning of buffer
                (memq (char-before) slash-popup-trigger-chars)))
       ;; Check if after the slash, there's whitespace or end of line
       (or (eolp)
           (memq (char-after) slash-popup-trigger-chars))
       (slash-popup--get-for-current-mode)))  ; commands exist for this mode

(defun slash-popup-next-command ()
  "Select the next command in the popup."
  (interactive)
  (when slash-popup--display-commands
    (setq slash-popup--selected-index
          (min (1- (length slash-popup--current-commands))
               (1+ slash-popup--selected-index)))
    (slash-popup--update-display)))

(defun slash-popup-prev-command ()
  "Select the previous command in the popup."
  (interactive)
  (when slash-popup--display-commands
    (setq slash-popup--selected-index
          (max 0 (1- slash-popup--selected-index)))
    (slash-popup--update-display)))

(defun slash-popup-select-command ()
  "Execute the currently selected command."
  (interactive)
  (when (and slash-popup--display-commands
             slash-popup--current-commands
             (>= slash-popup--selected-index 0)
             (< slash-popup--selected-index (length slash-popup--current-commands)))
    (let* ((cmd-data (nth slash-popup--selected-index slash-popup--current-commands))
           (cmd-fn (cdr cmd-data))
           (start-pos (when (markerp slash-popup--command-start-point)
                        (marker-position slash-popup--command-start-point))))

      ;; First, close the popup and clean up state
      (slash-popup--close)

      ;; Now remove the command text if we have valid positions
      (when start-pos
        (delete-region start-pos (point)))

      ;; Execute the command function
      (when (functionp cmd-fn)
        (funcall cmd-fn)))))

(defun slash-popup-cancel ()
  "Cancel the slash command popup."
  (interactive)
  (when slash-popup--display-commands
    (slash-popup--close)))

(defun slash-popup--filter-commands (input)
  "Filter available commands based on INPUT string."
  (let ((commands (slash-popup--get-for-current-mode))
        (case-fold-search t)) ;; Make search case-insensitive

    (cond
     ;; If no commands defined, return nil
     ((null commands) nil)

     ;; If input is empty, return all commands
     ((string-empty-p input) commands)

     ;; Filter commands by input
     (t
      (cl-remove-if-not
       (lambda (cmd)
         (let ((cmd-name (car cmd)))
           (and cmd-name
                (stringp cmd-name)
                (string-match-p (regexp-quote input) cmd-name))))
       commands)))))

(defun slash-popup--update-display ()
  "Update the popup display with filtered commands."
  (when slash-popup--display-commands
    (let ((filtered-commands (slash-popup--filter-commands slash-popup--current-input)))
      (setq slash-popup--current-commands filtered-commands
            ;; Ensure selected index is in bounds
            slash-popup--selected-index (min (if filtered-commands
                                                 (max 0 (1- (length filtered-commands)))
                                               0)
                                             slash-popup--selected-index))

      ;; Prepare buffer content
      (with-current-buffer (get-buffer-create slash-popup--buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)

          ;; If no commands match, show a message
          (if (null filtered-commands)
              (insert (propertize "No matching commands" 'face 'italic))

            ;; Otherwise show the filtered commands
            (let ((display-commands (seq-take filtered-commands slash-popup-max-items)))
              (dotimes (i (length display-commands))
                (let* ((cmd (nth i display-commands))
                       (cmd-name (car cmd))
                       (selected (= i slash-popup--selected-index))
                       (prefix (if selected "> " "  "))) ; Use "> " for selected items, "  " for non-selected

                  ;; Insert command with proper formatting
                  (insert (propertize prefix 'face (if selected 'slash-popup-face-selected nil))
                          (propertize cmd-name 'face slash-popup-face-command)
                          "\n")))))))

      ;; Get theme-aware colors for background and foreground
      (let ((bg-color (slash-popup--get-background-color))
            (fg-color (slash-popup--get-foreground-color)))

        ;; Position and show the posframe
        (posframe-show slash-popup--buffer-name
                       :position (or (and (markerp slash-popup--command-start-point)
                                          (marker-position slash-popup--command-start-point))
                                     (point))
                       :width slash-popup-width
                       :min-width 20
                       :max-width (min 30 (- (frame-width) 10))
                       :internal-border-width slash-popup-border-width
                       :internal-border-color slash-popup-border-color
                       :background-color bg-color
                       :foreground-color fg-color
                       :refresh t)))))

(defun slash-popup--close ()
  "Close the slash command popup and reset state."
  (remove-hook 'post-command-hook #'slash-popup--post-command-hook t)
  (when (get-buffer slash-popup--buffer-name)
    (posframe-delete slash-popup--buffer-name))
  (setq slash-popup--display-commands nil
        slash-popup--command-start-point nil
        slash-popup--current-input ""
        slash-popup--current-commands nil
        slash-popup--selected-index 0))

(defun slash-popup--post-command-hook ()
  "Monitor user actions to update or close the popup as needed."
  (when slash-popup--display-commands
    (let ((last-command-event-char (and (characterp last-command-event) last-command-event)))
      (cond
       ;; If RET was pressed, select the current command
       ((and last-command-event-char (memq last-command-event-char '(?\r ?\n)))
        (slash-popup-select-command))

       ;; If point moved before the slash, close the popup
       ((or (null slash-popup--command-start-point)
            (< (point) slash-popup--command-start-point)
            ;; If user deleted the slash character, close the popup
            (not (eq (char-after slash-popup--command-start-point) ?/)))
        (slash-popup--close))

       ;; Otherwise, update current input and filter commands
       (t
        (let ((new-input (buffer-substring-no-properties
                          (1+ slash-popup--command-start-point)
                          (point))))
          ;; Always update the display with the new input
          (setq slash-popup--current-input new-input
                slash-popup--selected-index 0)
          (slash-popup--update-display)))))))

(defun slash-popup-key-pressed ()
  "Handle the slash key being pressed to trigger popup."
  (interactive)
  ;; Check if we're in a context where slash commands can be triggered
  (if (slash-popup--can-trigger-p)
      (progn
        (insert "/")
        ;; Initialize the command popup - use a marker to keep track of position
        (setq slash-popup--command-start-point (copy-marker (1- (point)))
              slash-popup--display-commands t
              slash-popup--current-input ""
              slash-popup--selected-index 0)

        ;; Display the initial popup with all commands
        (slash-popup--update-display)

        ;; Set up the command hook to track further actions
        (add-hook 'post-command-hook #'slash-popup--post-command-hook nil t)

        ;; Activate our keymap with override
        (set-transient-map slash-popup--keymap t 'ignore-non-bound))

    ;; Otherwise, just insert a normal slash
    (insert "/")))

;;;###autoload
(define-minor-mode slash-popup-mode
  "Minor mode for showing slash commands in a popup interface.
With prefix argument ARG, turn on if positive, otherwise off."
  :lighter " /Popup"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "/") #'slash-popup-key-pressed)
            (define-key map (kbd "RET") (lambda ()
                                          (interactive)
                                          (if slash-popup--display-commands
                                              (slash-popup-select-command)
                                            (newline))))
            map))

;;;###autoload
(define-globalized-minor-mode global-slash-popup-mode
  slash-popup-mode
  (lambda () (slash-popup-mode 1)))

(defun slash-popup--inhibit-self-insert ()
  "Intercept keys when popup is active to prevent them from inserting."
  (when (and slash-popup--display-commands
             (eq this-command 'newline))
    (setq this-command 'slash-popup-select-command)))

;; Add pre-command hook globally to intercept newline
(add-hook 'pre-command-hook #'slash-popup--inhibit-self-insert)

(provide 'slash-popup-commands)
;;; slash-popup-commands.el ends here
