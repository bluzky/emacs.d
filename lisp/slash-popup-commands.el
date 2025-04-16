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
;;
;; Commands can be registered per-buffer, allowing different buffers of
;; the same major mode to have different slash commands.

;;; Code:

(require 'posframe)
(require 'cl-lib)
(require 'face-remap)  ;; For face color access

;; Customization Options

(defgroup slash-popup-commands nil
  "Popup slash commands configuration options."
  :group 'convenience
  :prefix "slash-popup-")

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

(defcustom slash-popup-border-width 1
  "Width of the border around the popup frame."
  :type 'integer
  :group 'slash-popup-commands)

(defface slash-popup-face-selected
  '((t :inherit highlight :weight bold :extend t))
  "Face for the selected command in the popup."
  :group 'slash-popup-commands)

;; Buffer-local variables

(defvar-local slash-popup-buffer-commands nil
  "Buffer-local slash commands.
An alist where each entry is (COMMAND-NAME . FUNCTION).")

;; Global State

(defvar slash-popup--state nil
  "Global state for slash popups.
An alist with these keys:
- `buffer': The original buffer where the popup was triggered
- `start-point': Position where the slash command starts
- `input': Current input text for filtering commands
- `commands': List of currently displayed commands
- `selected-index': Index of currently selected command
- `popup-buffer': Name of the buffer used for the popup display
- `active': Non-nil means the popup is currently active")

(defvar slash-popup--buffer-name " *slash-popup*"
  "Name of the posframe buffer for slash commands.")

(defvar slash-popup--keymap
  (let ((map (make-sparse-keymap)))
    ;; Navigation - arrow keys, Emacs standard keys
    (define-key map [down] #'slash-popup-next-command)
    (define-key map [up] #'slash-popup-prev-command)

    ;; Cancellation
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
  (slash-popup--get-face-attribute 'hl-line :background t))

(defun slash-popup--get-foreground-color ()
  "Get the foreground color from the current theme."
  (slash-popup--get-face-attribute 'default :foreground t))

(defun slash-popup--get-border-color ()
  "Get the background color from the current theme."
  (slash-popup--get-face-attribute 'highlight :background t))

;; Core Functions

(defun slash-popup-set-buffer-commands (commands-alist &optional buffer)
  "Set slash commands for the specified BUFFER.
COMMANDS-ALIST is an alist where each entry is (COMMAND-NAME . FUNCTION).
If BUFFER is nil, use the current buffer.
This replaces any existing commands for the buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq slash-popup-buffer-commands commands-alist)))

(defun slash-popup-clear-buffer-commands (&optional buffer)
  "Clear all slash commands for the specified BUFFER.
If BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (setq slash-popup-buffer-commands nil)))

(defun slash-popup--get-available-commands ()
  "Get all available slash commands for the current buffer."
  slash-popup-buffer-commands)

(defun slash-popup--can-trigger-p ()
  "Return non-nil if slash command can be triggered at point."
  (and (or (bolp)  ; beginning of line
           (and (> (point) 0)  ; not at beginning of buffer
                (memq (char-before) slash-popup-trigger-chars)))
       ;; Check if after the slash, there's whitespace or end of line
       (or (eolp)
           (memq (char-after) slash-popup-trigger-chars))
       slash-popup-buffer-commands))  ; commands exist for this buffer

(defun slash-popup-next-command ()
  "Select the next command in the popup."
  (interactive)
  (when (alist-get 'active slash-popup--state)
    (let* ((commands (alist-get 'commands slash-popup--state))
           (current-index (alist-get 'selected-index slash-popup--state 0))
           (new-index (min (1- (length commands))
                           (1+ current-index))))
      (setf (alist-get 'selected-index slash-popup--state) new-index)
      ;; (slash-popup--update-display)
      )))

(defun slash-popup-prev-command ()
  "Select the previous command in the popup."
  (interactive)
  (when (alist-get 'active slash-popup--state)
    (let* ((current-index (alist-get 'selected-index slash-popup--state 0))
           (new-index (max 0 (1- current-index))))
      (setf (alist-get 'selected-index slash-popup--state) new-index)
      ;; (slash-popup--update-display)
      )))

;; Define submenu as a special symbol
(defvar slash-popup-submenu-symbol 'submenu
  "Symbol used to identify submenu commands.")


(defun slash-popup-select-command ()
  "Execute the currently selected command or navigate to a sub-menu."
  (interactive)
  (when (alist-get 'active slash-popup--state)
    (let* ((commands (alist-get 'commands slash-popup--state))
           (index (alist-get 'selected-index slash-popup--state 0))
           (cmd-data (nth index commands))
           (cmd-name (car cmd-data))
           (cmd-fn (cdr cmd-data))
           (buffer (alist-get 'buffer slash-popup--state))
           (start-pos (when (markerp (alist-get 'start-point slash-popup--state))
                        (marker-position (alist-get 'start-point slash-popup--state)))))


      ;; Check if this is a submenu by checking if cmd-fn is a list with 'submenu as the first element
      (if (and (listp cmd-fn) (eq (car-safe cmd-fn) slash-popup-submenu-symbol))
          ;; Handle submenu - cmd-fn is (submenu . commands-list)
          (let ((submenu-commands (cdr cmd-fn)))
            ;; Save current state to parent stack
            (push (cons (alist-get 'commands slash-popup--state)
                        (alist-get 'input slash-popup--state ""))
                  (alist-get 'parent-commands slash-popup--state))

            ;; Update command path
            (push cmd-name (alist-get 'command-path slash-popup--state))

            ;; Clear the buffer input back to just the slash character
            (when (and buffer (buffer-live-p buffer) start-pos)
              (with-current-buffer buffer
                (delete-region (1+ start-pos) (point))))

            ;; Set new commands and reset selection state
            (setf (alist-get 'commands slash-popup--state) submenu-commands)
            (setf (alist-get 'command-list slash-popup--state) submenu-commands)
            (setf (alist-get 'selected-index slash-popup--state) 0)
            (setf (alist-get 'input slash-popup--state) "")

            ;; Update the display
            ;; (slash-popup--update-display)
            )

        ;; Regular command execution
        (slash-popup--close)

        ;; Switch to the original buffer
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            ;; Remove the command text
            (when start-pos
              (delete-region start-pos (point)))

            ;; Execute the command function
            (when (functionp cmd-fn)
              (funcall cmd-fn))))))))


(defun slash-popup-cancel ()
  "Cancel the slash command popup."
  (interactive)
  (when (alist-get 'active slash-popup--state)
    (slash-popup--close)))

(defun slash-popup--filter-commands (input)
  "Filter available commands based on INPUT string.
Sort filtered commands by the position of the matched string,
commands that match at the beginning of the name appear first."
  (let ((commands (alist-get 'command-list slash-popup--state))
        (case-fold-search t)) ;; Make search case-insensitive

    (cond
     ;; If no commands defined, return nil
     ((null commands) nil)

     ;; If input is empty, return all commands
     ((string-empty-p input) commands)

     ;; Filter commands by input and sort by match position
     (t
      (let ((filtered-commands
             (cl-remove-if-not
              (lambda (cmd)
                (let ((cmd-name (car cmd)))
                  (and cmd-name
                       (stringp cmd-name)
                       (string-match-p (regexp-quote input) cmd-name))))
              commands)))

        ;; Sort filtered commands by match position
        (sort filtered-commands
              (lambda (a b)
                (let* ((name-a (car a))
                       (name-b (car b))
                       (pos-a (string-match (regexp-quote input) name-a))
                       (pos-b (string-match (regexp-quote input) name-b)))
                  (< pos-a pos-b)))))))))

(defun slash-popup--update-display ()
  "Update the popup display with filtered commands.
Closes the popup if no commands match the filter."
  (when (alist-get 'active slash-popup--state)
    (let* ((input (alist-get 'input slash-popup--state ""))
           (filtered-commands (slash-popup--filter-commands input))
           (selected-index (alist-get 'selected-index slash-popup--state 0))
           (command-path (alist-get 'command-path slash-popup--state))
           ;; Ensure selected index is in bounds
           (selected-index (min (if filtered-commands
                                    (max 0 (1- (length filtered-commands)))
                                  0)
                                selected-index)))

      (setf (alist-get 'commands slash-popup--state) filtered-commands)

      ;; If no commands match, close the popup
      (if (null filtered-commands)
          ;; (insert (propertize "No matching commands" 'face 'italic))
          (slash-popup--close)

        ;; Otherwise continue with normal update
        ;; Update the state
        (setf (alist-get 'selected-index slash-popup--state) selected-index)

        ;; Prepare buffer content
        (with-current-buffer (get-buffer-create slash-popup--buffer-name)
          (let ((inhibit-read-only t))
            (erase-buffer)

            ;; Show the command path if we're in a submenu
            (when command-path
              (insert (propertize (concat "/"
                                          (mapconcat #'identity (reverse command-path) "/")
                                          "/")
                                  'face 'font-lock-comment-face))
              (insert "\n\n"))

            ;; Show the filtered commands
            (let ((display-commands (seq-take filtered-commands slash-popup-max-items)))
              (dotimes (i (length display-commands))
                (let* ((cmd (nth i display-commands))
                       (cmd-name (car cmd))
                       (cmd-fn (cdr cmd))
                       (selected (= i selected-index))
                       (is-submenu (and (listp cmd-fn)
                                        (eq (car-safe cmd-fn) slash-popup-submenu-symbol)))
                       (prefix (if selected " > " "   "))
                       ;; Add an indicator for submenus
                       (suffix (if is-submenu " »" ""))
                       (content (concat prefix cmd-name suffix "\n")))

                  ;; Insert command with proper formatting
                  (insert (propertize content
                                      'face (if selected 'slash-popup-face-selected nil))))))))

        ;; Get theme-aware colors
        (let ((bg-color (slash-popup--get-background-color))
              (fg-color (slash-popup--get-foreground-color))
              (border-color (slash-popup--get-border-color)))

          ;; Position and show the posframe
          (posframe-show slash-popup--buffer-name
                         :position (or (and (markerp (alist-get 'start-point slash-popup--state))
                                            (marker-position (alist-get 'start-point slash-popup--state)))
                                       (with-current-buffer (alist-get 'buffer slash-popup--state)
                                         (point)))
                         :width slash-popup-width
                         :min-width 20
                         :max-width (min 40 (- (frame-width) 10))
                         :internal-border-width slash-popup-border-width
                         :internal-border-color border-color
                         :background-color bg-color
                         :foreground-color fg-color
                         :refresh t))
        ))))

(defun slash-popup--close ()
  "Close the slash command popup and reset state."
  (remove-hook 'post-command-hook #'slash-popup--post-command-hook)
  (when (get-buffer slash-popup--buffer-name)
    (posframe-delete slash-popup--buffer-name))
  ;; Clean up keymap
  (setq overriding-terminal-local-map nil)
  (setf (alist-get 'active slash-popup--state) nil)
  (setq slash-popup--state nil)


  ;; Ensure keyboard focus returns to the original buffer
  (when-let ((buffer (alist-get 'buffer slash-popup--state)))
    (when (buffer-live-p buffer)
      (select-window (get-buffer-window buffer)))))

(defun slash-popup--post-command-hook ()
  "Monitor user actions to update or close the popup as needed."
  (cl-block slash-popup--post-command-hook
    (when (alist-get 'active slash-popup--state)
      (let* ((buffer (alist-get 'buffer slash-popup--state)))
        ;; Check if we've switched to a different buffer
        (when (not (eq (current-buffer) buffer))
          (slash-popup--close)
          (cl-return-from slash-popup--post-command-hook))

        ;; Continue with normal processing in the original buffer
        (cond
         ;; If RET was pressed, select the current command
         ;; ((and last-command-event-char (memq last-command-event-char '(?\r ?\n)))
         ;;  (slash-popup-select-command))

         ;; If point moved before the slash, close the popup
         ((let ((start-point (alist-get 'start-point slash-popup--state)))
            (or (null start-point)
                (< (point) start-point)
                ;; If user deleted the slash character, close the popup
                (not (eq (char-after start-point) ?/))))
          (slash-popup--close))

         ;; Otherwise, update current input and filter commands
         (t
          (let* ((start-point (alist-get 'start-point slash-popup--state))
                 (new-input (buffer-substring-no-properties
                             (1+ start-point)
                             (point)))
                 (old-input (alist-get 'input slash-popup--state "")))


            ;; Only reset selection if the input changed
            (when (not (string= old-input new-input))
              (setf (alist-get 'input slash-popup--state) new-input)
              (setf (alist-get 'selected-index slash-popup--state) 0))

            (slash-popup--update-display))))))))

(defun slash-popup-key-pressed ()
  "Handle the slash key being pressed to trigger popup."
  (interactive)
  ;; Check if we're in a context where slash commands can be triggered
  (if (slash-popup--can-trigger-p)
      (progn
        (insert "/")
        ;; Initialize the command popup with global state
        ;; Set the state
        (setq slash-popup--state
              `((buffer . ,(current-buffer))
                (start-point . ,(copy-marker (1- (point))))
                (input . "")
                (commands . ,(slash-popup--get-available-commands))
                (command-list . ,(slash-popup--get-available-commands))
                (selected-index . 0)
                (popup-buffer . ,slash-popup--buffer-name)
                (command-path . ())
                (parent-commands . nil)
                (active . t)))

        (setf (alist-get 'command-path slash-popup--state) nil)
        (setf (alist-get 'active slash-popup--state) t)

        ;; Display the initial popup with all commands
        ;; (slash-popup--update-display)

        ;; Set up the command hook to track further actions
        (add-hook 'post-command-hook #'slash-popup--post-command-hook)

        ;; Use overriding-terminal-local-map for higher priority
        (setq overriding-terminal-local-map slash-popup--keymap)

        ;; Also set up a cleanup function for when the keymap is deactivated
        (add-hook 'post-command-hook
                  (lambda ()
                    (unless (alist-get 'active slash-popup--state)
                      (setq overriding-terminal-local-map nil)
                      (remove-hook 'post-command-hook 'slash-popup--cleanup-keymap)))
                  nil t))

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
                                          (if (alist-get 'active slash-popup--state)
                                              (slash-popup-select-command)
                                            (newline))))
            map))

;;;###autoload
(define-globalized-minor-mode global-slash-popup-mode
  slash-popup-mode
  (lambda () (slash-popup-mode 1)))

(defun slash-popup--inhibit-self-insert ()
  "Intercept keys when popup is active to prevent them from inserting."
  (when (and (alist-get 'active slash-popup--state)
             (eq this-command 'newline))
    (setq this-command 'slash-popup-select-command)))

;; Add needed cleanup function
(defun slash-popup--cleanup-keymap ()
  "Remove the overriding keymap when popup is closed."
  (when (not (alist-get 'active slash-popup--state))
    (setq overriding-terminal-local-map nil)))

;; Add pre-command hook globally to intercept newline
(add-hook 'pre-command-hook #'slash-popup--inhibit-self-insert)



(provide 'slash-popup-commands)
;;; slash-popup-commands.el ends here
