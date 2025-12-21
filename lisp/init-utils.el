
;; generate random string
(defun generate-password (length)
  "Generate a random password of the specified LENGTH."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()_+"))
    (apply #'string (loop repeat length collect (elt chars (random (length chars)))))))

;; random a password
(defun random-password ()
  "Random a password string."
  (interactive)
  (let ((input-number (read-number "Length: " 16)))
    (insert (format "%s" (generate-password input-number)))))


;;; Delayed execution for low-priority configurations
;; Inspired by Kyure-A/.emacs.d - queues configurations to run after startup

(defvar my/delayed-priority-low-configurations nil
  "List of low-priority configuration forms to execute after startup.")

(defvar my/delayed-configuration-timer nil
  "Timer for processing delayed configurations.")

(defmacro with-delayed-execution (&rest body)
  "Queue BODY to be executed after Emacs startup completes.
Use this for non-critical configurations that don't need to run during init."
  `(setq my/delayed-priority-low-configurations
         (append my/delayed-priority-low-configurations ',body)))

(defun my/process-delayed-configurations ()
  "Process one delayed configuration from the queue."
  (when my/delayed-priority-low-configurations
    (condition-case err
        (eval (pop my/delayed-priority-low-configurations))
      (error (message "Error in delayed config: %S" err))))
  ;; Cancel timer when queue is empty
  (unless my/delayed-priority-low-configurations
    (when my/delayed-configuration-timer
      (cancel-timer my/delayed-configuration-timer)
      (setq my/delayed-configuration-timer nil)
      (message "All delayed configurations processed."))))

;; Start processing delayed configurations 0.3 seconds after startup
;; Process one item every 1ms to avoid blocking
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq my/delayed-configuration-timer
                  (run-with-timer 0.3 0.001 #'my/process-delayed-configurations))))


(provide 'init-utils)
