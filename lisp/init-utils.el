
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


(provide 'init-utils)
