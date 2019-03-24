(defun system-is-linux-p ()
  (string-equal system-type "gnu/linux"))

(defun system-is-windows-p ()
  (string-equal system-type "windows-nt"))
