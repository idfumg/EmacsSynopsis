(defvar refill-mode nil
  "Mode variable for refill minor mode.")

(make-variable-buffer-local 'refill-mode)

(defun refill-mode (&optional arg)
  "Refill minor mode."
  (interactive "P")

  (setq refill-mode (if (null arg)
                        (not refill-mode)
                      (> (prefix-numeric-value arg) 0)))

  (make-local-variable 'after-change-functions)

  (if refill-mode
      (progn
        (add-hook 'after-change-functions 'refill nil t)
        (message "refill-mode enabled"))
    (progn
      (remove-hook 'after-change-functions 'refill t)
      (message "refill-mode disabled"))))

(if (not (assq 'refill-mode minor-mode-alist))
            (setq minor-mode-alist (cons '(refill-mode " Refill") minor-mode-alist)))

(defun refill (start end len)
  "After a text change, refill the current paragraph *"
  (message "!")
  (let* ((insertion? (zerop len))
         (left (if insertion?
                   start
                 (save-excursion
                   (goto-char start)
                   (beginning-of-line 0)
                   (point)))))

    (fill-region left end nil nil t)))

(provide 'refill)
