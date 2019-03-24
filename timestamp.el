(defvar writestamp-format "%x"
  "*Format for writestamps (c.f. 'format-time-string').")

(defvar writestamp-prefix (concat "WRITESTAMP" "((")
  "*Unique string identifying start of writestamp.")

(defvar writestamp-suffix "))"
  "*String that terminates a writestamp.")

(defun update-writestamps ()
  "Find writestamps and replace them with the current time on the separated line."
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (while (re-search-forward (concat "^" (regexp-quote writestamp-prefix)) nil t)
          (let ((start (point)))
            (re-search-forward (concat (regexp-quote writestamp-suffix) "$")
                               (save-excursion
                                 (end-of-line)
                                 (point)))
            (delete-region start (match-beginning 0))
            (goto-char start)
            (insert (format-time-string writestamp-format (current-time))))))))
  nil)

(add-hook 'local-write-file-hooks 'update-writestamps)

(provide 'timestamp)
