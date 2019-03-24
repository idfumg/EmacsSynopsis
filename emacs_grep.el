(defun xx ()
  (interactive)
  (let (findStr dirX filenameRegex fileList)
    (setq findStr (read-string "find string regex: "))
    (setq dirX (read-directory-name "dir to search: "))
    (setq filenameRegex (read-string "file name regex: "))

    (setq fileList
          (directory-files-recursively dirX filenameRegex))

    (message "%s" findStr)
    (message "%s" dirX)
    (message "%s" filenameRegex)
    (message "%s" fileList)

    ;; (mapc
    ;;  (lambda (x)
    ;;    (message "%s" x))
    ;;  fileList)

    (mapc
     (lambda (x)
       (with-temp-buffer
         (insert-file-contents x)
         (goto-char 0)
         (when (re-search-forward findStr nil t)
           (print
            (buffer-substring-no-properties
             (line-beginning-position)
             (line-end-position))))))
     fileList)

    ))
