(defalias 'scroll-ahead 'scroll-down)
(defalias 'scroll-behind 'scroll-up)
(defalias 'scroll-ahead-window 'scroll-down-command)
(defalias 'scroll-behind-window 'scroll-up-command)

(defun scroll-one-line-ahead ()
  (interactive)
  (scroll-ahead 1))

(defun scroll-one-line-behind ()
  (interactive)
  (scroll-behind 1))

(defun scroll-n-lines-ahead (&optional n)
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))

(global-set-key [?\C-p] 'previous-line)
(global-set-key [?\C-n] 'next-line)
(global-set-key [?\M-p] 'scroll-n-lines-ahead)
(global-set-key [?\M-n] 'scroll-n-lines-behind)
(global-set-key [?\C-\M-p] 'scroll-ahead-window)
(global-set-key [?\C-\M-n] 'scroll-behind-window)

(defun read-only-if-symlink ()
  (when (file-symlink-p buffer-file-name)
    (progn
      (setq buffer-read-only t)
      (message "File is a symlink"))))

(add-hook 'find-file-hooks 'read-only-if-symlink)

(yes-or-no-p "yes or no? ")

;; define callback `before` or `after` function call
;; name of the callback
;; its active (will be called from now) or inactive (will not)
;; it's need to automatically compile this callback or not
(defadvice helm-buffers-list (before only-existing-buffer activate compile)
  (interactive
   (list
    (read-buffer "Switch to buffer: " (other-buffer) (null current-prefix-arg)))))

(defvar unscroll-point (make-marker)
  "Text position for next call to 'unscroll'.")

(defvar unscroll-window-start (make-marker)
  "Window start for next call to 'unscroll'.")

(defvar unscroll-horizontal-scroll nil
  "Text horizontal for next call to 'unscroll'.")

;; set properties to the Emacs symbols
(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)

(defun unscroll-remember-if-need ()
  (when (not (get last-command 'unscrollable))
    (set-marker unscroll-window-start (window-start))
    (set-marker unscroll-point (point))
    (setq unscroll-horizontal-scroll (window-hscroll))))

(defadvice scroll-up (before remember-for-unscroll activate compile)
  (unscroll-remember-if-need))

(defadvice scroll-down (before remember-for-unscroll activate compile)
  (unscroll-remember-if-need))

(defadvice scroll-left (before remember-for-unscroll activate compile)
  (unscroll-remember-if-need))

(defadvice scroll-right (before remember-for-unscroll activate compile)
  (unscroll-remember-if-need))

(defun unscroll ()
  (interactive)
  (when (not unscroll-point)
    (error "Cannot unscroll"))
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-horizontal-scroll))

(defun insert-current-time ()
  (interactive "*")
  (insert (current-time-string)))

(format-time-string "%H.%M" (current-time))


;; * - user options and can be set by M-x set-variable
;; \\[function] - used for replacing the keybinding that invokes command
(defvar insert-time-format "%X"
  "*Format for \\[insert-time] (c.f. 'format-time-string').")

(defvar insert-date-format "%x"
  "*Format for \\[insert-date] (c.f. 'format-time-string').")

(defvar writestamp-format "%x"
  "*Format for writestamps (c.f. 'format-time-string').")

(defvar writestamp-prefix (concat "WRITESTAMP" "((")
  "*Unique string identifying start of writestamp.")

(defvar writestamp-suffix "))"
  "*String that terminates a writestamp.")

;; * - means abort this function if the current buffer is read-only
(defun insert-time ()
  "Insert the current time according to insert-time-format."
  (interactive "*")
  (insert (format-time-string insert-time-format (current-time))))

(defun insert-date ()
  "Insert the current date according to insert-date-format."
  (interactive "*")
  (insert (format-time-string insert-date-format (current-time))))

(add-hook 'local-write-file-hooks 'update-writestamps)

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

;; (defun update-writestamps ()
;;   "Find writestamps and replace them with the current time on the separated line."
;;   (save-excursion
;;     (save-restriction
;;       (save-match-data
;;         (widen)
;;         (goto-char (point-min))
;;         (while (re-search-forward (concat "^"
;;                                           (regexp-quote writestamp-prefix)
;;                                           "\\(.*\\)"
;;                                           (regexp-quote writestamp-suffix)
;;                                           "$"))
;;           (replace-match (format-time-string writestamp-format (current-time)) t t nil 1)))))
;;   nil)


WRITESTAMP((19.03.2019))


(defun flatten (lst)
  (if (null lst)
      nil
    (if (listp (car lst))
        (append (flatten (car lst)) (flatten (cdr lst)))
      (cons (car lst) (flatten (cdr lst))))))

(flatten '(a ((b) c) d))

(defmacro limited-save-excursion (&rest subexprs)
  "Like save-excursion, but only restores point."
  '(let* ((orig-point (point))
          (result (progn ,@subexprs)))
     (goto-char orig-point)
     result))

(defmacro limited-save-excursion (&rest subexprs)
  "Like save-excursion, but only restores point."
  (let ((orig-point-symbol (make-symbol "orig-point")))
    '(let* ((,orig-point-symbol (point))
            (result (progn ,@subexprs)))
       (goto-char ,orig-point-symbol)
       result)))

(defmacro limited-save-excursion (&rest subexprs)
  "Like save-excursion, but only restores point."
  (let ((orig-point-symbol (make-symbol "orig-point")))
    '(let ((,orig-point-symbol (point-marker)))
       (unwind-protect
           (progn ,@subexprs)
         (goto-char ,orig-point-symbol)))))
