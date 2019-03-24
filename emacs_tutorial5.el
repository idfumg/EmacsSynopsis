;; Some useful Emacs definitions to learn.

;; emacs /usr/local/share/emacs/26.1/lisp/loaddefs.el
;; M-x customize - customize Emcas settings and custom variables.
;; defconst - defines a symbol as constant.
;; defsubst - defines an inline function.
;; C-h C-h for common help.
;; setq-default - sets values only in buffers taht do not have their own values for the variable.
;; compare-windows - compares two opened windows.

(concat "abc" "def")
(substring "The quick brown fox jumped." 16 19)
(+ 11 fill-column)
(concat "The " (number-to-string (+ 11 fill-column)) " foxes")
(+)
(+ 3)
(+ 3 4)
(*)
(* 3)
(* 3 4)
(message "This message appears in the echo area!")
(message "The name of this buffer: %s" (buffer-name))
(message "The value of fill-column is %d." fill-column)

;; Bind a value to the symbol.
(set 'flowers '(rose violet daisy buttercup))

;; Bind a value to the symbol and quote it automatically.
(setq carnivores '(lion tiger leopard))

carnivores

(setq trees '(pine fir oak maple)
      herbivores '(gazelle antelope zebra))
trees
herbivores

;; Get a file name of this buffer in the file system.
(buffer-file-name)

;; Get the buffer name in Emacs.
(buffer-name)

;; Get the current buffer itself (buffer variable).
(current-buffer)

;; Get the most recently switched buffer.
(other-buffer)

;; C-f. Forward char.
(forward-char)

;; C-b. Backward char.
(backward-char)

;; C-e. Forward sentence.
(forward-sentence)

;; Forward line.
(forward-line)

;; C-x b. Switch to the other buffer.
(switch-to-buffer (other-buffer))

;; Switch to the buffer for a  program (new buffer is not visible for human).
(set-buffer (other-buffer))

;; Get buffer size.
(buffer-size)

;; Get current point position value as location from the buffer beginning.
(point)

;; Get minimum possible point in the current buffer.
(point-min)

;; Get maximum possible point in the current buffer.
(point-max)

;; Save the mark at the current position and move point to the beginning of the buffer (C-u C-SPC).
(defun example/beginning-of-buffer()
  (interactive)
  (push-mark)
  (goto-char (point-min)))

;; Save the mark at the current position and move point to the end of the buffer.
(defun example/end-of-buffer()
  (interactive)
  (push-mark)
  (goto-char (point-max)))

;; Mark whole buffer from the beginning to the end.
(defun example/mark-whole-buffer()
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min)))

;; save-excursion saves locations of the point in the current buffer and buffer itself
;;   and restores it later.
;; let* expression garantees that all variables will have declared by order each after another. So latter vars can use of the values to which let* set variables in the earlier part of varlist of let*.
;; B - read the buffer name.
;; r - read the start and the end marks from current buffer.
(defun example/append-to-buffer(buffer start end)
  (interactive "BAppend to buffer: \nr")
  (let ((working-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (barf-if-buffer-read-only)
      (save-excursion
        (insert-buffer-substring working-buffer start end)))))

;; Switch or create to the another window buffer.
(switch-to-buffer-other-window "*Messages*")

;; Switch to the next buffer in the current window.
(switch-to-next-buffer)

;; Switch to the previous buffer in the current window.
(switch-to-prev-buffer)

;; Switch or create a new buffer in the current window.
(switch-to-buffer "asd113")

;; Create a new frame and create a new buffer in it.
(switch-to-buffer-other-frame "emacs_tutorial5.el")

(defun example/copy-to-buffer (buffer start end)
  (interactive "BCopy to buffer: \nr")
  (let ((working-buffer (current-buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (barf-if-buffer-read-only)
      (erase-buffer)
      (save-excursion
        (insert-buffer-substring working-buffer start end)))))

;; * says that there is no error when wokring with a read-only buffer.
;; b says that a user should enter a existing buffer.
(defun example/insert-buffer (buffer)
  (interactive "*bInsert buffer: ")
  (or (bufferp buffer) (setq buffer (get-buffer buffer)))
  (let (start end newmark)
    (save-excursion
      (save-excursion
        (set-buffer buffer)
        (setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark))
  nil)

;; "P" says that Emacs should pass prefix argument to the function (C-u 7 M-x ...).
(defun example/beginning-of-buffer-complicated(&optional arg)
  (interactive "P")
  (message (format "%d" arg)))

(what-line)
(what-page)

;; save-restriction - restores any narrowing if it was exists after evaluating it's body.
;; widen - remove any narrowing if it exists.
(defun example/what-line()
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (message "Line %d" (1+ (count-lines 1 (point)))))))

(defun example/zap-to-char (arg char)
  (interactive "p\ncZap to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input) char)))
  (kill-region (point)
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 (point))))

(last-buffer)
last-command
(set-mark (point))
(numberp 1)
(zerop 0)
(symbolp 'a)


;; defvar - sets the value of a variable if the one doesn't has the value
;;   already (for define a module local variables).
(defvar foo 124)
foo

;; defcustom - for define variables which will be modifying outside the module.
;; Also it's used for checking if a variable value is valid when user
;; uses `set-variable` function.
;; `customize-set-variable` do the same but pay attention to the :set property
;; of a defined variable.


;; LISTS
(setq bouquet '(rose violet buttercup))
(setq flowers (cdr bouquet))
(setq bouquet (cons 'lily bouquet))
(eq (cdr (cdr bouquet)) flowers)

;; Recursion depth:
;; You may want to increase the values ofmax-specpdl-sizeandmax-lisp-eval-depth.

;; Loops

(setq animals '(gazelle giraffe lion tiger))

(defun example/print-elements-of-list (list)
  (while list
    (print (car list))
    (setq list (cdr list))))

(example/print-elements-of-list animals)

(defun example/triangle (number-of-rows)
  (let ((row-number 1)
        (total 0))
    (while (<= row-number number-of-rows)
      (setq total (+ total row-number))
      (setq row-number (1+ row-number)))
    total))

(example/triangle 7)

(defun example/reverse-with-while (list)
  (let (result '())
    (while list
      (setq result (cons (car list) result))
      (setq list (cdr list)))
    result))

(example/reverse-with-while '(1 2 3 4 5))

(defun example/reverse-with-dolist (list)
  (let (result '())
    (dolist (element list result)
      (setq result (cons element result)))))

(example/reverse-with-dolist '(1 2 3 4 5))

(defun example/triangle-with-dotimes (number-of-rows)
  (let ((total 0))
    (dotimes (row-number number-of-rows total)
      (setq total (+ total (1+ row-number))))))

(example/triangle-with-dotimes 7)


(defun example/print-elements-recursively (list)
  (when list
    (print (car list))
    (example/print-elements-recursively (cdr list))))

(example/print-elements-of-list '(1 2 3 4 5 6))

(defun example/triangle-recursively (number-of-rows)
  (if (= number-of-rows 1)
      1
    (+ number-of-rows (example/triangle-recursively (1- number-of-rows)))))

(example/triangle-recursively 7)

(defun example/triangle-with-cond (number)
  (cond
   ((< number 1) 0)
   ((= number 1) 1)
   (t (+ number (example/triangle-with-cond (1- number))))))

(example/triangle-with-cond 7)

(defun example/square-list (list)
  (cond
   ((not list) nil)
   (t (cons (* (car list) (car list)) (example/square-list (cdr list))))))

(example/square-list '(1 2 3 4 5 6))

;; Recursive pattern #1.
(defun example/sum-elements (list)
  (cond
   ((not list) 0)
   (t (+ (car list) (example/sum-elements (cdr list))))))

(example/sum-elements '(1 2 3))

;; Recursive pattern #2.
(defun example/keep-three-letter-words (list)
  (cond
   ((not list) nil)
   ((eq 3 (length (symbol-name (car list))))
    (cons (car list) (example/keep-three-letter-words (cdr list))))
   (t (example/keep-three-letter-words (cdr list)))))

(example/keep-three-letter-words '(one two three four five six))

;; Recursive pattern #3.
(defun example/seq-sum (number)
  (defun example/seq-sum-helper (current total result)
    (cond
     ((> current total) result)
     (t (example/seq-sum-helper (1+ current) total (+ result current)))))

  (example/seq-sum-helper 0 number 0))

(example/seq-sum 7)


;; Regexp searching.
(sentence-end)
sentence-end-without-period
sentence-end-without-space

(defun example/forward-sentence (&optional arg)
  (interactive "p")
  (or arg (setq arg 1))
  (let ((opoint (point))
        (sentence-end (sentence-end)))
    (while (> arg 0)
      (let ((par-end (save-excursion (end-of-paragraph-text) (point))))
        (if (re-search-forward sentence-end par-end t)
            (skip-chars-backward " \t\n")
          (goto-char par-end)))
      (setq arg (1- arg)))
    (constrain-to-field nil opoint t)))


;; M-} - forward paragraph.
(example/forward-sentence)
(forward-paragraph)
;; asadasd. adasdad
;; asdasd. qwe
;; asad.


;; `r` - it will cause Emacs to pass the beginning and end of the region to the function args.
(defun example/count-words-region (beginning end)
  (interactive "r")
  (save-excursion
    (goto-char beginning)
    (message "beginning = %d, end = %d" beginning end)
    (let ((count 0))
      (while (and (< (point) end) (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))
      (cond
       ((zerop count) (message "The region does not have any words."))
       ((= count 1) (message "The region has 1 word."))
       (t (message "The region has %d words." count))))))

(example/count-words-region (point-min) (point-max))

(defun example/count-words-region-recursive (beginning end)
  (interactive "r")
  (save-excursion
    (goto-char beginning)
    (let ((count (helper end 0)))
      (cond
       ((zerop count) (message "The region does not have any words."))
       ((= count 1) (message "The region has 1 word."))
       (t (message "The region has %d words." count)))))

  (defun helper (end result)
    (cond
     ((>= (point) end) result)
     ((not (re-search-forward "\\w+\\W*" end t)) result)
     (t (helper end (1+ result))))))

(example/count-words-region-recursive (point-min) (point-max))

(custom-set-variables '(eval-expression-print-length nil))

(defun example/count-words-in-defun ()
  (interactive)
  (beginning-of-defun)
  (let ((count 0)
        (search-pattern "\\(\\w\\|\\s_\\)+[^ \t\n]*[ \t\n]*")
        (end (save-excursion (end-of-defun) (point))))
    (while (and (< (point) end) (re-search-forward search-pattern end t))
      (setq count (1+ count)))
    (message "%d" count)
    count))

(defun example/lengths-list-file (filename)
  (message "Working on `%s' ..." filename)
  (save-excursion
    (let ((buffer (find-file-noselect filename))
          (result '()))
      (with-current-buffer buffer
        (save-restriction
          (setq buffer-read-only t)
          (widen)
          (goto-char (point-min))
          (while (re-search-forward "^(defun" nil t)
            (setq result (cons (example/count-words-in-defun) result))))
        (kill-buffer buffer))
      result)))

(example/lengths-list-file "emacs_tutorial6.el")

(defun example/lengths-list-files (filenames)
  (let ((result '()))
    (while filenames
      (setq result (append result (example/lengths-list-file
                                   (expand-file-name (car filenames)))))
      (setq filenames (cdr filenames)))
    result))

(example/lengths-list-files '("emacs_tutorial6.el" "emacs_tutorial4.el"))
(message (expand-file-name "emacs_tutorial6.el"))

(defun example/lengths-list-files-recursive (filenames)
  (if filenames
      (append
       (example/lengths-list-file (expand-file-name (car filenames)))
       (example/lengths-list-files-recursive (cdr filenames)))))

(example/lengths-list-files-recursive '("emacs_tutorial6.el" "emacs_tutorial4.el"))

(sort '(4 5 8 1 9 0 4 2 1) '<)

(sort
 (example/lengths-list-files
  '("emacs_tutorial6.el"
    "emacs_tutorial4.el"
    "emacs_tutorial3.el"
    "emacs_tutorial2.el"
    "emacs_tutorial1.el"))
 '<)

(directory-files ".")

(defun example/files-in-directory (directory)
  (interactive "DDirectory name: ")

  (let ((result '())
        (files (directory-files-and-attributes directory t)))

    (while files
      (let* ((current-file (car files))
             (filename (car current-file))
             (directory? (eq t (cdr current-file)))
             (dot? (equal "." (substring filename -1)))
             (el-file? (equal ".el" (substring filename -3))))
        (cond

         (el-file?
          (setq result (cons filename result)))

         (directory?
          (if dot?
              nil
            (setq result
                  (append (example/files-in-directory filename) result))))))

      (setq files (cdr files)))
    result))

(example/files-in-directory ".")

(sort (example/files-in-directory ".") 'string-lessp)

(defvar examples/top-of-ranges
  '(
    10   20  30  40  50
    60   70  80  90 100
    110 120 130 140 150
    160 170 180 190 200
    210 220 230 240 250
    260 270 280 290 300
    ))


;; M-x debug-on-entry -> example/seq-sum
;; d - for nex expression
(example/seq-sum 3)


;; Working with property lists
(defun myfunc (&rest args)
  (let ((first (plist-get args :first))
        (second (plist-get args :second)))
    (list first second)))

(myfunc :first 1 :second 2)
