;; ielm - lisp interpretator
(defun func-name () ())
(require 'ert) ; elisp testing library
(ert-deftest func-name () (should (predicate)))
;; M-x ert-run-tests

(setq my-var 90 my-other-var 10)
(null nil)
(null t)
'(1 2 3 4)
(null '())
(equal nil '())
(setq my-list '(1 2 3))
(add-to-list 'my-list 4) ; modify the list
(cons 5 my-list) ; creates new list
(car my-list) ; first element of the list
(cdr my-list) ; remaining of the list without first element
(nth 2 my-list) ; returns nth list element
(member 2 my-list) ; checks if element belongs to list (returns sublist)

(defun does-exists (number some-list)
  (not (null (member number some-list))))

(ert-deftest check-number ()
  (should (not (does-exists 5 '(1 2 3))))
  (should (does-exists 45 '(45 30))))

(dolist (elem L)
  (when (= (mod elem 2) 0)
    (print elem)))

;; create local scope variable
(let (some-var)
  (when (null some-var)
    (message "var is null")))

(let ((some-var 0)
      (other-var 90))
  (if (null some-var)
      (message "NOT SUPPOSE TO HAPPEN")
    (message "Yey")))

(setq L '(1 2 3 4))
(defun sum-evens (some-list)
  (let ((sum 0))
    (dolist (element some-list)
      (when (= (mod element 2) 0)
        (setq sum (+ sum element))))
    sum))
(sum-evens L)

(ert-deftest sum-evens-test ()
  (should (= (sum-evens '(1 3 5)) 0))
  (should (= (sum-evens '(2 4 6)) 12))
  (should-not (= (sum-evens '(2 4 6)) 13)))

;; C-h f help for functions
(defun cheap-count-words ()
  (interactive) ;; Allows M-x
  (let ((words 0))
    (save-excursion
      (goto-char (point-min))
      (while (forward-word)
        (setq words (1+ words))))
    (message (format "Words in buffer: %s" words))
    words))

(require 'ert)

(ert-deftest count-words-test ()
  (get-buffer-create "*test*")
  (with-current-buffer "*test*"
    (erase-buffer)
    (insert "Hello world")
    (should (= (cheap-count-words) 2)))
  (kill-buffer "*test*"))
