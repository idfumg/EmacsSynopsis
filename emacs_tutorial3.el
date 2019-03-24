;; Run stack trace of the function for debug.
(defun example/debug ()
  (if t
      (progn
        (debug)
        (message "Hello, world!"))
    (message "unimplemented")))

(example/debug)

;; Stop program and show an error.
(defun example/error ()
  (if nil
      (message "Hello, world!")
    (error "unimplemented")))

(example/error)

;; Define a variable and print it.
(defun example/let ()
  (let ((var "My custom variable"))
    (message "%s" var)))

(example/let)

;; Allows use of M-x.
(defun example/interactive (directory)
  (interactive "DDirectory name: ")
  (message "You wrote: %s" directory))

;; Allows input two and more functions arguments.
(defun example/interactive2 (directory1 directory2)
  (interactive "DDirectory name: \nDNext directory name: ")
  (message "You wrote: %s %s" directory1 directory2))

;; C-h v
;; Describe variable.
buffer-file-name

;; Print the current point.
(message "%s" (point))

;; Jump to the next word.
(forward-word)

;; Declare global variable, but only once.
(defvar global-var "It's a global variable")
(defun example/declare-variable ()
  (message "%s" global-var))

(example/declare-variable)

;; Common Lisp loop.
(defun example/loop ()
  (cl-loop for i from 0 to 9 collect i))

(example/loop)

;; Common Lisp reverse loop.
(defun example/reverse-loop ()
  (reverse (cl-loop for i from 0 to 9 collect i)))

(example/reverse-loop)

;; Common Lisp reverse loop in place (without create a new one).
(defun example/reverse-loop-in-place ()
  (nreverse (cl-loop for i from 0 to 9 collect i)))

(example/reverse-loop-in-place)

;; How lists represent.
;; (1 . (2 . (3 . nil))) == (1 2 3)

;; Cons add element (or list) to the list.
(defun example/cons ()
  (let ((x (cl-loop for i from 0 to 9 collect i)))
    (cons (reverse x) x)))

(example/cons)

;; insert text where cursor is.
(insert "Hello!")
(insert "Hello, " "world!")

(defun example/insert (name)
  (insert (format "\nHello, %s!" name)))

(example/insert "idfumg")

;; Switch buffers.
(defun example/switch-buffer (name)
  (switch-to-buffer-other-window name)
  (erase-buffer)
  (insert "It's a " name " buffer")
  (other-window 1))

(example/switch-buffer "*test*")

;; Read from minibuffer.
(defun example/read-from-minibuffer ()
  (let ((name (read-from-minibuffer "Enter your name: ")))
    (message (format "You entered: %s" name))))

(example/read-from-minibuffer)

;; Insert c++ namespace.
(defun example/insert-namespace (name)
  (interactive "SEnter namespace name: ")
  (let ((value (format "namespace %s {\n\n} // namespace %s" name name)))
    (insert value)))

(example/insert-namespace "rail")

;; Insert c++ namespace.
(defun example/insert-class (name)
  (interactive "SEnter class name: ")
  (let ((value (format "class %s {\npublic:\n\n};" name name)))
    (insert value)
    (backward-char)
    (backward-char)
    (backward-char)
    (insert "    ")))

(example/insert-class "rail")

;; Define a list.
(setq list-of-names '("Sarah" "Chloe" "Mathilde"))

;; Get the first element of the list.
(car list-of-names)

;; Get the list of all but the first element.
(cdr list-of-names)

;; Add an element to the beginning (modify the list).
(push "Stephanie" list-of-names)

;; Call example/insert of each element of the list.
(mapcar 'example/insert list-of-names)

;; Call hello for each person in the list in other window.
(defun example/hello-in-other-window (buffer-name names)
  (switch-to-buffer-other-window buffer-name)
  (erase-buffer)
  (mapcar (lambda (name) (insert (format "Hello, %s!\n" name))) names)
  (other-window 1))

(example/hello-in-other-window "test" list-of-names)

;; Modify Hello in second buffer.
(defun example/replace-hello (name)
  (switch-to-buffer-other-window name)
  (goto-char (point-min))
  (while (search-forward "Hello" nil t)
    (replace-match "Bonjour"))
  (other-window 1))

(example/replace-hello "test")

;; Modify Hello in second buffer and make it bold.
(defun example/replace-hello-bold (name)
  (switch-to-buffer-other-window name)
  (goto-char (point-min))
  (while (re-search-forward "Bonjour, \\(.+\\)!" nil t)
    (add-text-properties (match-beginning 1)
                         (match-end 1)
                         '(face bold)))
  (other-window 1))

(example/replace-hello-bold "test")

;; Strings matches.
(string-match "" "") ;; O(n)
(string-equal "" "") ;; O(n)
(equal "" "") ;; O(1)
(zerop (length "")) ;; O(1)
(eq "" "") ;; O(1) - compares object, not strings.

;; Reverse string.
(s-reverse "ab xyz")

;; Convert string to character list, reverse and convert to string.
(string-to-list "foo")
(reverse (string-to-list "foo"))
(apply 'string (reverse (string-to-list "foo")))

;; Looking for characters in buffer.
(with-temp-buffer
  (insert "abcdefg")
  (goto-char (point-min))
  (while (not (= (char-after) ?g))
    (forward-char))
  (point))

;; Trim example.
(s-trim "  this  ")

(defun example/trim (name)
  (while (string-match "\s+" name)
    (setq name (replace-match "" nil nil name)))
  name)

(example/trim "  this  ")

;; Split strings.
(split-string "1 thing 2 say 3 words 4 you" "[0-9]+" t)
(mapcar 's-trim (split-string "1 thing 2 say 3 words 4 you" "[0-9]+" t))

;; Join strings.
(s-join "/" '("" "home" "idfumg" "work"))
(mapconcat 'identity '("" "home" "idfumg" "work") "/")

;; Serialization.
(read (prin1-to-string "Hello, world!"))

;; Kill line analog.
(defun example/kill-line1 ()
  (let ((beg (point)))
    (forward-line 1)
    (forward-char -1)
    (delete-region beg (point))))

(defun example/kill-line2 ()
  (delete-region (point)
                 (save-excursion
                   (forward-line 1)
                   (forward-char -1)
                   (point))))

(defun example/kill-line3 ()
  (delete-region (point) (line-end-position)))

(example/kill-line1)
(example/kill-line2)
(example/kill-line3)

(defun camelCase-to_underscores (start end)
  "Convert any string matching something like aBc to a_bc"
  (interactive "r")
  ;; (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "\\([a-z]\\)\\([A-Z]\\)\\([a-z]\\)" nil t)
        (replace-match (concat (match-string 1)
                               "_"
                               (downcase (match-string 2))
                               (match-string 3))
                       t nil))))
