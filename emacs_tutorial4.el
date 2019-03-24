;; Define macro. Its will replace macro name with macro body.
;; Its not a function but works as a function.
(defmacro example/macro1 (name)
  name)

;; Define function. Its will execute its body end return name as an argument.
(defun example/func1 (name)
  name)

;; Expand macro, returning its body with replaceing in compile time.
(macroexpand '(example/macro1 123))

;; Expands ans (name name) - as a function `name` with an argument `name`.
;; Next, when interpretator will work, it'll try to call list function `name`.
;; If that function wasn't defined, we will get an error.
(defmacro example/macro2 (name)
  '(name name))

(macroexpand '(example/macro2 123)) ;; => (name name)
(example/macro2 123) ;; => invalid function `name`

;; ` - using when we want return some template in macro.
;; , - using in template when we dont want using `name` as symbol but want to
;;     use the appropriate name value which was sent to the macro (123 123).
;; If that function 123 wasn't defined we will get an error.
(defmacro example/macro3 (name)
  `(,name ,name))

(macroexpand '(example/macro3 123)) ;; => (123 123)
(example/macro3 123) ;; => invalid function `123`

;; We try to use macro for getting a list of the same value of `name`.
(defmacro example/macro4 (name)
  `(list ,name ,name))

(macroexpand '(example/macro4 123)) ;; => (list 123 123)
(example/macro4 123) ;; => '(123 123)


;; Using &rest keyword for getting a list of variable lenght arguments.
(defmacro example/macro5 (&rest body)
  body)

(macroexpand '(example/macro5 1 2 3)) ;; => (1 2 3)
(example/macro5 1 2 3) ;; => invalid function `1`

;; We try to get list of arguments, not function call.
;; But we get list of list. Fix that in the next example.
(defmacro example/macro6 (&rest body)
  `(list ,body))

(macroexpand '(example/macro6 1 2 3)) ;; => (list (1 2 3))

;; Try to get list of arguments, not a function call.
;; So, the @ operator remove outer list and leave only his values.
(defmacro example/macro7 (&rest body)
  `(list ,@body))

(macroexpand '(example/macro7 1 2 3)) ;; => (list 1 2 3)
(example/macro7 1 2 3) ;; => (1 2 3)

;; Some example of macro - define two variables with an appropriate values.
(defmacro example/macro8 (&rest body)
  (let ((res nil))
    (dolist (elem body)
      (setq res (cons (list 'setq (nth 1 elem) (nth 0 elem))
                      res)))
    `(progn ,@res)))

(macroexpand '(example/macro8 (1 a) (2 b))) ;; => (progn (setq a 1) (setq b 2))
(example/macro8 (1 a) (2 b)) ;; => 1

;; emacs development tools
(require 'cedet)

;; Working with eieio and classes for OOP.
;; We must define some slots.
(defclass HelloWorld ()
  ((user :initarg :user)))

;; Create object with name `123` and field user `idfumg`.
(setq h (HelloWorld "123" :user "idfumg"))

;; Get object field value by field name.
(oref h user)

;; Set field value by field name and value.
(oset h user "Pinguin")

;; Define class method.
(defmethod hello ((this HelloWorld))
  (message (concat "Hello, " (oref this user))))

(hello h)
