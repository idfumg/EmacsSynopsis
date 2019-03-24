н
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mystring "my string")
(message "%c" (aref mystring 1))
(aset mystring 1 ?x)
(message "%c" (aref mystring 1))
(string-width mystring)

(equal 97 ?a)
(equal 97 (string-to-char "a"))
(equal ?a (string-to-char "a"))

(make-string 10 ?a)
(make-vector 10 ?a)
(make-list 10 ?a)
(make-symbol "newsymbol")

(string ?a ?b ?c)

(substring "abcdefg" 0 3)
(substring "abcdefg" -3 -1)
(substring "abcdefg" -3)
(substring "abcdefg" -3 nil)
(substring [1 2 3 4] -3 nil)

(concat "abc" "def")
(concat [1 2 3] [4 5 6])
(concat)

(split-string "  two words ")
(split-string "  two words " split-string-default-separators)
(split-string "  two/words /" "/")
(split-string "  two/words /" "/" t)
(split-string "" "")
(split-string "" "" t)

;; Clears content of the string to zeros for security (if password in it)
(setq mystring "my string")
(clear-string mystring)
mystring

(char-equal ?x ?X)
(let ((case-fold-search nil)) (char-equal ?x ?X))
(eq "abc" "abc") ;; compare two objects (not actual memory data)
(string-equal "abc" "abc") ;; compare actual memory data of strings
(string= "abc" "abc") ;; the same
(string< "abc" "def") ;; lexicographically
(string-lessp "abc" "def") ;; the same
(string> "abc" "def") ;; lexicographically
(string-greaterp "abc" "def") ;; the same
(string-version-lessp "foo2.png" "foo12.pg") ;; compares lexicographically but numbers
(string-prefix-p "abc" "abcdefg") ;; starts with
(string-suffix-p "efg" "abcdefg") ;; ends with
(compare-strings "abcdefg" 5 6 "efghkl" 0 2)
(compare-strings "abc" nil nil "abd" nil nil)
(compare-strings "abc" nil nil "abD" nil nil t) ;; t = ignore case
(assoc-string "abc" '("qwe" "abc" "asd"))
(assoc-string "abc" '("qwe" "ABC" "asd") t) ;; upper case before search

(downcase "QAWEASD12313`&^%#$")
(downcase ?X)
;; (downcase-region beg end)
(upcase "asdqwe!&%^123")
(capitalize "asdasd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(consp '(1 2 3))
(consp nil)
(atom nil)
(atom t)
(atom '1)
(listp '(1))
(null '()) ;; used for lists objects (`not` used for boolean values)
(car '(a b c))
(cdr '(a b c))
(car-safe 'qwe) ;; car for list or nil for other objects
(cdr-safe 'qwe) ;; cdr for list or nil for other objects
(setq L '(1 2 3))
(pop L) ;; pops first element and assign cdr for the variable
(nth 1 '(1 2 3))
(nthcdr 2 '(1 2 3 4 5))
(last '(1 2 3 4))
(length '(1 2 3 4))
(caar '((1 2) 3 4 5))
(cadr '((1 2) 3 4 5))
(cdar '((1 2) 3 4 5))
(cddr '((1 2) 3 4 5))
(butlast '(1 2 3 4 5))
(butlast '(1 2 3 4 5) 2)
(butlast '(1 2) 2)

(cons 1 '(2))
(cons 1 '())
(cons 1 2)
(cons 1 '(2 3 4))
(list 1 2 3 4)
(list 1 '(2 3 4))
(list)
(append '(1) '(2 3))
(append '(1) 2)
(append)
(append '(a b c) '(x y z)) ;; for several lists
(apply 'append '((a b c) (x y z))) ;; for list of lists
(copy-tree '((1 2 3 (4 5 (6 7))))) ;; recursively copying of the list
(number-sequence 0 9)
(number-sequence 0 9 2)
(number-sequence 9 0 -2)

(setq L '(1 2 3))
(push 0 L) ;; (setq L (cons 0 L))

(setq L '(1 2 3))
(add-to-list 'L 0)
(add-to-list 'L 3) ;; no effect 'cause 3 already exists in the list L
(member 3 L)
(member 4 L)

(setq L '(1 2 3))
(setcar L 0)
L

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists as Sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-dups '(1 2 2 3 4 4 5)) ;; destructive

(memq 3 '(1 2 2 3 4 4 5)) ;; is it member? (it is uses `eq`)
(memql 3 '(1 2 2 3 4 4 5)) ;; is it member? (it is uses `eql` for floating-point)
(member 3 '(1 2 2 3 4 4 5)) ;; is it member? (it is uses `equal`)

(delq 3 '(1 2 2 3 4 4 5)) ;; destructive - delete element from the list (it is uses `eq`)
(delete 3 '(1 2 2 3 4 4 5)) ;; delete element from the list destructive (it is uses `equal`)
(remq 3 '(1 2 2 3 4 4 5)) ;; non-destructive (returns a copy) (it is uses `eq`)
(remove 3 '(1 2 2 3 4 4 5)) ;; remove element from the list non-destructive (it is uses `equal`)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Association lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
(assoc 'oak trees) ;; compares with equal
(cdr (assoc 'oak trees))
(rassoc 'cones trees)
(assq 'oak trees) ;; compares with eq
(alist-get 'oak trees) ;; return value by comparing with eq (only objects addresses)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plist-get '(foo 4) 'foo)
(plist-put '(foo 4) 'bar 7) ;; it provides uniqueness
(plist-member '(foo 4) 'foo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sequencep '(1 2 3))
(sequencep [1 2 3])
(sequencep "123")

(length '(1 2 3))
(length [1 2 3])
(length "123")

(elt [1 2 3] 1) ;; constant time

(copy-sequence [1 2 3]) ;; copy the sequence with the same type

(reverse [1 2 3])

(sort [4 1 6 0 8] '<)

(seq-length "123")
(seq-drop [1 2 3 4 5] 3)
(seq-take [1 2 3 4 5] 3)
(seq-drop-while (lambda (x) (< x 0)) [-1 -2 -3 4 5])
(seq-take-while (lambda (x) (< x 0)) [-1 -2 -3 4 5])
(seq-do (lambda (x) (- x)) [-1 -2 -3 4 5])
(seq-map '1+ [-1 -2 -3 4 5])
(seq-filter (lambda (x) (> x 0)) [-1 -2 -3 4 5])
(seq-remove (lambda (x) (< x 0)) [-1 -2 -3 4 5])
(seq-reduce '+ [-1 -2 -3 4 5] 0)
(seq-some #'numberp ["abc" 1 nil])
(seq-some #'numberp ["abc" "1"])
(seq-some #'null ["a" 1 nil])
(seq-find #'numberp ["abc" 1 nil 2])
(seq-every-p #'numberp [1 2 3])
(seq-empty-p [1 2 3])
(seq-empty-p nil)
(seq-count (lambda (x) (> x 0)) [-1 -2 -3 4 5])
(seq-sort #'> [-1 -2 -3 4 5])
(seq-sort-by #'seq-length #'> ["a" "ab" "abc"])
(seq-contains ["a" "ab" "abc"] "ab")
(seq-set-equal-p [a b c] [c a b]) ;; compare regardless of the elements order
(seq-position [a b c] 'b)
(seq-uniq [1 2 3 2 1 5 3 4]) ;; returns unique elements with the saved order
(seq-subseq [1 2 3 4 5] 1)
(seq-subseq [1 2 3 4 5] 1 -1)
(seq-concatenate 'list [1 2 3] [4 5] [6])
(seq-mapcat #'seq-reverse [[3 2 1] [6 5 4]])
(seq-partition '(0 1 2 3 4 5 6 7) 3) ;; returns grouped elements by length
(seq-intersection [1 2 3 4] [3 4 5 6])
(seq-difference [1 2 3 4] [3 4 5 6]) ;; returns elements from first list which differs
(seq-group-by #'integerp [1 2.1 3 2 3.2])
(seq-group-by (lambda (x) (elt x 0)) [[a 1] [a 2] [b 3] [c 4]])
(seq-into [1 2 3] 'list)
(seq-into '(1 2 3) 'vector)
(seq-into [?1 ?2 ?3] 'string)
(seq-min [3 2 1 4 -1])
(seq-min "qwea")
(seq-max "qwea")
(seq-doseq (item "abc") (message (char-to-string item))) ;; works with list, vector, string

;; destructuring like in the clojure :]
(seq-let [first second] [1 2 3 4]
  (list first second))
(seq-let [_ third _ fourth] [1 2 3 4]
  (list third fourth))
(seq-let [a b &rest others] [1 2 3 4]
  others)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Arrays in Emacs Lisp are: string, vector, bool-vector, char-table
;; They major difference with lists is a constant access time.
;; You can not change length of array after creating it.

;; aref - access an array element
;; aset - change an array element

(arrayp "asd")
(arrayp [1 2 3])
(aref "asd" 1)
(aset "asd" 1 ?b)
(fillarray "abc" ?0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creating a vector does not evaluate it's elements and makes it as is.

(vectorp [1 2 3])
(vector 1 2 "3" "4" nil)
(make-vector 9 'Z)
(vconcat '(1 2 3) [4 5] "67") ;; concatinate elements of sequences
(append [1 2 3]) ;; contans list and therefore can make list from vector
(setq myvector [1 2 3])
(aset myvector 1 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bool-vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Specific container for effective working with the bool values.

(setq my-bool-vector1 (bool-vector nil t nil t))
(setq my-bool-vector2 (bool-vector t nil t nil))
(make-bool-vector 9 nil)
(bool-vector-p my-bool-vector1)
(bool-vector-exclusive-or my-bool-vector1 my-bool-vector2)
(bool-vector-union my-bool-vector1 my-bool-vector2)
(bool-vector-intersection my-bool-vector1 my-bool-vector2)
(bool-vector-set-difference my-bool-vector1 my-bool-vector2)
(bool-vector-not my-bool-vector1)
(bool-vector-subsetp my-bool-vector1 my-bool-vector2) ;; if every t in a is t in b
(bool-vector-count-consecutive my-bool-vector1 nil 0) ;; conseq elements of nil from 0
(vconcat my-bool-vector1) ;; can be used for printing bool vectors
(aref my-bool-vector1 1)
(aset my-bool-vector1 1 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq temphash (make-hash-table
 :test 'equal
 :size 128))
(clrhash temphash)
(setq myhash #s(hash-table data (:a 1 :b 2)))
(gethash :a myhash)
(gethash :c myhash -1)
(puthash :q 9 myhash)
(remhash :q myhash)
(clrhash myhash)
(maphash (lambda (name value) (message "%s %s" name value)) myhash)
(hash-table-p myhash)
(copy-hash-table myhash) ;; keys and values are shared (only hash table are would copied)
(hash-table-count myhash)
(hash-table-test myhash)
(hash-table-size myhash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mysymbol 'foo)
(setq mysymbol-uninterned (make-symbol "foo")) ;; new allocated symbol
(symbolp 'a)
(symbolp 'split-string)
(symbol-name 'a)
(symbol-function '+)
(symbol-value mysymbol)
(symbol-plist mysymbol)
(eq mysymbol mysymbol-uninterned)
(gensym) ;; generate symbol by appending counter to it
(gensym 'foo)
(intern "foo") ;; creates a symbol in the global obarray of program
(eq (intern "foo") mysymbol) ;; true 'cause it in the global program obarray
(intern-soft "foo") ;; it checks if this symbol exists in the obarray
(unintern "foo")
(intern-soft "foo")

;; Each symbol can contains some random properties with values
(put 'a 'first-property 'value1)
(put 'a 1 'value2)
(put 'a :someprop 'value3)
(get 'a 'first-property)
(get 'a 1)
(get 'a :someprop)
(symbol-plist 'a)
(setplist 'a '(:a 1 :b 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backquote
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mylist '(1 2 3))
'(a list of (+ 2 3) elements)
`(a list of (+ 2 3) elements)
`(a list of ,(+ 2 3) elements)
`(a list of ,mylist elements)
`(a list of ,@mylist elements)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simple pattern matching by some value
(setq return-code 'success)
(pcase return-code
  ('success "It's Okey")
  (code (format "Unexpected %s" code)))

;; More powerful pattern matching by matching with the whole sexp
(pcase '(add 1 2)
  (`(add ,x ,y) (format "Add indentified %s %s" x y))
  (_ (error "Unknown expression!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nonlocal exists (exceptions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exception-example ()
  (catch 'my-custom-exception
    (let ((i 0))
      (while (< i 10)
        (let ((j 1))
          (while (< j 10)
            (if (and (= i 5) (= j 5))
                (throw 'my-custom-exception2 (list i j)))
            (setq j (1+ j))))
        (setq i (1+ i))))))

(catch 'my-custom-exception2
  (exception-example))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Error normally abort all or a part of the running program and returns to a point
;; that is set up to handle an error.

(error "asd") ;; signals an error

;; unwind-protect - protects `body` from errors or exceptions and do finalize steps
(let ((buffer (get-buffer-create "*temp*")))
  (with-current-buffer buffer
    (unwind-protect
        (insert "asd")
      (kill-buffer buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(keywordp :abc) ;; you can't set a value to an any keyword (they constant)

;; boundp returns true if a variable is bounded (and not void)
(let (var)
  (boundp 'var))

;; can remove value part of th variable cell and make a variable as void
(let ((var 2))
  (makunbound 'var)
  (boundp 'var))

(defvar x 1 "My non-const variable") ;; initialize global variable
(defconst y 2 "const") ;; initialize global variable (notify reader that it const)

(defvar xunbound) ;; define an unbound variable
(boundp 'xunbound)

;; when its acomplicated variable value the best to put calculation in declaration
;; C-M-x - force variable reinitialization
(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-a" '+)
    map)
  "my doc string")

;; You can get symbol value if that variable was created in runtimeby make-symbol
(symbol-value 'x)

;; watch if a variable changes
(defun function-watcher (&rest args) (print args))
(setq x 1)
(add-variable-watcher 'x 'function-watcher)
(setq x 2)
(get-variable-watchers 'x)
(remove-variable-watcher 'x 'function-watcher)

;; clojures :]

(setq lexical-binding t) ;; enable lexical binding in the current file

(let ((count 0))
  (defun adder ()
    (setq count (1+ count))))
(adder) ;; 1
(adder) ;; 2

;; declare buffer-local variables

;; make-local-variable - make some existing variable as a buffer local
;; make-variable-buffer-local - make set of some new variable as auto buffer local
;; setq-default - the only way to set default value of prev var declaration
;; setq-local - setq + make-local-variable

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(functionp '+)
(func-arity '+) ;; info about argument list
(subrp 'message) ;; returns true if function is a built-in Lisp function
(subrp '+)
(subrp (symbol-function 'message))
(subrp (symbol-function '+))
(byte-code-function-p (symbol-function 'next-line))
(lambda (a b c) (+ a b c))
(funcall (lambda (a b c) (+ a b c)) 1 2 3) ;; call function with args
(funcall (lambda (a &optional b) (if b (+ a b) (1+ a))) 1 2)
(funcall (lambda (n &rest args) (+ n (apply '+ args))) 1 2 3 4)
(defun fn ()
  (+ 1 2))
(fn)
(defalias 'myfn 'fn)
(myfn)

;; inline compiled function code into the caller
(defsubst mysubst (x)
  x)
(mysubst 1)

;; more efficient inline function wich can inline compiled byte-code
(define-inline myinline (a b)
  (+ a b))
(myinline 1 2)

(defalias '1+ (apply-partially '+ 1))
(1+ 10)
(identity 1) ;; has no side effects and returns it's arg
(ignore 1 2 3) ;; ignore any args passed into it


;; (mapcar 'funcall my-hooks)
(mapcar '1+ [1 2 3])
(mapcar 'string "abc")
(mapcar 'print [1 2 3])
(mapcan 'list '(1 2 3))
(mapc 'print [1 2 3]) ;; used for side effects only and returns ini seq
(mapconcat 'symbol-name '(The cat in hat) " ")
(string-join ["asd" "qwe"] " ")

;; The following forms are equivalent
(lambda (x) (* x x))
(function (lambda (x) (* x x)))
#'(lambda (x) (* x x))

(fboundp '+)
(fmakunbound 'fn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Arguments to the macro call not evaluated and they are given to the macro call
;; as s-expression list. So, macro can operate with it and returns new, modified
;; AST (macro expansion).
;; We shouldn't run logic in macros, because it will execute during compilation.
;; So, it will rexecute once at the compile-time and never at the runtime.
;; So, such a logic should be quoted and then we would compile the one.
;; Do not use macros as something returning a Lisp value, which would changed.
;; When that behaviour would be compiled only one object would be created.
;; So every next call and changes would be to that object. It's wrong.

(defmacro inc (var)
  (list 'setq var (list '1+ var)))

(macroexpand '(inc y))
(macrop 'inc)

(defmacro inc2 (param1 param2)
  (list 'progn (list 'inc param1) (list 'inc param2)))

(macroexpand '(inc2 x y))
(macroexpand-all '(inc2 x y))

(defmacro t-becomes-nil (param)
  `(when (eq ,param t)
     (setq ,param nil)))

(macroexpand '(t-becomes-nil x))

(defmacro for (var from init to final do &rest body)
  (let ((maxvar (make-symbol "max")))
    `(let ((,var ,init)
           (,maxvar ,final))
       (while (<= ,var ,maxvar)
         ,@body
         (inc ,var)))))

(macroexpand '(for x from 1 to 3 do (print x)))

(for x from 1 to 3 do
     (print x))

(defmacro range-for (element container &rest body)
  `(progn
     (seq-doseq (,element ,container)
       ,@body)
     ,container))

(range-for element [1 2 3]
           (print element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x customize - can show some Emacs groups which you should select for your own
;; project by using :group keyword.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minibuffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(read-from-minibuffer "Enter something: ")
(read-from-minibuffer "Enter something: ")
(read-from-minibuffer
 (concat
  (propertize "Bold" 'face '(bold default))
  (propertize " and normal: " 'face '(default))))
(read-string "Enter something: ")
(read-string "Enter something: ")
(read-regexp "Enter regexp: ")
(read-no-blanks-input "Enter: ")
(read-minibuffer "Enter: " "(+ 1 2)")
(eval-minibuffer "Enter: " "(+ 1 2)")

;; Some minibuffer's history variables for particular domain
minibuffer-allow-text-properties
history-delete-duplicates
history-length
minibuffer-history
query-replace-history
file-name-history
buffer-name-history
regexp-history
extended-command-history
shell-command-history
face-name-history
read-expression-history

(add-to-history 'minibuffer-history "newelt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

completion-regexp-list ;; if setup all completions should match with it
completion-ignore-case
read-file-name-completion-ignore-case
read-buffer-completion-ignore-case

(try-completion "foo" '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4)))
(try-completion "foo" '(("barfoo" 2) ("foo" 3)))
(try-completion "forw" obarray)
(try-completion "foo" '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4))
                (lambda (s) (> (length (car s)) 6)))
(all-completions "foo" '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4))
                 (lambda (s) (> (length (car s)) 6)))
(test-completion "barfoo" '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4)))
(completing-read "Enter: " '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4)) nil nil)
completing-read-function ;; for overwriting completely functional from delegate function

minibuffer-completion-table
minibuffer-completion-predicate
(minibuffer-complete-word)
(minibuffer-complete)
(minibuffer-complete-and-exit)
(minibuffer-completion-help)
(display-completion-list)
(buffer-string)
(with-output-to-temp-buffer "*completions*"
  (display-completion-list
   (all-completions "foo" '(("foobar1" 1) ("barfoo" 2) ("foobaz" 3) ("foobar2" 4)))))
minibuffer-local-completion-map

(read-buffer "Buffer name: " "foo" t)
(read-command "Command name: ")
(read-file-name "Filename: ")
(read-directory-name "Directory: ")
(read-shell-command "Shell command: ")

completion-styles
completion-styles-alist

;; specific major mode completion function which uses by completion-at-point
completion-at-point-functions

(y-or-n-p "Yes or no: ")
(yes-or-no-p "Yes or no: ")

(setq read-hide-char ?*)
(read-passwd "Enter password: ")

(minibuffer-message "minibuffer: %s" (minibuffer-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(read-key-sequence "Enter key sequence: ")
(read-key)
(read-event)
(discard-input)

(execute-extended-command "Enter command: ")
(command-execute 'toggle-read-only)
;;(execute-kbd-macro )

pre-command-hook ;; it's called before the current command execute
post-command-hook ;; it's called after the current command execute
this-command ;; current command
last-command ;; command before the current command

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; interactive functions can be called with M-x or through key sequence bound to it.

(defun example/1 ()
  (interactive)
  (forward-word 2))

(defun example/2 (n)
  (interactive "p")
  (forward-word (* 2 n)))

(defun example/3 (n)
  (interactive "nCount: ")
  (forward-word (* 2 n)))

(defun example/4 (buffer1 buffer2 buffer3)
  (interactive "bBuffer1: \nbBuffer2: \nBuffer3: ")
  (delete-other-windows)
  (split-window (selected-window) 15)
  (switch-to-buffer buffer1)
  (other-window 1)
  (split-window (selected-window) 15)
  (switch-to-buffer buffer2)
  (other-window 1)
  (switch-to-buffer buffer3))

(example/4 "*scratch*" "*Messages*" "emacs_manual.el")

(commandp 'example/2)
(call-interactively 'example/2)
(funcall-interactively 'example/2 1) ;; called-interactively-p would be t inside function
(execute-extended-command 2)
(command-execute 'example/2)

(defun example/5 ()
  (interactive)
  (when (called-interactively-p 'any)
    (message "Interactive!")
    'foo-called-interactively))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eventp nil)

(defun sigusr-handler()
  (interactive)
  (message "Caught signal %S" last-input-event))

(define-key special-event-map [sigusr1] 'sigusr-handler)
(signal-process (emacs-pid) 'sigusr1)

;; returns event modifiers from event
(event-modifiers ?a) ;; nil
(event-modifiers ?A) ;; shift
(event-modifiers ?\C-a) ;; control
(event-modifiers ?\C-%) ;; control
(event-modifiers ?\C-\S-a) ;; shift control
(event-modifiers 'f5) ;; nil
(event-modifiers 's-f5) ;; super
(event-modifiers 'M-S-f5) ;; meta shift
(event-modifiers 'mouse-1) ;; click
(event-modifiers 'down-mouse-1) ;; down

;; returns key or mouse button that event describes
(event-basic-type ?a) ;; 97
(event-basic-type 97) ;; a
(event-basic-type ?A) ;; 97
(event-basic-type ?\C-a) ;; 97
(event-basic-type ?\C-%) ;; 37
(event-basic-type ?\C-\S-a) ;; 97
(event-basic-type 'f5) ;; f5
(event-basic-type 's-f5) ;; f5
(event-basic-type 'M-S-f5) ;; f5
(event-basic-type 'mouse-1) ;; mouse-1
(event-basic-type 'down-mouse-1) ;; mouse1

;; convert a list of modifiers and a basic event type to a complex unified event type
(event-convert-list '(control ?a)) ;; ?\C-a
(event-convert-list '(control meta ?a)) ;; ?\C-\M-a
(event-convert-list '(control super f1)) ;; 'C-s-f1

(read-key-sequence "Click on the mode line: ")
;; preferrable mehod
(read-key-sequence-vector "Click on the mode line: ")

(listify-key-sequence "\C-x")
(listify-key-sequence [enter])

(read-event) ;; more low-level read event function
(read-char)

(sleep-for 2)
(message (format "%s" (sit-for 1)))

(recursive-edit)

(put 'delete-region 'disabled "Text deleting was disabled!")
(disable-command 'delete-region)
(enable-command 'delete-region)

(command-history)

(execute-kbd-macro [?\M-x])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymaps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; converts text to Emacs internal key sequence (string or vector)
(kbd "C-x")
(kbd "C-x  C-f")
(kbd "RET")
(kbd "C-c SPC")
(kbd "<f1> SPC")
(kbd "C-M-<down>")

(keymapp lisp-mode-map) ;; is the objects keymap?
(make-sparse-keymap)
(make-keymap)
c-mode-map

(current-active-maps)
(key-binding "\C-x\C-f")
(lookup-key (current-global-map) "\C-x\C-f")
(lookup-key (current-global-map) (kbd "\C-x\C-f"))
(local-key-binding "\C-x\C-f")
(global-key-binding "\C-x\C-f")
(minor-mode-key-binding "\C-x\C-f")

;; Changing key bindings
;; global-set-key - change bindings for global keymap
;; local-set-key - change binding for local keymap
;; define-key - more generic function wich works with manual define keymap
;; \C- - control character
;; \M- - meta character
;; "\C-\M-x" == [?\C-\M-x]
;; (control ?a) - using modifer names, more generic way

;; build sparse keymap and bind C-f for that
(setq map (make-sparse-keymap))
(define-key map "\C-f" 'forward-char)
map

;; build sparse submap for C-x and bind f in that
(define-key map (kbd "C-x f") 'forward-word)
map

;; bind C-p to the ctl-x-map
(define-key map (kbd "C-p") ctl-x-map)
map

;; bind C-f to foo in the ctl-x-map
(define-key map (kbd "C-p C-f") 'foo)
map

(substitute-key-definition 'find-file 'find-file-read-only (current-global-map))

;; remap - it's dummy event that indicates remap decision
(define-key my-mode-map [remap kill-line] 'my-kill-line)

;; undo remapping
(define-key my-mode-map [remap kill-line] nil)

;; show symbol of remapped command
(command-remapping 'kill-line)

(global-set-key (kbd "C-x C-\\") 'next-line)
(global-set-key [?\C-x ?\C-\\] 'next-line)
(global-set-key [(control ?x) (control ?\\)] 'next-line)

(global-set-key [M-mouse-1] 'mouse-set-point)
(global-unset-key [M-mouse-1])

(local-set-key (kbd "C-x C-\\") 'next-line)
(local-unset-key (kbd "C-x C-\\"))

(accessible-keymaps c-mode-map)
27 ;; eval to see key sequence

;; (map-keymap 'forward-char (current-global-map))


(where-is-internal 'kill-line)
(where-is-internal 'forward-char)

(describe-bindings)
(describe-bindings-internal)
(describe-symbol 'kill-line)
(describe-gnu-project)
(describe-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major and Minor modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use defvar or defcustom in major mode for variables so that they are not
;; reinitialized when major mode reenables

(add-hook 'lisp-interaction-mode-hook 'auto-fill-mode)

major-mode ;; shows the current major mode
mode-name ;; shows te current mode name for mode line
lisp-mode-map ;; shows lisp mode kemap settings
lisp-mode-syntax-tableq ;; shows lisp mode syntax table
lisp-mode-hook ;; mode hook for lisp
completion-at-point-functions ;; how to cmplete various keywords in major mode
(make-local-variable 'q) ;; makes custom variables buffer local
(run-mode-hooks 'lisp-mode-hook) ;; run all hooks for the current mode
(define-derived-mode ) ;; recomended way to define your own major mode
change-major-mode-hook ;; hook that running when current major mode would be changed

interpreter-mode-alist ;; which mode should setup for shebang `#!` in the script file
magic-mode-alist ;; would call function if the text at the beginning of the buffer matches regexp
;; magic-mode-alist -> (if failed) -> auto-mode-alist -> (if failed) -> magic-fallback-mode-alist
auto-mode-alist ;; declare extensions for filenames which define various major modes

(normal-mode) ;; setup appropriate mode for current buffer based on the file extension
(set-auto-mode) ;; it would be called from normal-mode.

(add-to-list 'auto-mode-alist
             '(("\\.[^/]*\\'" . fundamental-mode)
               ("/[^\\./]*\\'" . fundamental-mode)
               ("\\.C\\'" . c++-mode)))

;; create mode map for a new mode
(defvar hypertext-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-3] 'do-hyper-link)
    map))
;; define new major mode
(define-derived-mode hypertext-mode
  text-mode
  "Hypertext"
  "Major mode for hypertext."
  (setq-local case-fold-search nil))

;; Basic major modes
text-mode
prog-mode
special-mode

;; All mode hooks shold be called with
(run-mode-hooks 'my-hooks)

;; list of the all minore modes
minor-mode-list

;; Names of minor modes in mode line
minor-mode-alist

;; Define if the minor mode is active
minor-mode-map-alist



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you want to look at the contents of a file but not alter it.
(insert-file-contents "/etc/resolv.conf")

;; Read a file to a buffer without any modifications and conversions
(find-file-literally "/etc/resolv.conf")

;; Read file and create or open an approriate buffer for load data to it.
;; It can perform wildcards.
(find-file "/etc/resolv.conf")

;; Read file without switch to the new buffer (which returns by the function)
(find-file-noselect "/etc/resolv.conf")

;; Read file as readonly buffer
(find-file-read-only "/etc/resolv.conf")

;; Use or not wildcards
find-file-wildcards

;; many others wrapper on posix low-level functions. See manual.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq buffer (current-buffer))
(bufferp buffer)

(set-buffer "*scratch*")

;; save-current-buffer - saves current buffer and restores it lately.
(defun example/append-to-buffer (buffer start end)
  (interactive "BAppend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (save-current-buffer
      (set-buffer (get-buffer-create buffer))
      (insert-buffer-substring oldbuf start end))))

;; alternatively
(defun example/append-to-buffer (buffer start end)
  (interactive "BAppend to buffer: \nr")
  (let ((oldbuf (current-buffer)))
    (with-current-buffer (get-buffer-create buffer)
      (insert-buffer-substring oldbuf start end))))

;; with-temp-buffer - do whatever you want with temp unvisible buffer

(buffer-name)
(buffer-name buffer)
(get-buffer "*scratch*")
buffer-file-name
buffer-file-truename
(get-file-buffer "emacs_manual.el")
list-buffers-directory
(buffer-modified-p)
(set-buffer-modified-p t)
(not-modified)
buffer-read-only
(read-only-mode)
(barf-if-buffer-read-only)
(buffer-list)
(mapcar 'buffer-name (buffer-list))
(other-buffer)
(last-buffer)
(get-buffer-create "foo")
(buffer-live-p (get-buffer "foo"))
(kill-buffer "foo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq current-window (selected-window))
(windowp current-window)
(window-live-p current-window)
(window-valid-p current-window)
(selected-window-group)
(window-frame)
(window-list)
(window-parent)
(frame-first-window)
(window-at-side-p)
(window-tree)
(window-total-height)
(window-total-width)
(window-total-size)
(window-pixel-height)
(window-pixel-width)
(window-full-height-p)
(window-full-width-p)
(window-body-height)
(window-body-width)
(window-body-size)
(window-mode-line-height) ;; in pixels
(window-max-chars-per-line)
window-min-height
window-min-width
(window-min-size)
(window-resizable (selected-window) 1)
(fit-window-to-buffer)
(maximize-window)
(minimize-window)
(split-window)
(split-window-below)
(split-window-right)
(split-window-horizontally)
(split-window-vertically)
(delete-window)
(delete-other-windows)
window-combination-limit
(select-window (selected-window))
;; save-selected-window - working temporarily on some window and restore previous
;; with-selected-window - as previous but select window also
(frame-selected-window)
;;(set-frame-selected-window nil nil)
(window-use-time)
(next-window)
(previous-window)
(other-window 1)
;; walk-windows
(one-window-p)
(window-buffer)
(set-window-buffer nil "emacs_manual.el")
buffer-display-count
(get-buffer-window "emacs_manual.el")
(get-buffer-window-list "emacs_manual.el")
(switch-to-buffer "emacs_manual.el")
(switch-to-buffer-other-window "emacs_manual.el")
(switch-to-buffer-other-frame "emacs_manual.el")
(frame-list)
(delete-other-frames)
(display-buffer-in-side-window "emacs_manual.el" nil)
(window-point)
(point)
(set-window-point nil (- (window-point) 10))
(window-start)
(window-end)

;; It's allowed you to scroll window. Just low-level function.
(set-window-start
 (selected-window)
 (save-excursion
   (goto-char 1)
   (forward-line 2)
   (point)))

(pos-visible-in-window-p (window-start))
(pos-visible-in-window-p (point-min))
(window-line-height)
(scroll-down 10)
(scroll-up 10)
(recenter)
(terminal-coding-system)
(terminal-name)
(terminal-list)
(terminal-parameters)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq current-frame (selected-frame))
(framep current-frame) ;; t - text terminal, x - graphical terminal
(frame-terminal)
(get-device-terminal ":0.0") ;; format : `hostname:displaynumber.screennumber`
(make-frame-on-display ":0.0")
(x-display-list)

(frame-char-height)
(frame-char-width)
;;(set-frame-font asad)
(frame-position)
(frame-width)
(frame-height)
(set-frame-size (selected-frame) 100 100)
(frame-parameters)
(frame-parameter nil 'buffer-list)
(frame-parameter nil 'display) ;; :0.0
(frame-parameter nil 'display-type) ;; color
(frame-parameter nil 'title)
(frame-parameter nil 'name)
(frame-parameter nil 'explicit-name)
(frame-parameter nil 'scroll-bar-width)
(frame-parameter nil 'border-width)
(frame-parameter nil 'minibuffer)
(frame-parameter nil 'cursor-type)
(frame-parameter nil 'font-backend)
(frame-parameter nil 'background-mode)
(frame-parameter nil 'tty-color-mode)
(frame-parameter nil 'font)
(frame-parameter nil 'foreground-color)
(frame-parameter nil 'background-color)
(frame-parameter nil 'mouse-color)
(frame-parameter nil 'cursor-color)
(frame-parameter nil 'border-color)
(frame-parameter nil 'terminal-parameters)
(frame-parameter nil 'frame-title-format)

(setq new-frame (make-frame))
(select-frame current-frame)
(delete-frame new-frame)
(delete-other-frames)

(frame-list)
(visible-frame-list)
(next-frame)
(previous-frame)
(minibuffer-window)

(frame-visible-p current-frame)
(current-frame-configuration)
(defined-colors)
(tty-color-alist)
(display-graphic-p)
(display-color-p)
(display-mouse-p)
(display-pixel-height)
(display-pixel-width)
(display-screens)
(display-images-p)
(display-visual-class)
(display-color-cells)
(x-server-version)
(x-server-vendor)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(point)
(point-min)
(point-max)
(buffer-end 1) ;; max or min position
(buffer-size)
(goto-char 1)
(forward-char 1)
(backward-char 1)
(forward-word 1)
(backward-word 3)
words-include-escapes
(beginning-of-buffer)
(end-of-buffer)
(beginning-of-line 1)
(end-of-line 1)
(line-end-position)
(forward-line 1)
(count-lines (point-min) (point-max))
(count-words (point-min) (point-max))
(line-number-at-pos)
(skip-chars-forward "a-zA-z")
(skip-chars-backward "^a-zA-z")

(example/append-string-to-buffer "!!!asdad!!!" (current-buffer))
(defun example/append-string-to-buffer (string buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (insert string))))

(save-excursion
  (save-restriction
    (goto-char 1)
    (forward-line 2)
    (narrow-to-region 1 (point))
    (goto-char (point-min))
    (replace-string "Strings" "bar")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq m1 (make-marker))
(set-marker m1 100)
m1
(goto-char (point-min))
(insert "Q")
(setq m2 (copy-marker m1))
(set-marker m1 nil)
(markerp m1)
(integer-or-marker-p m1)
(number-or-marker-p m1)
(point-marker)
(point-min-marker)
(point-max-marker)
(marker-position m1)
(marker-buffer m1)
(marker-insertion-type m1)
(mark)
mark-ring
(region-beginning)
(region-end)
(use-region-p) ;; mark mode is enabled, the mark is active, region is valid

kill-ring
kill-ring-max
buffer-undo-list
undo-limit
fill-column
(auto-fill-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(current-indentation)
(indent-to 4)
indent-tabs-mode
indent-line-function
tab-always-indent
(newline-and-indent)
(reindent-then-newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

default-text-properties
(text-properties-at 1)

register-alist
(zlib-available-p)
(base64-encode-string "qwe123")
(base64-decode-string "cXdlMTIz")
(secure-hash-algorithms)
(buffer-hash)
(gnutls-digests)
(gnutls-available-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-ASCII Characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

enable-multibyte-characters
(set-buffer-multibyte t)
(multibyte-string-p "asdйцу")
(string-bytes "asdйцу")
(unibyte-string ?x ?X)
(string-to-multibyte "йцу123")
(string-to-unibyte "qwe123")
(byte-to-string 97)
(multibyte-char-to-unibyte ?e)
(unibyte-char-to-multibyte ?e)
(set-buffer-multibyte t)
(string-as-multibyte "qwe123йцу")
(string-as-unibyte "123qweйцу")
(charset-list)
(charsetp 'ascii)
(charsetp 'cp866)
(char-charset ?н)
(charset-plist 'cp866)
(list-charset-chars 'cp866)
(decode-char 'cp866 ?н)
(encode-char ?н 'cp866)
(charset-after 1)
(find-charset-string "123йцуqwe")
standard-translation-table-for-encode
standard-translation-table-for-decode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encodings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

buffer-file-coding-system
save-buffer-coding-system
last-coding-system-used
file-name-coding-system
selection-coding-system

(coding-system-list)
(coding-system-p 'cp866)
(check-coding-system 'cp866)
(coding-system-eol-type 'cp866)
(find-coding-systems-region (point-min) (point-max))
(find-coding-systems-string "йцу")
(check-coding-systems-region (point-min) (point-max) '(cp866 cp1251 windows-1251))
(detect-coding-region (point-min) (point-max))
(detect-coding-string "123asdЙЦУ")
inhibit-null-byte-detection
(coding-system-charset-list 'utf-8-unix)


;; it tries: default CS -> buffer-file-coding-system -> prefer-coding-system
(select-safe-coding-system (point-min) (point-max))
(select-safe-coding-system "123asdЙЦУ" nil)
(select-safe-coding-system "123asdЙЦУ" nil '(cp866 cp1251 utf-8-unix)) ;; try with default coding system
(prefer-coding-system 'utf-8-unix)
(keyboard-coding-system)
(terminal-coding-system)
current-input-method
default-input-method
locale-coding-system
system-messages-locale
system-time-locale
(locale-info 'codeset)
(locale-info 'days)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Threads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(current-thread)
(all-threads)
(setq t1 (make-thread (lambda () (sleep-for 10))))
(thread-join t1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

exec-path
exec-directory
exec-suffixes
default-directory
(call-process "ls" nil "ls-output" nil "-lhs")
(seq-length (list-system-processes))
(list-processes)
(process-list)
(process-status "shell")
(process-attributes 1)

(open-network-stream "ya.ru" "ya.ru" "ya.ru" "80")
(process-send-string "ya.ru" "GET / HTTP/1.1\r\n\r\n")

(shell-command "evince")

process-environment

(require 'notifications)
(notifications-notify "asd")

(let ((progress-reporter (make-progress-reporter "Working..." 0 500)))
  (dotimes (k 500)
    (sit-for 0.01)
    (progress-reporter-update progress-reporter k))
  (progress-reporter-done progress-reporter))

(display-warning "qwe" "asd" :warning)

(with-output-to-temp-buffer "foo"
    (print "qwe"))

(font-family-list)
