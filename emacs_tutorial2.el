;; show message in the `messages` buffer
(message "hello, world!")

;; show menu bar in top of the screen
(menu-bar-mode -1)

;; show line numbers in buffers
(global-linum-mode t)

;; highlight parenteses in buffers
(show-paren-mode 1)

;; show emacs lisp function argument highlighting
(turn-on-eldoc-mode)

;; describe-function
;; C-h f show-paren-mode

;; describe-variable
;; C-h v visible-bell

;; open source file with specified function
;; M-x find-function

;; find all functions with specified text in their values
;; M-x apropos

;; set variable value but it may set buffer local (set quoted)
(setq column-number-mode t)
(set 'column-number-mode t)

;; set default variable value
(setq-default tab-width 4)

emacs-lisp-mode-hook
'emacs-lisp-mode-hook

;; sets the first function definition to second function definition
(fset 'yes-or-no-p 'y-or-n-p)

;; working with lists. set new list variable
(setq diff-switches '("-b" "-u"))
(setq diff-switches (list "-b" "-u"))

;; add to exists list variable
(add-to-list 'load-path "~/elisp")

;; hooks - functions are called from emacs listp in order to modify the
;; behaviour of something
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-to-list 'emacs-list-mode-hook 'turn-on-eldoc-mode)

;; delete from a list
(setq load-path (delete "~/elisp" load-path))
(remove-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; dot notation - cons cell
;; '("abc" "def") is equivalent to (const "abc" (cons "def" nil))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(car '("abc" . "def"))
(cdr '("abc" . "def"))

;; returns "def", which is a string
(cdr '("abc" . "def"))

;; return "def", which is a list
(cdr '("abc" "def"))

;; keybindings
(global-set-key [f10] (quote save-buffer))

;; C-x ESC ESC (repeat-complex-command)
;; M-x global-set-key

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; C-h r - emacs editor manual
