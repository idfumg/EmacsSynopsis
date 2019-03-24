(defvar quip-mode-hook nil
  "*List of functions to call when entering Quip mode.")

(defvar quip-mode-map nil
  "Keymap for quip major mode.")

(defvar quip-mode-syntax-table nil
  "Syntax table for quip major mode.")

(defalias 'backward-quip 'backward-page)
(defalias 'forward-quip 'forward-page)
(defalias 'narrow-to-quip 'narrow-to-page)
(defalias 'what-quip 'what-page)

(if quip-mode-map
    nil
  (setq quip-mode-map (copy-keymap text-mode-map))
  (define-key quip-mode-map "\C-x [" 'backward-quip)
  (define-key quip-mode-map "\C-x ]" 'forward-quip)
  (define-key quip-mode-map "\C-x n q" 'narrow-to-quip)
  (define-key quip-mode-map "\C-c w" 'what-quip))

(if quip-mode-syntax-table
    nil
  (setq quip-mode-syntax-table (copy-syntax-table text-mode-syntax-table)))

(defun quip-mode()
  "Major mode for editing Quip files.
Special commands:
  \\{quip-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'quip-mode)
  (setq mode-name "Quip")
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'page-delimiter)
  (setq paragraph-start "%%\\I[ \t\n^L]")
  (setq paragraph-separate "%%$\\ [ \t\^L]*$")
  (setq page-delimiter "^%%$")
  (use-local-map quip-mode-map)
  (run-hooks 'quip-mode-hook))

(defun count-quips ()
  "Count the quips in the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((result (count-matches "^%%$")))
        (message "%s" result)
        result))))

(provide 'quip)

;; paragraph-start
;; paragraph-separate
;; C-x [ and C-x ] - for moving page back and forth

;; User can bind like this:
;; (add-hook 'quip-mode-hook
;;           (lambda ()
;;             (local-set-key "\M-p" 'backward-quip)
;;             (local-set-key "\M-n" 'forward-quip)
;;             (local-unset-key "\C-x [")
;;             (local-unset-key "\C-x ]")))

;; You can see function description with commands by calling M-x describe-function
