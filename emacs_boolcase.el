(defvar boolcase-mode-words '("true" "false")
  "Words to capitalize")

(defun boolcase-mode-fix ()
  (save-excursion
    (copy-region-as-kill (point) (progn (backward-sexp) (point)))
    (when (member (current-kill 0) boolcase-mode-words)
      (capitalize-word 1))
    (setq kill-ring (cdr kill-ring))))

(defun boolcase-mode-check ()
  "Check if we capitalize or not"
  (if (= last-command-event 101)
      (boolcase-mode-fix)))

(define-minor-mode boolcase-mode
  "Automatically capitalize booleans"
  :lighter " BC"

  (if boolcase-mode
      (add-hook 'post-self-insert-hook
                'boolcase-mode-check nil t)
    (remove-hook 'post-self-insert-hook
                 'boolcase-mode-check t)))
