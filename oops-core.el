;; Native library ==============================================================
(require 'thingatpt)

(defun oops-thing-at-point (&optional skip-select)
  "Return string on which the point is or just string of selection."
  (if mark-active
      (if (null skip-select)
          ;; return the selection.
          (buffer-substring-no-properties (region-beginning) (region-end))
        ;; skip selection, return nil
        nil)
    ;; else.
    (thing-at-point 'symbol)
    )
  )

(defun oops-kill-current-buffer ()
  "Kill `current-buffer'."
  (interactive)
  (kill-buffer (current-buffer))
  )

(defun oops-toggle-comment ()
  "Toggle comment."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when mark-active
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))
            )
      )
    (comment-or-uncomment-region start end)
    )
  )

(defun oops-duplicate-lines (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
          (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
        (insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
        (insert current-line)
        (decf n))))
  )

(defun oops-kill-lines ()
  ""
  (interactive)
  (message "Yet ready...")
  )

(provide 'oops-core)
