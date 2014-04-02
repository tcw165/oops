
;; native library.
(require 'thingatpt)

(defun oops-require-check ()
  "Check if the cursor or the selection is on which just after 'require statement.
Return t if is on which just after 'require statement. Other is nil."
  (save-excursion
    (backward-up-list)
    (forward-char)
    (if (equal "require" (thing-at-point 'symbol))
        t ;; return t
      nil ;; return nil
      )
    )
  )

(defun oops-thing-at-point ()
  "Return string according to syntax-table, \"_w\", to get the
symbol string in which the point is.
Or just return the text selection."
  (if mark-active
      ;; if there is already a selection, use the selection.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; else.
    (thing-at-point 'symbol)
    )
  )
