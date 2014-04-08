;; Update load-path ============================================================
(add-to-list 'load-path (concat (file-name-directory load-file-name) "3rd-party"))

;; Native library ==============================================================
(require 'thingatpt)

;; 3rd party library ===========================================================
(require 'highlight-symbol)
(require 'highlight-parentheses)
(require 'auto-complete)

(defun oops-thing-at-point ()
  "Return string on which the point is or just string of selection."
  (if mark-active
      ;; if there is already a selection, return the selection.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; else.
    (thing-at-point 'symbol)
    )
  )

(defun oops-kill-current-buffer ()
  "Kill current-buffer."
  (interactive)
  (kill-buffer (current-buffer))
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
