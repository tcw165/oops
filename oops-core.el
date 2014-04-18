;; Update load-path ============================================================
(defun oops-add-subdirs-to-load-path ()
  "Add all the sub-directories under current directory into load-path."
  (when (not (null load-file-name))
    (dolist (file (directory-files (file-name-directory load-file-name)))
      (let ((file-path (concat (file-name-directory load-file-name) file)))
        (when (and (file-directory-p file-path)
                   (not (equal file "."))
                   (not (equal file ".."))
                   (not (equal file ".svn"))
                   (not (equal file ".git"))
                   (not (memq file-path load-path)))
          (add-to-list 'load-path file-path)
          )
        )
      )
    )
  )
(oops-add-subdirs-to-load-path)

;; Native library ==============================================================
(require 'thingatpt)

;; 3rd party library ===========================================================
(require 'highlight-symbol)
(require 'highlight-parentheses)
(require 'auto-complete)

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
