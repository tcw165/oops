;; GNU Library =================================================================
(require 'ido)
(require 'imenu)
(require 'thingatpt)
(require 'company)

;; Oops Library ================================================================
(require 'oops-lisp-lib)
(require 'oops-python-lib)
(require 'oops-c-lib)
(require 'oops-cpp-lib)

;; Text Operation ==============================================================

(defun oops-undo ()
  "Discard the selection and then undo something."
  (interactive)
  (deactivate-mark)
  (undo)
  )

(defun oops-kill-current-buffer ()
  "Kill the buffer you are focusing which is `current-buffer'."
  (interactive)
  (kill-buffer (current-buffer))
  )

(defun oops-toggle-comment ()
  "Toggle comment smartly. It support comment on both single line and multiple lines."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))
            )
      )
    (comment-or-uncomment-region beg end)
    )
  )

(defun oops-duplicate-lines ()
  "Duplciate the current line or the lines between point and mark without modifying `kill-ring'."
  (interactive)
  ;; There're two situation:
  ;; (point) ---------------
  ;; -----------------(mark)
  ;; or
  ;; (mark)-----------------
  ;; ----------------(point)
  (let* ((beg (line-beginning-position 1))
         (end (line-beginning-position 2))
         (str (buffer-substring-no-properties beg end)))
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line 1)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (beginning-of-line 2)
                  (point))
            str (buffer-substring-no-properties beg end)
            )
      ;; (push-mark)
      )
    (save-excursion
      (if (and mark-active
               (> (mark) (point)))
          (goto-char (region-end)))
      (beginning-of-line 2)
      (insert str)
      )
    ;; Restore selection.
    (setq deactivate-mark nil)
    )
  )

(defun oops-kill-lines ()
  "Kill the current line or the lines between point and mark without modifying `kill-ring'."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line 1)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (beginning-of-line 2)
                  (point))
            )
      )
    (delete-region beg end)
    )
  )

(defun oops--move-lines (step)
  "Move the current line or the lines covered by region upward or downward without modifying `kill-ring'."
  ;; There're two situation:
  ;; (point) ---------------
  ;; -----------------(mark)
  ;; or
  ;; (mark)-----------------
  ;; ----------------(point)
  (let* ((beg (line-beginning-position 1))
         (end (line-beginning-position 2))
         (line-num 1)
         (point-excursion (- (point) end))
         (mark-excursion 0)
         text)
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line 1)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (beginning-of-line 2)
                  (point))
            line-num (count-lines (region-beginning) (region-end))
            point-excursion (- (point) end)
            mark-excursion (- (mark) (point))
            )
      )
    ;; Extract region.
    (setq text (delete-and-extract-region beg end))
    ;; Move upward/downward and insert the cut region.
    (forward-line step)
    (insert text)
    ;; Set new point.
    (goto-char (+ (point) point-excursion))
    ;; Set new mark.
    (when mark-active
      (deactivate-mark)
      (set-mark (+ (point) mark-excursion))
      (setq deactivate-mark nil)
      )
    )
  )

(defun oops-move-lines-up ()
  "Move the current line or the lines covered by region upward without modifying `kill-ring'."
  (interactive)
  (oops--move-lines -1)
  )

(defun oops-move-lines-down ()
  "Move the current line or the lines covered by region downward without modifying `kill-ring'."
  (interactive)
  (oops--move-lines 1)
  )

(defun oops-indent-or-company ()
  (interactive)
  (if (or mark-active
          ;; including: "begining of line"", "end of line"", "space"", "("
          (looking-at "\\(^\\|$\\|\(\\|\\s-+\\)"))
      (indent-for-tab-command)
    (company-complete-common))
  )

;; Navigation ==================================================================

;; TODO: customization support.
(defvar oops-history-max 32)

;; ###autoload
(defun oops-prev-history ()
  "Navigate to previous record in the history."
  (interactive)
  (cond
   ;; lisp
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'lisp-interaction-mode))
    (oops-lisp-prev-history)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

(defun oops-next-history ()
  "Navigate to next record in the history."
  (interactive)
  (cond
   ;; lisp
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'lisp-interaction-mode))
    (oops-lisp-next-history)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

;; ###autoload
(defun oops-jump-to-definition-atpt ()
  "Find the symbol's definition. If there's only one result, open the file in the current window. If there're multiple results, show a list under the current window.

For lisp, it supports symbol of `defun', `defadvice', `defvar' and `provide'.
For C/C++, it doesn't support yet.
For Python, it doesn't support yet."
  (interactive)
  (cond
   ;; lisp
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'lisp-interaction-mode))
    (oops-lisp-jump-to-definition-atpt)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

;; ###autoload
(defun oops-goto-global-symbol ()
  ""
  (interactive)
  (cond
   ;; lisp
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'lisp-interaction-mode))
    )
   ;; c
   ;; c++
   ;; python
   )
  )

;; ###autoload
(defun oops-goto-local-symbol ()
  (interactive)
  (cond
   ;; lisp
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'lisp-interaction-mode))
    (oops-lisp-goto-lsymb)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

;; ###autoload
(defun oops-common-escape ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers),
or go back to just one window (by deleting all but the selected window)."
  (interactive)
  (cond
   ((eq last-command 'mode-exited) nil
    )
   ((region-active-p)
    (deactivate-mark)
    )
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit)
    )
   (current-prefix-arg
    nil)
   ((> (recursion-depth) 0)
    (exit-recursive-edit)
    )
   (buffer-quit-function
    (funcall buffer-quit-function)
    )
   ;; ((not (one-window-p t))
   ;;  (delete-other-windows)
   ;;  )
   ((string-match "^ \\*" (buffer-name (current-buffer)))
    (bury-buffer)
    )
   )
  )

(provide 'oops-core)
