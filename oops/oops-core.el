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

(defvar oops-history-max 16)
(defvar oops-history nil
  "The history is a list containing element with format of \(MARKER . BOUND-BEGINNING\). The MARKER's position is actually the BOUND-END.")


;; ###autoload
(defun oops--push-history (bound-beg bound-end)
  "Push the history item into history and remove the duplicate elements. Check `oops-history' for the details of the history element."
  ;; Remove duplicate history.
  (if (listp oops-history)
      (dolist (history-item oops-history)
        (let ((beg (cdr history-item))
              (end (marker-position (car history-item)))
              (buffer (marker-buffer (car history-item))))
          (when (and (eq bound-beg beg)
                     (eq bound-end end)
                     (eq (current-buffer) buffer))
            (setq oops-history (delq history-item oops-history))
            )
          )
        )
    )
  ;; Push history.
  (let ((history-item (cons (copy-marker (set-marker (mark-marker) bound-end))
                            bound-beg)))
    (if (null oops-history)
        ;; 1st history element.
        (setq oops-history (list history-item))
      ;; else.
      (setq oops-history (cons history-item oops-history))
      )
    )
  ;; Keep the lenght less than maximum length.
  (when (> (length oops-history) oops-history-max)
    (set-marker (car (nthcdr oops-history-max oops-history)) nil)
    (setcdr (nthcdr (1- oops-history-max) oops-history) nil)
    )
  )

;; test code ====================>
(defun test1 ()
  )

(defun test2 ()
  (test1)
  )

(defun test3 ()
  (test1)
  (test2)
  )
;; (setq oops-history nil)
;; <==================== test code

;; ###autoload
(defun oops-prev-history ()
  "Navigate to previous history by rotating the `oops-history'.
\(1 2 3 4 5\) => \(2 3 4 5 1\) and use \(2\) history."
  (interactive)
  ;; Pop entries which refer to non-existent buffers.
  (while (and oops-history
              (not (marker-buffer (caar oops-history))))
    (setq oops-history (cdr oops-history))
    )
  ;; Rotate the history.
  (setq oops-history (nconc (cdr oops-history)
                            (list (car oops-history))))
  (if oops-history
      (let* ((marker (caar oops-history))
             (buffer (marker-buffer marker))
             (beg (cdar oops-history))
             (end (marker-position marker)))
        (set-buffer buffer)
        ;; Disable region.
        (setq mark-active nil)
        (or (and (>= end (point-min))
                 (<= end (point-max)))
            (if widen-automatically
                (widen)
              )
            )
        (set-marker (mark-marker) beg)
        (goto-char end)
        (switch-to-buffer buffer)
        ;; Enable region.
        (unless (= beg end)
          (setq mark-active t)
          )
        (message "Go to previous navigation history.")
        ;; (message "[%s/%s] - %s" (length oops-history) oops-history-max oops-history)
        )
    (message "No history was set!")
    )
  )

(defun oops-next-history ()
  "Navigate to next history by rotating the `oops-history'.
\(2 3 4 5 1\) => \(1 2 3 4 5\) and use \(1\) history."
  (interactive)
  ;; Pop entries which refer to non-existent buffers.
  (while (and oops-history
              (not (marker-buffer (caar (last oops-history)))))
    (setq oops-history (butlast oops-history))
    )
  ;; Rotate the history.
  (setq oops-history (nconc (last oops-history)
                            (butlast oops-history)))
  (if oops-history
      (let* ((marker (caar oops-history))
             (buffer (marker-buffer marker))
             (beg (cdar oops-history))
             (end (marker-position marker)))
        (set-buffer buffer)
        ;; Disable region.
        (setq mark-active nil)
        (or (and (>= end (point-min))
                 (<= end (point-max)))
            (if widen-automatically
                (widen)
              )
            )
        (set-marker (mark-marker) beg)
        (goto-char end)
        (switch-to-buffer buffer)
        ;; Enable region.
        (unless (= beg end)
          (setq mark-active t)
          )
        (message "Go to next navigation history.")
        ;; (message "[%s/%s] - %s" (length oops-history) oops-history-max oops-history)
        )
    (message "No history was set!")
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
