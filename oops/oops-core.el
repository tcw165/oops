;; GNU library =================================================================
(require 'ido)
(require 'imenu)
(require 'thingatpt)
(require 'company)

;; Text Operation ==============================================================

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

(defun oops-undo ()
  "Just undo something without caring the region."
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

(defun oops-move-lines (step)
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
  (oops-move-lines -1)
  )

(defun oops-move-lines-down ()
  "Move the current line or the lines covered by region downward without modifying `kill-ring'."
  (interactive)
  (oops-move-lines 1)
  )

;; Navigation ==================================================================

(defun oops-prev-history ()
  "Navigate to previous history by looking element in the `global-mark-ring'."
  (interactive)
  (pop-global-mark)
  (message "[%s] - %s" global-mark-ring-max global-mark-ring)
  )

(defun oops-next-history ()
  "Navigate to next history by looking element in the `global-mark-ring'."
  (interactive)
  (message "(oops-next-history) Yet ready...")
  )

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

;; `imenu' =====================================================================

(defun oops-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (oops-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Find symbol in file: " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position))))
    )
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (oops-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position)))))
    )
   )
  )

;; `company' ===================================================================

(defun oops-indent-or-company ()
  (interactive)
  (if (or mark-active
          (looking-at "\\(^\\|$\\|\(\\|\\s-+\\)")) ;; begining of line, end of line, space, '('
      (indent-for-tab-command)
    (company-complete-common))
  )

(provide 'oops-core)
