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

(defun oops-kill-current-buffer ()
  "Kill the buffer you are focusing which is `current-buffer'."
  (interactive)
  (kill-buffer (current-buffer))
  )

(defun oops-toggle-comment ()
  "Toggle comment smartly. It support comment on both single line and multiple lines."
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

(defun oops-duplicate-lines ()
  ""
  (interactive)
  (message "Yet ready...")
  ;; (save-excursion
  ;;   (let ((nb (or n 1))
  ;;         (current-line (thing-at-point 'line)))
  ;;     ;; when on last line, insert a newline first
  ;;     (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
  ;;       (insert "\n"))

  ;;     ;; now insert as many time as requested
  ;;     (while (> n 0)
  ;;       (insert current-line)
  ;;       (decf n))))
  )

(defun oops-kill-lines ()
  ""
  (interactive)
  (message "Yet ready...")
  )

(defun oops-generic-escape ()
  ""
  (interactive)
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
          (looking-at "\\(^\\|$\\|\(\\|\)\\|\\s-+\\)")) ;; begining of line, end of line, space, '(', ')'
      (indent-for-tab-command)
    (company-complete-common))
  )

(provide 'oops-core)
