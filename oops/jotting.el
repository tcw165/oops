;; (find-lisp-object-file-name 'major-mode 'defvar)
;; (find-lisp-object-file-name 'oops-mode 'defvar)

;; (let ((a 123))
;;   (message "%d" a)
;;   (variable-binding-locus 'a)
;;   )
;; (variable-binding-locus 'company-mode)
;; (variable-binding-locus 'oops-mode)
;; (symbol-value 'oops-mode)

;; (oops-lisp-describe-variable 'oops-mode)
;; (prin1 'oops-mode)

;; (defun describe-variable (variable &optional buffer frame)
(defun oops-lisp-describe-variable (variable)
  (let ((standard-output (oops--lisp-help-buffer 1))
        file-name)
    (save-excursion
      (let ((permanent-local (get variable 'permanent-local))
            (locus (variable-binding-locus variable))
            (val (symbol-value variable))
            val-start-pos
            )
        (with-current-buffer (current-buffer)
          (prin1 variable)
          (setq file-name (find-lisp-object-file-name variable 'defvar))

          (if file-name
              (progn
                (princ " is a variable defined in `")
                (princ (if (eq file-name 'C-source)
                           "C source code"
                         (file-name-nondirectory file-name)))
                (princ "'.\n")
                ;; (with-current-buffer standard-output
                ;;   (save-excursion
                ;;     (re-search-backward "`\\([^`']+\\)'" nil t)
                ;;     (help-xref-button 1 'help-variable-def
                ;;                       variable file-name)
                ;;     )
                ;;   )
                )
            )
          )
        (with-current-buffer standard-output
          (setq val-start-pos (point))
          (princ "value is ")
          (let ((from (point))
                (line-beg (line-beginning-position))
                (print-rep
                 (let ((print-quoted t))
                   (prin1-to-string val))))
            (if (< (+ (length print-rep) (point) (- line-beg)) 68)
                (insert print-rep)
              (terpri)
              (pp val)
              (if (< (point) (+ 68 (line-beginning-position 0)))
                  (delete-region from (1+ from))
                (delete-region (1- from) from)
                )
              )
            (let* ((sv (get variable 'standard-value))
                   (origval (and (consp sv)
                                 (condition-case nil
                                     (eval (car sv))
                                   (error :help-eval-error)))))
              (when (and (consp sv)
                         (not (equal origval val))
                         (not (equal origval :help-eval-error)))
                (princ "\nOriginal value was \n")
                (setq from (point))
                (pp origval)
                (if (< (point) (+ from 20))
                    (delete-region (1- from) from)
                  )
                )
              )
            )
          )
        (terpri)
        (when locus
          (cond
           ((bufferp locus)
            (princ (format "Local in buffer %s; "
                           (buffer-name)))
            )
           ((framep locus)
            (princ (format "It is a frame-local variable; "))
            )
           ((terminal-live-p locus)
            (princ (format "It is a terminal-local variable; "))
            )
           (t (princ (format "It is local to %S" locus)))
           )
          (if (not (default-boundp variable))
              (princ "globally void")
            (let ((global-val (default-value variable)))
              (with-current-buffer standard-output
                (princ "global value is ")
                (if (eq val global-val)
                    (princ "the same.")
                  (terpri)
                  ;; Fixme: pp can take an age if you happen to
                  ;; ask for a very large expression.  We should
                  ;; probably print it raw once and check it's a
                  ;; sensible size before prettyprinting.  -- fx
                  (let ((from (point)))
                    (pp global-val)
                    ;; See previous comment for this function.
                    ;; (help-xref-on-pp from (point))
                    (if (< (point) (+ from 20))
                        (delete-region (1- from) from)
                      )
                    )
                  )
                )
              )
            )
          (terpri)
          )

        ;; If the value is large, move it to the end.
        (with-current-buffer standard-output
          (when (> (count-lines (point-min) (point-max)) 10)
            ;; Note that setting the syntax table like below
            ;; makes forward-sexp move over a `'s' at the end
            ;; of a symbol.
            (set-syntax-table emacs-lisp-mode-syntax-table)
            (goto-char val-start-pos)
            ;; The line below previously read as
            ;; (delete-region (point) (progn (end-of-line) (point)))
            ;; which suppressed display of the buffer local value for
            ;; large values.
            (when (looking-at "value is") (replace-match ""))
            (save-excursion
              (insert "\n\nValue:")
              (set (make-local-variable 'help-button-cache)
                   (point-marker)))
            (insert "value is shown ")
            (insert-button "below"
                           'action help-button-cache
                           'follow-link t
                           'help-echo "mouse-2, RET: show value")
            (insert ".\n")
            )
          )
        (terpri)

        (let* ((alias (condition-case nil
                          (indirect-variable variable)
                        (error variable)))
               (obsolete (get variable 'byte-obsolete-variable))
               (use (car obsolete))
               (safe-var (get variable 'safe-local-variable))
               (doc (or (documentation-property
                         variable 'variable-documentation)
                        (documentation-property
                         alias 'variable-documentation)))
               (extra-line nil))

          ;; Mention if it's a local variable.
          (cond
           ((and (local-variable-if-set-p variable)
                 (or (not (local-variable-p variable))
                     (with-temp-buffer
                       (local-variable-if-set-p variable))))
            (setq extra-line t)
            (princ "  Automatically becomes ")
            (if permanent-local
                (princ "permanently "))
            (princ "buffer-local when set.\n")
            )

           ((not permanent-local))

           ((bufferp locus)
            (princ "  This variable's buffer-local value is permanent.\n")
            )

           (t (princ "  This variable's value is permanent \
if it is given a local binding.\n"))
           )

          ;; Mention if it's an alias.
          (unless (eq alias variable)
            (setq extra-line t)
            (princ (format "  This variable is an alias for `%s'.\n" alias)))

          (when obsolete
            (setq extra-line t)
            (princ "  This variable is obsolete")
            (if (nth 2 obsolete)
                (princ (format " since %s" (nth 2 obsolete))))
            (princ (cond ((stringp use) (concat ";\n  " use))
                         (use (format ";\n  use `%s' instead." (car obsolete)))
                         (t ".")))
            (terpri))

          (when (member (cons variable val) file-local-variables-alist)
            (setq extra-line t)
            (if (member (cons variable val) dir-local-variables-alist)
                (let ((file (and (buffer-file-name)
                                 (not (file-remote-p (buffer-file-name)))
                                 (dir-locals-find-file
                                  (buffer-file-name))))
                      (dir-file t))
                  (princ "  This variable's value is directory-local")
                  (if (null file)
                      (princ ".\n")
                    (princ ", set ")
                    (if (consp file) ; result from cache
                        ;; If the cache element has an mtime, we
                        ;; assume it came from a file.
                        (if (nth 2 file)
                            (setq file (expand-file-name
                                        dir-locals-file (car file)))
                          ;; Otherwise, assume it was set directly.
                          (setq dir-file nil)))
                    (princ (if dir-file
                               "by the file\n  `"
                             "for the directory\n  `"))
                    (with-current-buffer standard-output
                      (insert-text-button
                       file 'type 'help-dir-local-var-def
                       'help-args (list variable file)))
                    (princ "'.\n")))
              (princ "  This variable's value is file-local.\n")))

          (when (memq variable ignored-local-variables)
            (setq extra-line t)
            (princ "  This variable is ignored as a file-local \
variable.\n"))

          ;; Can be both risky and safe, eg auto-fill-function.
          (when (risky-local-variable-p variable)
            (setq extra-line t)
            (princ "  This variable may be risky if used as a \
file-local variable.\n")
            (when (assq variable safe-local-variable-values)
              (princ "  However, you have added it to \
`safe-local-variable-values'.\n")
              )
            )

          (when safe-var
            (setq extra-line t)
            (princ "  This variable is safe as a file local variable ")
            (princ "if its value\n  satisfies the predicate ")
            (princ (if (byte-code-function-p safe-var)
                       "which is a byte-compiled expression.\n"
                     (format "`%s'.\n" safe-var)))
            )

          (if extra-line (terpri))
          (princ "Documentation:\n")
          (with-current-buffer standard-output
            (insert (or doc "Not documented as a variable."))
            )
          )

        ;; Make a link to customize if this variable can be customized.
        (when (custom-variable-p variable)
          (let ((customize-label "customize"))
            (terpri)
            (terpri)
            (princ (concat "You can " customize-label " this variable."))
            (with-current-buffer standard-output
              (save-excursion
                (re-search-backward
                 (concat "\\(" customize-label "\\)") nil t)
                (help-xref-button 1 'help-customize-variable variable)
                )
              )
            )
          ;; Note variable's version or package version
          (let ((output (describe-variable-custom-version-info variable)))
            (when output
              (terpri)
              (terpri)
              (princ output)
              )
            )
          )

        (with-current-buffer standard-output
          ;; Return the text we displayed.
          (buffer-string)
          )
        )
      )
    )
  )
