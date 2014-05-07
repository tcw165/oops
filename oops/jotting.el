;; (find-lisp-object-file-name 'major-mode 'defvar)
;; (find-lisp-object-file-name 'oops-mode 'defvar)

;; (variable-binding-locus 'company-mode)
;; (variable-binding-locus 'oops-mode)

;; (documentation-property 'company-mode 'variable-documentation)

;; (symbol-value 'oops-mode)
;; (read "123")

;; (defun eat-output (c)
;;   (message "%c" c)
;;   )
;; (print "this is the output" 'eat-output)

;; (print 'The\ cat\ in)
;; (print "the hat")
;; (print " came back")

;; (oops--lisp-temp-buffer)
(defun oops--lisp-temp-buffer ()
  (let ((buffer (get-buffer-create "*scratch*")))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)
        )
      )
    buffer
    )
  )

;; (get 'company-mode 'standard-value)
;; (variable-binding-locus 'company-mode)
;; (oops-lisp-describe-variable 'company-mode)
;; (oops-lisp-describe-variable 'load-path)
(defun oops-lisp-describe-variable (variable)
  (let* ((standard-output (oops--lisp-temp-buffer))
         (alias (condition-case nil
                    (indirect-variable variable)
                  (error variable)))
         (doc (or (documentation-property
                   variable 'variable-documentation)
                  (documentation-property
                   alias 'variable-documentation)))
         (val (symbol-value variable))
         (locus (variable-binding-locus variable))
         (permanent-local (get variable 'permanent-local))
         (file-name (find-lisp-object-file-name variable 'defvar))
         (print-length 32)
         (print-level 3)
         )
    ;; TODO: Make variable text with hyper-link property.

    ;; Indicate where is its definition and its current value.
    (princ variable)
    (princ " is a variable defined in ")
    (princ (if (eq file-name 'C-source)
               "C source code"
             (file-name-nondirectory file-name)))
    (terpri)
    (terpri)
    (princ "Its value is ")
    (pp val)
    ;; Add hyper-link property to file-name text.
    ;; (with-current-buffer standard-output
    ;;   (save-excursion
    ;;     (re-search-backward "`\\([^`']+\\)'" nil t)
    ;;     (help-xref-button 1 'help-variable-def
    ;;                       variable file-name)
    ;;     )
    ;;   )

    ;; If it is a customizable variable, show its original value.
    ;; "Original value was?"
    (let* ((sv (get variable 'standard-value))
           (origval (and (consp sv)
                         (condition-case nil
                             (eval (car sv))
                           (error :help-eval-error)))))
      (when (and (consp sv)
                 (not (equal origval val))
                 (not (equal origval :help-eval-error)))
        (princ "; customizable its original value was ")
        (pp origval)
        )
      )

    ;; Indicate which kind of variable it is and its defaulte value.
    ;; "Local in buffer?"
    ;; "Frame-local variable?"
    ;; "Terminal-local variable?"
    (when locus
      (cond
       ((bufferp locus)
        (princ (format "; local in buffer %s; " (buffer-name)))
        )
       ((framep locus)
        (princ (format "; it is a frame-local variable; "))
        )
       ((terminal-live-p locus)
        (princ (format "; it is a terminal-local variable; "))
        )
       (t (princ (format "; it is local to %S" locus)))
       )

      (if (not (default-boundp variable))
          (princ "globally void")
        (let ((global-val (default-value variable)))
          (princ "global value is ")
          (if (eq val global-val)
              (princ "the same.\n")
            (pp global-val)
            (terpri)
            )
          )
        )
      )

    ;; Indicate which kind of local variable it is.
    (cond
     ((and (local-variable-if-set-p variable)
           (or (not (local-variable-p variable))
               (with-temp-buffer
                 (local-variable-if-set-p variable))))
      (princ "\n  Automatically becomes ")
      (if permanent-local
          (princ "permanently "))
      (princ "buffer-local when set.\n\n")
      )
     ((not permanent-local))
     ((bufferp locus)
      (princ "\n  This variable's buffer-local value is permanent.\n\n")
      )
     (t
      (princ "\n  This variable's value is permanent if it is given a local binding.\n\n"))
     )

    (error "stop.")

    ;; Print message like following:
    ;; "Automatically becomes buffer-local when set?"
    (let* ((obsolete (get variable 'byte-obsolete-variable))
           (use (car obsolete))
           (safe-var (get variable 'safe-local-variable))
           (extra-line nil))

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
                   'help-args (list variable file))
                  )
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

    ;; (with-current-buffer standard-output
    ;;   ;; Return the text we displayed.
    ;;   (buffer-string)
    ;;   )
    )
  )
