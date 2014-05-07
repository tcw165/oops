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
         (file-name (find-lisp-object-file-name variable 'defvar))
         (permanent-local (get variable 'permanent-local))
         (obsolete (get variable 'byte-obsolete-variable))
         (safe-var (get variable 'safe-local-variable))
         (print-length 32)
         (print-level 3)
         )
    ;; TODO: Make variable text with hyper-link property.

    ;; Indicate where is its definition and current value.
    (princ (format "`%s'" variable))
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
      (princ "buffer-local when set.")
      )
     ((not permanent-local))
     ((bufferp locus)
      (princ "\n  This variable's buffer-local value is permanent.")
      )
     (t
      (princ "\n  This variable's value is permanent if it is given a local binding."))
     )

    ;; Indicate it is a alias.
    (unless (eq alias variable)
      (princ (format "\n  This variable is an alias for `%s'." alias))
      )

    ;; Indicate ???
    (when obsolete
      (let ((use (car obsolete)))
        (princ "\n  This variable is obsolete")
        (if (nth 2 obsolete)
            (princ (format " since %s" (nth 2 obsolete)))
          )
        (princ (cond 
                ((stringp use)
                 (concat "; " use)
                 )
                (use
                 (format "; use `%s' instead." (car obsolete))
                 )
                (t ".")
                ))
        )
      )

    ;; Indicate ???
    (when (member (cons variable val) file-local-variables-alist)
      (if (member (cons variable val) dir-local-variables-alist)
          ;; dir-local:
          (let ((file (and (buffer-file-name)
                           (not (file-remote-p (buffer-file-name)))
                           (dir-locals-find-file
                            (buffer-file-name))))
                (dir-file t))
            (princ "\n  This variable's value is directory-local")
            (unless (null file)
              (princ ", set ")
              (if (consp file) ; result from cache
                  ;; If the cache element has an mtime, we
                  ;; assume it came from a file.
                  (if (nth 2 file)
                      (setq file (expand-file-name
                                  dir-locals-file (car file)))
                    ;; Otherwise, assume it was set directly.
                    (setq dir-file nil)
                    )
                )
              ;; TODO: There was a xref-button, something is missing.
              (princ (if dir-file
                         "by the file."
                       "for the directory."))
              )
            )
        ;; file-local:
        (princ "\n  This variable's value is file-local.")
        )
      )

    ;; Indicate ???
    (when (memq variable ignored-local-variables)
      (princ "\n  This variable is ignored as a file-local variable."))

    ;; Indicate it could be both risky and safe, eg auto-fill-function.
    (when (risky-local-variable-p variable)
      (princ "\n  This variable may be risky if used as a file-local variable.")
      (when (assq variable safe-local-variable-values)
        (princ "\n  However, you have added it to `safe-local-variable-values'.")
        )
      )

    ;; Indicate ???
    (when safe-var
      (princ "\n  This variable is safe as a file local variable.")
      (princ "\n  If its value, satisfies the predicate ")
      (princ (if (byte-code-function-p safe-var)
                 "which is a byte-compiled expression."
               (format "`%s'." safe-var)))
      )

    ;; Documentation.
    (terpri)
    (terpri)
    (princ "Documentation:\n")
    (princ (or doc "Not documented as a variable."))
    )

  ;; Make a link to customize if this variable can be customized.
  ;; (when (custom-variable-p variable)
  ;;   (let ((customize-label "customize"))
  ;;     (terpri)
  ;;     (terpri)
  ;;     (princ (concat "You can " customize-label " this variable."))
  ;;     (with-current-buffer standard-output
  ;;       (save-excursion
  ;;         (re-search-backward
  ;;          (concat "\\(" customize-label "\\)") nil t)
  ;;         (help-xref-button 1 'help-customize-variable variable)
  ;;         )
  ;;       )
  ;;     )
  ;;   ;; Note variable's version or package version
  ;;   (let ((output (describe-variable-custom-version-info variable)))
  ;;     (when output
  ;;       (terpri)
  ;;       (terpri)
  ;;       (princ output)
  ;;       )
  ;;     )
  ;;   )
  )
