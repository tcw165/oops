(require 'find-func)
(require 'help-fns)

;; =============================================================================

(defun oops--lisp-thingatpt ()
  "Return string on which the point is or just string of selection."
  (if mark-active
      ;; return the selection.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; else.
    (thing-at-point 'symbol)
    )
  )

;; History Navigation ==========================================================

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

(defun test4 ()
  (test3)
  )
;; (setq oops--lisp-history nil)
;; (setq oops--lisp-history-indicator nil)
;; (message "oops--lisp-history (%s):\n%s\n\noops--lisp-history-indicator (%s):\n%s" (length oops--lisp-history) oops--lisp-history (length oops--lisp-history-indicator) oops--lisp-history-indicator)
;; <==================== test code

(defvar oops--lisp-history nil
  "The history is a list containing records with following format:

\('symbol-record SYMBOL TYPE BUFFER\)
   This kind of record is for the time after jumping to the definition.
   TYPE is one of 'defun, 'defvar or 'defface.

\('marker-record SYMBOL MARKER\)
   This kind of record is for the time before jumping to the definition.

The 1st element of all the records is RECORD-TYPE, which value is 'marker-record or 'symbol-record.")

(defvar oops--lisp-history-indicator nil
  "An history indicator that points to the record you access last time.")


(defun oops--lisp-push-history (record-type symbol &optional type buffer)
  "Push the current state as a record into history. Check `oops--lisp-history' for the details."
  (let ((record (cond
                 ((eq record-type 'symbol-record)
                  (list 'symbol-record symbol type buffer)
                  )
                 ((eq record-type 'marker-record)
                  (list 'marker-record symbol (copy-marker (set-marker (mark-marker) (point))))
                  )
                 )))
    (when record
      ;; Push history.
      (if (null oops--lisp-history)
          ;; 1st record.
          (setq oops--lisp-history (list record))
        ;; else.
        ;; Discard records which are behind the indicator.
        (let* ((full-length (length oops--lisp-history))
               (indicator-length (length oops--lisp-history-indicator))
               (indicator-index (- full-length indicator-length))
               (before-index (1- indicator-index)))
          (setq oops--lisp-history (if (and (> full-length 1)
                                            (eq 'marker-record (caar oops--lisp-history-indicator))
                                            (eq 'symbol-record (car (nth before-index oops--lisp-history)))
                                            )
                                       ;; If the current record and previos record is like a pair of (('marker-record ...) ('symbol-record ...)), discard them both.
                                       (cdr oops--lisp-history-indicator)
                                     ;; Or just discard records behind.
                                     oops--lisp-history-indicator))
          )
        ;; Concatenate record.
        (setq oops--lisp-history (cons record oops--lisp-history))
        )

      ;; Update indicator.
      (setq oops--lisp-history-indicator oops--lisp-history)

      ;; Keep the lenght less than maximum length.
      (when (> (length oops--lisp-history) oops-history-max)
        (setcdr (nthcdr (1- oops-history-max) oops--lisp-history) nil)
        )
      ;; (message "oops--lisp-history (%s):\n%s\n\noops--lisp-history-indicator (%s):\n%s"
      ;;          (length oops--lisp-history)
      ;;          oops--lisp-history
      ;;          (length oops--lisp-history-indicator)
      ;;          oops--lisp-history-indicator)
      )
    )
  )

(defun oops--lisp-use-history ()
  (let* ((record (car oops--lisp-history-indicator))
         (record-type (car record)))
    (cond
     ;; ('symbol-record SYMBOL TYPE BUFFER):
     ((eq record-type 'symbol-record)
      (let* ((symbol (nth 1 record))
             (type (nth 2 record))
             (predicate (cdr (assq type '((defun . oops--lisp-find-function)
                                          (defvar . oops--lisp-find-variable)))))
             (search-result (funcall predicate symbol))
             (buffer (nth 0 search-result))
             (beg (nth 1 search-result))
             (end (nth 2 search-result))
             )
        ;; Switch buffer.
        (switch-to-buffer buffer)
        ;; Disable region.
        (setq mark-active nil)
        (goto-char end)
        ;; Enable region.
        (set-marker (mark-marker) beg)
        (unless (= beg end)
          (setq mark-active t)
          )
        )
      )
     ;; ('marker-record SYMBOL MARKER)
     ((eq record-type 'marker-record)
      (let ((buffer (marker-buffer (nth 2 record)))
            (pos (marker-position (nth 2 record))))
        ;; Switch buffer.
        (switch-to-buffer buffer)
        ;; Disable region.
        (setq mark-active nil)
        (goto-char pos)
        )
      )
     )
    ;; ;; Debug message.
    ;; (message "oops--lisp-history (%s):\n%s\n\noops--lisp-history-indicator (%s):\n%s"
    ;;            (length oops--lisp-history)
    ;;            oops--lisp-history
    ;;            (length oops--lisp-history-indicator)
    ;;            oops--lisp-history-indicator)
    )
  )

(defun oops-lisp-prev-history ()
  "Navigate to previous history by rotating the `oops--lisp-history'.
\(1 2 3 4 5\) => \(2 3 4 5 1\) and use \(2\) history."
  (interactive)
  (if (and oops--lisp-history
           (> (length oops--lisp-history) 0))
      (progn
        ;; Discard invalid records which refer to killed buffers:
        (let ((record (car oops--lisp-history-indicator)))
          (while (or (and (eq 'marker-record (car record))
                          (not (marker-buffer (nth 2 record))))
                     (and (eq 'symbol-record (car record))
                          (not (buffer-live-p (nth 3 record)))))
            (setq oops--lisp-history (delq record oops--lisp-history))
            (setq oops--lisp-history-indicator (cdr oops--lisp-history-indicator))
            (setq record (car oops--lisp-history-indicator))
            )
          )

        ;; Move the indicator.
        (unless (null (cdr oops--lisp-history-indicator))
          (setq oops--lisp-history-indicator (cdr oops--lisp-history-indicator))
          (oops--lisp-use-history)
          )
        (let ((i (length oops--lisp-history))
              (j (length oops--lisp-history-indicator)))
          (message "[History] navigate to previous record: %s\/%s" j i)
          )
        )
    (message "[History] no record was set!")
    )
  )

(defun oops-lisp-next-history ()
  "Navigate to next history by rotating the `oops--lisp-history'.
\(2 3 4 5 1\) => \(1 2 3 4 5\) and use \(1\) history."
  (interactive)

  (if (and oops--lisp-history
           (> (length oops--lisp-history) 0))
      (progn
        ;; ;; Discard invalid records which refer to killed buffers.
        ;; ;; Case 1: ('marker-record SYMBOL MARKER)
        ;; (while (and oops--lisp-history
        ;;             (eq 'marker-record (caar (last oops--lisp-history)))
        ;;             (not (marker-buffer (nth 2 (car (last oops--lisp-history))))))
        ;;   (setq oops--lisp-history (butlast oops--lisp-history))
        ;;   )
        ;; ;; Case 2: ('symbol-record SYMBOL TYPE BUFFER)
        ;; (while (and oops--lisp-history
        ;;             (eq 'symbol-record (caar (last oops--lisp-history)))
        ;;             (not (buffer-live-p (nth 3 (car (last oops--lisp-history))))))
        ;;   (setq oops--lisp-history (butlast oops--lisp-history))
        ;;   )

        ;; Move the indicator.
        (unless (eq oops--lisp-history-indicator oops--lisp-history)
          (let* ((full-length (length oops--lisp-history))
                 (indicator-length (length oops--lisp-history-indicator))
                 (indicator-index (- full-length indicator-length))
                 (before-index (1- indicator-index)))
            (setq oops--lisp-history-indicator (nthcdr before-index oops--lisp-history))
            )
          (oops--lisp-use-history)
          )
        (let ((i (length oops--lisp-history))
              (j (length oops--lisp-history-indicator)))
          (message "[History] navigate to next record: %s\/%s" j i)
          )
        )
    (message "[History] no record was set!")
    )
  )

;; Help ========================================================================

(defun oops--lisp-help-buffer (&optional clean-buf)
  (let ((buf (get-buffer-create "**Help**")))
    (when (and clean-buf
               (booleanp clean-buf))
      (with-current-buffer buf
        (erase-buffer))
      )
    buf
    )
  )

(defun oops--lisp-describe-function (symbol)
  "Display the full documentation of FUNCTION \(a symbol\).
\(It was written by refering to the GNU functions, `describe-function' and `describe-function-1'.\)
"
  (let* ((standard-output (oops--lisp-help-buffer t))
         (advised (and (featurep 'advice)
                       (ad-get-advice-info symbol)))
         (real-function (or (and advised
                                 (let ((origname (cdr (assq 'origname advised))))
                                   (and (fboundp origname) origname)))
                            symbol))
         (def (symbol-function real-function))
         (file-name (find-lisp-object-file-name symbol def))
         (doc (documentation symbol))
         (aliased (symbolp def))
         (real-def (if aliased
                       (let ((f def))
                         (while (and (fboundp f)
                                     (symbolp (symbol-function f)))
                           (setq f (symbol-function f))
                           )
                         f
                         )
                     def
                     ))
         (tmp-string (if (and (or (byte-code-function-p def)
                                  (keymapp def)
                                  (memq (car-safe def) '(macro lambda closure)))
                              file-name
                              (help-fns--autoloaded-p symbol file-name))
                         (if (commandp def)
                             "an interactive autoloaded "
                           "an autoloaded "
                           )
                       (if (commandp def)
                           "an interactive "
                         "a "
                         )
                       ))
         )

    ;; Print what kind of function-like object FUNCTION is.
    (princ (format "%s is " symbol))
    (princ (cond ((or (stringp def) (vectorp def))
                  "a keyboard macro")
                 ((subrp def)
                  (if (eq 'unevalled (cdr (subr-arity def)))
                      (concat tmp-string "special form")
                    (concat tmp-string "built-in function")
                    )
                  )
                 ((byte-code-function-p def)
                  (concat tmp-string "compiled Lisp function")
                  )
                 (aliased
                  (format "an alias for `%s'" real-def)
                  )
                 ((eq (car-safe def) 'lambda)
                  (concat tmp-string "Lisp function")
                  )
                 ((eq (car-safe def) 'macro)
                  (concat tmp-string "Lisp macro")
                  )
                 ((eq (car-safe def) 'closure)
                  (concat tmp-string "Lisp closure")
                  )
                 ((autoloadp def)
                  (format "%s autoloaded %s"
                          (if (commandp def) "an interactive" "an")
                          (if (eq (nth 4 def) 'keymap) "keymap"
                            (if (nth 4 def) "Lisp macro" "Lisp function")))
                  )
                 ((keymapp def)
                  (let ((is-full nil)
                        (elts (cdr-safe def)))
                    (while elts
                      (if (char-table-p (car-safe elts))
                          (setq is-full t
                                elts nil))
                      (setq elts (cdr-safe elts)))
                    (concat tmp-string (if is-full
                                           "keymap"
                                         "sparse keymap"
                                         )))
                  )
                 (t "")
                 ))

    ;; Indicate where it is from and its documentation.
    ;; We used to add .el to the file name, but that's completely wrong when the user used load-file.
    (when file-name
      (princ " in \"")
      (princ (if (eq file-name 'C-source)
                 "C source code"
               (file-name-nondirectory file-name)))
      (princ "\"")
      )
    (princ ".")
    (terpri)
    (terpri)

    ;; Print documentation.
    (with-current-buffer standard-output
      (help-fns--key-bindings symbol)
      (help-fns--compiler-macro symbol)
      (help-fns--parent-mode symbol)
      (help-fns--obsolete symbol)
      (setq doc (help-fns--signature symbol doc real-def real-function))

      (terpri)
      (if doc
          (princ (format "Documentation:\n%s" doc))
        (princ "No documentation.")
        )

      (goto-char 1)
      )
    ;; Return buffer.
    standard-output
    )
  )

(defun oops--lisp-describe-variable (symbol)
  "Display the full documentation of VARIABLE \(a symbol\).
\(It was written by refering to the GNU function, `describe-variable'.\)
"
  (let* ((standard-output (oops--lisp-help-buffer t))
         (alias (condition-case nil
                    (indirect-variable symbol)
                  (error symbol)))
         (doc (or (documentation-property
                   symbol 'variable-documentation)
                  (documentation-property
                   alias 'variable-documentation)))
         (val (symbol-value symbol))
         (locus (variable-binding-locus symbol))
         (file-name (find-lisp-object-file-name symbol 'defvar))
         (permanent-local (get symbol 'permanent-local))
         (obsolete (get symbol 'byte-obsolete-variable))
         (safe-var (get symbol 'safe-local-variable))
         (print-length 32)
         (print-level 3)
         )
    ;; TODO: Make symbol text with hyper-link property.

    ;; Indicate where is its definition and current value.
    (princ (format "%s" symbol))
    (princ " is a symbol defined in \"")
    (princ (if (eq file-name 'C-source)
               "C source code"
             (file-name-nondirectory file-name)))
    (princ "\".")
    (terpri)
    (terpri)
    (princ (format "Its value is %S." val))
    ;; Add hyper-link property to file-name text.
    ;; (with-current-buffer standard-output
    ;;   (save-excursion
    ;;     (re-search-backward "`\\([^`']+\\)'" nil t)
    ;;     (help-xref-button 1 'help-variable-def
    ;;                       symbol file-name)
    ;;     )
    ;;   )

    ;; If it is a customizable variable, show its original value.
    ;; "Original value was?"
    (let* ((sv (get symbol 'standard-value))
           (origval (and (consp sv)
                         (condition-case nil
                             (eval (car sv))
                           (error :help-eval-error)))))
      (when (and (consp sv)
                 (not (equal origval val))
                 (not (equal origval :help-eval-error)))
        (princ "\ncustomizable, original value was `")
        (pp origval)
        (princ "'.")
        )
      )

    ;; Indicate which kind of variable it is and its defaulte value.
    ;; "Local in buffer?"
    ;; "Frame-local variable?"
    ;; "Terminal-local variable?"
    (when locus
      (cond
       ((bufferp locus)
        (princ (format "\nlocal in buffer `%s'." (buffer-name)))
        )
       ((framep locus)
        (princ (format "\nframe-local variable."))
        )
       ((terminal-live-p locus)
        (princ (format "\nterminal-local variable."))
        )
       (t (princ (format "\nit is local to %S." locus)))
       )

      (if (not (default-boundp symbol))
          (princ "\nglobally void")
        (let ((global-val (default-value symbol)))
          (princ "\nglobal value is ")
          (if (eq val global-val)
              (princ "the same.")
            (princ "`")
            (pp global-val)
            (princ "'.")
            )
          )
        )
      )

    ;; Indicate which kind of local variable it is.
    (terpri)
    (cond
     ((and (local-variable-if-set-p symbol)
           (or (not (local-variable-p symbol))
               (with-temp-buffer
                 (local-variable-if-set-p symbol))))
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
    (unless (eq alias symbol)
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
    (when (member (cons symbol val) file-local-variables-alist)
      (if (member (cons symbol val) dir-local-variables-alist)
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
    (when (memq symbol ignored-local-variables)
      (princ "\n  This variable is ignored as a file-local variable."))

    ;; Indicate it could be both risky and safe, eg auto-fill-function.
    (when (risky-local-variable-p symbol)
      (princ "\n  This variable may be risky if used as a file-local variable.")
      (when (assq symbol safe-local-variable-values)
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

    ;; Make a link to customize if this variable can be customized.
    ;; (when (custom-variable-p symbol)
    ;;   (let ((customize-label "customize"))
    ;;     (terpri)
    ;;     (terpri)
    ;;     (princ (concat "You can " customize-label " this variable."))
    ;;     (with-current-buffer standard-output
    ;;       (save-excursion
    ;;         (re-search-backward
    ;;          (concat "\\(" customize-label "\\)") nil t)
    ;;         (help-xref-button 1 'help-customize-variable symbol)
    ;;         )
    ;;       )
    ;;     )
    ;;   ;; Note variable's version or package version
    ;;   (let ((output (describe-variable-custom-version-info symbol)))
    ;;     (when output
    ;;       (terpri)
    ;;       (terpri)
    ;;       (princ output)
    ;;       )
    ;;     )
    ;;   )

    (with-current-buffer standard-output
      (goto-char 1)
      )
    ;; Return buffer.
    standard-output
    )
  )

(defun oops-lisp-show-help-atpt ()
  (let* ((symb (intern-soft (oops--lisp-thingatpt)))
         search-result)
    (when symb
      (cond
       ;; TODO: Support feature.
       ;; Library:
       ;; ((featurep symb)
       ;;  nil
       ;;  )
       ;; Function:
       ((fboundp symb)
        (setq search-result (oops--lisp-find-function symb))
        (if search-result
            (oops-update-help search-result)
          ;; Built-in function, show HELP.
          (oops-update-help (oops--lisp-describe-function symb))
          )
        )
       ;; Variable:
       ((boundp symb)
        (setq search-result (oops--lisp-find-variable symb))
        (if search-result
            (oops-update-help search-result)
          ;; Built-in variable, show HELP.
          (oops-update-help (oops--lisp-describe-variable symb))
          )
        )
       )
      )
    )
  )

;; Source Code Navigation ======================================================

(defconst oops--lisp-search-symbol-regexp-alist
  '((defun . find-function-regexp)
    (defvar . find-variable-regexp)
    (defface . find-face-regexp))
  "Alist mapping definition types into regexp variables. Each regexp variable's value should actually be a format string to be used to substitute the desired symbol name into the regexp.")


(defun oops--lisp-search-for-symbol (symbol type library)
  "Search for SYMBOL's definition of type TYPE in LIBRARY. Visit the library in a buffer, and return a list (BUFFER POS-BEG POS-END), or just nil if the definition can't be found in the file.

TYPE specifies the kind of definition, and it is interpreted via `oops--lisp-search-symbol-regexp-alist'.
\(It was written by refering to GNU function, `find-function-search-for-symbol'.\)
"
  ;; Some functions are defined as part of the construct that defines something else.
  (while (and (symbolp symbol)
              (get symbol 'definition-name))
    (setq symbol (get symbol 'definition-name))
    )

  (cond
   ((string-match "\\.el\\(c\\)\\'" library)
    (setq library (substring library 0 (match-beginning 1)))
    )
   ;; Strip extension from .emacs.el to make sure symbol is searched in
   ;; .emacs too.
   ((string-match "\\.emacs\\(.el\\)" library)
    (setq library (substring library 0 (match-beginning 1)))
    )
   )

  (let* ((filename (find-library-name library))
         (regexp-symbol (cdr (assq type oops--lisp-search-symbol-regexp-alist))))
    (save-excursion
      (with-current-buffer (find-file-noselect filename)
        (let ((regexp (format (symbol-value regexp-symbol)
                              ;; Entry for ` (backquote) macro in loaddefs.el,
                              ;; (defalias (quote \`)..., has a \ but
                              ;; (symbol-name symbol) doesn't.  Add an
                              ;; optional \ to catch this.
                              (concat "\\\\?"
                                      (regexp-quote (symbol-name symbol)))))
              (case-fold-search))
          (with-syntax-table emacs-lisp-mode-syntax-table
            (goto-char (point-min))
            (if (or (re-search-forward regexp nil t)
                    ;; `regexp' matches definitions using known forms like
                    ;; `defun', or `defvar'.  But some functions/variables
                    ;; are defined using special macros (or functions), so
                    ;; if `regexp' can't find the definition, we look for
                    ;; something of the form "(SOMETHING <symbol> ...)".
                    ;; This fails to distinguish function definitions from
                    ;; variable declarations (or even uses thereof), but is
                    ;; a good pragmatic fallback.
                    (re-search-forward
                     (concat "^([^ ]+" find-function-space-re "['(]?"
                             (regexp-quote (symbol-name symbol))
                             "\\_>")
                     nil t))
                ;; Return the struct.
                (let ((end (point))
                      (beg (progn
                             (forward-thing 'symbol -1)
                             (point)
                             )))
                  (list (current-buffer) beg end)
                  )
              ;; Not found, return nil
              nil
              )
            )
          )
        )
      )
    )
  )

(defun oops--lisp-find-function (symbol)
  "Return a list (BUFFER POS-BEG POS-END) pointing to the definition of FUNCTION. Return nil if symbol is a built-in function.
\(It was written by refering to GNU function, `find-function-noselect'.\)
"
  (let ((def (symbol-function (find-function-advised-original symbol)))
        aliases)
    ;; FIXME for completeness, it might be nice to print something like:
    ;; foo (which is advised), which is an alias for bar (which is advised).
    (while (symbolp def)
      (or (eq def symbol)
          (if aliases
              (setq aliases (concat aliases
                                    (format ", which is an alias for `%s'"
                                            (symbol-name def))))
            (setq aliases (format "`%s' is an alias for `%s'"
                                  symbol (symbol-name def)))
            ))
      (setq symbol (symbol-function (find-function-advised-original symbol))
            def (symbol-function (find-function-advised-original symbol)))
      )
    (and aliases (message "%s" aliases))

    ;; Find library and return the result.
    (let ((library (cond ((autoloadp def)
                          (nth 1 def)
                          )
                         ((subrp def) nil)
                         ((symbol-file symbol 'defun))
                         )))
      (and library (oops--lisp-search-for-symbol symbol 'defun library))
      )
    )
  )

(defun oops--lisp-find-variable (symbol)
  "Return a list (BUFFER POS-BEG POS-END) pointing to the definition of VARIABLE. Return nil if symbol is a built-in variable.
\(It was written by refering to GNU function, `find-variable-noselect'.\)
"
  (let ((library (symbol-file symbol 'defvar)))
    (and library (oops--lisp-search-for-symbol symbol 'defun library))
    )
  )

(defun oops--lisp-find-library (symbol)
  "Return a list (BUFFER POS-BEG POS-END) pointing to the definition of LIBRARY.
\(It was written by refering to GNU function, `find-library'.\)
"
  ;; TODO: implement it!
  )

(defun oops-lisp-jump-to-definition-atpt ()
  ;; TODO:
  ;; * advising function list!
  ;; * variable, function and feature with same name!
  ;; * add local variable navigation!
  (let* ((symb (intern-soft (oops--lisp-thingatpt)))
         search-result)

    ;; Disable region.
    (setq mark-active nil)
    ;; oops-mode

    (cond
     ;; Function:
     ((fboundp symb)
      (setq search-result (oops--lisp-find-function symb))
      (if (not search-result)
          ;; Built-in function.
          (message "[Definition] No support for built-in function: %s." symb)
        ;; Lisp function.
        ;; Save current state before switching.
        (oops--lisp-push-history 'marker-record symb)

        (switch-to-buffer (car search-result))

        ;; Save new state after switching.
        (oops--lisp-push-history 'symbol-record symb 'defun (car search-result))

        ;; Enable region.
        (set-marker (mark-marker) (nth 1 search-result))
        (goto-char (nth 2 search-result))
        (unless (= (marker-position (mark-marker)) (point))
          (setq mark-active t)
          )
        (message "[Definition] function: %s" symb)
        )
      )

     ;; Variable:
     ((boundp symb)
      ;; (find-function-do-it symb 'defvar 'switch-to-buffer)
      ;; Lisp variable.
      (setq search-result (oops--lisp-find-variable symb))
      (if (not search-result)
          ;; Built-in variable.
          (message "[Definition] No support for built-in variable: %s." symb)
        ;; Save current state before switching.
        (oops--lisp-push-history 'marker-record symb)

        (switch-to-buffer (car search-result))

        ;; Save new state after switching.
        (oops--lisp-push-history 'symbol-record symb 'defvar (car search-result))

        ;; Enable region.
        (set-marker (mark-marker) (nth 1 search-result))
        (goto-char (nth 2 search-result))
        (unless (= (marker-position (mark-marker)) (point))
          (setq mark-active t)
          )
        (message "[Definition] variable: %s" symb)
        )
      )

     ;; library
     ((featurep symb)
      (find-library (symbol-name symb))
      ;; TODO: go to (require 'text) line
      (message "[Definition] library: %s" symb)
      )
     )
    )
  )

;; require `imenu'.
(defun oops-lisp-goto-lsymb (&optional symbol-list)
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
               (oops-lisp-goto-lsymb (imenu--make-index-alist))
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
          (oops-lisp-goto-lsymb symbol))
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

(provide 'oops-lisp-lib)
