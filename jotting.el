;; subrp
;; symbol-function
;; symbol-file
;; find-function-noselect
;; find-variable-noselect
;; find-function-search-for-symbol

(symbol-function (read "find-function-noselect"))
(symbol-file (read "find-function-noselect"))
(find-function-noselect (read "find-variable-noselect"))
(car (find-function-noselect (read "find-variable-noselect")))
(cdr (find-function-noselect (read "find-variable-noselect")))
(get-file-buffer (expand-file-name "~/.emacs.d/oops/jotting.el"))
(oops-lisp-find-function (read "find-file"))
(generate-new-buffer "*test*")

;; <------------------------------------------------------ test end;

(defun find-function (function)
  "Find the definition of the FUNCTION near point.

Finds the source file containing the definition of the function
near point (selected by `function-called-at-point') in a buffer and
places point before the definition.
Set mark before moving, if the buffer already existed.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non-nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer)
  )

(defun find-function-do-it (symbol type switch-fn)
  "Find Emacs Lisp SYMBOL in a buffer and display it.
TYPE is nil to search for a function definition,
or else `defvar' or `defface'.

The variable `find-function-recenter-line' controls how
to recenter the display.  SWITCH-FN is the function to call
to display and select the buffer.
See also `find-function-after-hook'.

Set mark before moving, if the buffer already existed."
  (let* ((orig-point (point))
         (orig-buf (window-buffer))
         (orig-buffers (buffer-list))
         (buffer-point (save-excursion
                         (find-definition-noselect symbol type)))
         ;; 'find-definition-noselect is the key function.
         ;; 'symbol-function
         (new-buf (car buffer-point))
         (new-point (cdr buffer-point)))
    (when buffer-point
      (when (memq new-buf orig-buffers)
        (push-mark orig-point)
        )
      (funcall switch-fn new-buf)
      (when new-point
        (goto-char new-point)
        )
      (recenter find-function-recenter-line)
      (run-hooks 'find-function-after-hook)
      )
    )
  )

(defun find-function-read (&optional type)
  "Read and return an interned symbol, defaulting to the one near point.

If TYPE is nil, insist on a symbol with a function definition.
Otherwise TYPE should be `defvar' or `defface'.
If TYPE is nil, defaults using `function-called-at-point',
otherwise uses `variable-at-point'."
  (let* ((symb1 (cond ((null type) (function-called-at-point))
                      ((eq type 'defvar) (variable-at-point))
                      (t (variable-at-point t))))
         (symb  (unless (eq symb1 0) symb1))
         (predicate (cdr (assq type '((nil . fboundp)
                                      (defvar . boundp)
                                      (defface . facep)))))
         (prompt-type (cdr (assq type '((nil . "function")
                                        (defvar . "variable")
                                        (defface . "face")))))
         (prompt (concat "Find " prompt-type
                         (and symb (format " (default %s)" symb))
                         ": "))
         (enable-recursive-minibuffers t)
         )
    (list (intern (completing-read
                   prompt obarray predicate
                   t nil nil
                   (and symb (symbol-name symb))
                   )
                  )
          )
    )
  )

(defun find-definition-noselect (symbol type &optional file)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of SYMBOL.
If the definition can't be found in the buffer, return (BUFFER).
TYPE says what type of definition: nil for a function, `defvar' for a
variable, `defface' for a face.  This function does not switch to the
buffer nor display it.

The library where SYMBOL is defined is searched for in FILE or
`find-function-source-path', if non-nil, otherwise in `load-path'."
  (cond
   ((not symbol)
    (error "You didn't specify a symbol")
    )
   ((null type)
    (find-function-noselect symbol)
    )
   ((eq type 'defvar)
    (find-variable-noselect symbol file)
    )
   (t
    (let ((library (or file (symbol-file symbol type))))
      (find-function-search-for-symbol symbol type library)
      )
    )
   )
  )

(defun find-function-noselect (function &optional lisp-only)
  "Return a pair (BUFFER . POINT) pointing to the definition of FUNCTION.

Finds the source file containing the definition of FUNCTION
in a buffer and the point of the definition.  The buffer is
not selected.  If the function definition can't be found in
the buffer, returns (BUFFER).

If FUNCTION is a built-in function, this function normally
attempts to find it in the Emacs C sources; however, if LISP-ONLY
is non-nil, signal an error instead.

If the file where FUNCTION is defined is not known, then it is
searched for in `find-function-source-path' if non-nil, otherwise
in `load-path'."
  (if (not function)
      (error "You didn't specify a function"))
  (let ((def (symbol-function (find-function-advised-original function)))
        aliases)
    ;; FIXME for completeness, it might be nice to print something like:
    ;; foo (which is advised), which is an alias for bar (which is advised).
    (while (symbolp def)
      (or (eq def function)
          (if aliases
              (setq aliases (concat aliases
                                    (format ", which is an alias for `%s'"
                                            (symbol-name def))))
            (setq aliases (format "`%s' is an alias for `%s'"
                                  function (symbol-name def)))
            )
          )
      (setq function (symbol-function (find-function-advised-original function))
            def (symbol-function (find-function-advised-original function)))
      )
    (if aliases
        (message "%s" aliases))
    (let ((library (cond ((autoloadp def)
                          (nth 1 def)
                          )
                         ((subrp def)
                          (if lisp-only
                              (error "%s is a built-in function" function)
                            )
                          (help-C-file-name def 'subr)
                          )
                         ((symbol-file function 'defun)
                          )
                         ))
          )
      ;; KEY !=============================================>
      (find-function-search-for-symbol function nil library)
      ;; <==================================================
      )
    )
  )

(defun find-variable-noselect (variable &optional file)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of VARIABLE.

Finds the library containing the definition of VARIABLE in a buffer and
the point of the definition.  The buffer is not selected.
If the variable's definition can't be found in the buffer, return (BUFFER).

The library where VARIABLE is defined is searched for in FILE or
`find-function-source-path', if non-nil, otherwise in `load-path'."
  (if (not variable)
      (error "You didn't specify a variable")
    (let ((library (or file
                       (symbol-file variable 'defvar)
                       (help-C-file-name variable 'var))))
      (find-function-search-for-symbol variable 'defvar library)
      )
    )
  )

(defun find-function-search-for-symbol (symbol type library)
  "Search for SYMBOL's definition of type TYPE in LIBRARY.
Visit the library in a buffer, and return a cons cell (BUFFER . POSITION),
or just (BUFFER . nil) if the definition can't be found in the file.

If TYPE is nil, look for a function definition.
Otherwise, TYPE specifies the kind of definition,
and it is interpreted via `find-function-regexp-alist'.
The search is done in the source for library LIBRARY."
  (if (null library)
      (error "Don't know where `%s' is defined" symbol)
    )
  ;; Some functions are defined as part of the construct
  ;; that defines something else.
  (while (and (symbolp symbol) (get symbol 'definition-name))
    (setq symbol (get symbol 'definition-name))
    )
  (if (string-match "\\`src/\\(.*\\.\\(c\\|m\\)\\)\\'" library)
      (find-function-C-source symbol (match-string 1 library) type)
    (when (string-match "\\.el\\(c\\)\\'" library)
      (setq library (substring library 0 (match-beginning 1)))
      )
    ;; Strip extension from .emacs.el to make sure symbol is searched in
    ;; .emacs too.
    (when (string-match "\\.emacs\\(.el\\)" library)
      (setq library (substring library 0 (match-beginning 1)))
      )
    (let* ((filename (find-library-name library))
           (regexp-symbol (cdr (assq type find-function-regexp-alist))))
      (with-current-buffer (find-file-noselect filename)
        (let ((regexp (format (symbol-value regexp-symbol)
                              ;; Entry for ` (backquote) macro in loaddefs.el,
                              ;; (defalias (quote \`)..., has a \ but
                              ;; (symbol-name symbol) doesn't.  Add an
                              ;; optional \ to catch this.
                              (concat "\\\\?"
                                      (regexp-quote (symbol-name symbol)))))
              (case-fold-search)
              )
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
                (progn
                  (beginning-of-line)
                  (cons (current-buffer) (point)))
              (cons (current-buffer) nil))
            )
          )
        )
      )
    )
  )

(defun function-called-at-point ()
  "Return a function around point or else called by the list containing point.
If that doesn't give a function, return nil."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (or (condition-case ()
            (save-excursion
              (or (not (zerop (skip-syntax-backward "_w")))
                  (eq (char-syntax (following-char)) ?w)
                  (eq (char-syntax (following-char)) ?_)
                  (forward-sexp -1)
                  )
              (skip-chars-forward "'")
              (let ((obj (read (current-buffer))))
                (and (symbolp obj) (fboundp obj) obj)
                )
              )
          (error nil)
          )
        (condition-case ()
            (save-excursion
              (save-restriction
                (narrow-to-region (max (point-min)
                                       (- (point) 1000)) (point-max))
                ;; Move up to surrounding paren, then after the open.
                (backward-up-list 1)
                (forward-char 1)
                ;; If there is space here, this is probably something
                ;; other than a real Lisp function call, so ignore it.
                (if (looking-at "[ \t]")
                    (error "Probably not a Lisp function call")
                  )
                (let ((obj (read (current-buffer))))
                  (and (symbolp obj) (fboundp obj) obj)
                  )
                )
              )
          (error nil))
        (let* ((str (find-tag-default))
               (sym (if str (intern-soft str))))
          (if (and sym (fboundp sym))
              sym
            (save-match-data
              (when (and str (string-match "\\`\\W*\\(.*?\\)\\W*\\'" str))
                (setq sym (intern-soft (match-string 1 str)))
                (and (fboundp sym) sym)
                )
              )
            )
          )
        )
    )
  )

(defun variable-at-point (&optional any-symbol)
  "Return the bound variable symbol found at or before point.
Return 0 if there is no such symbol.
If ANY-SYMBOL is non-nil, don't insist the symbol be bound."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (or (condition-case ()
	    (save-excursion
	      (skip-chars-forward "'")
	      (or (not (zerop (skip-syntax-backward "_w")))
            (eq (char-syntax (following-char)) ?w)
            (eq (char-syntax (following-char)) ?_)
            (forward-sexp -1))
	      (skip-chars-forward "'")
	      (let ((obj (read (current-buffer))))
          (and (symbolp obj) (boundp obj) obj)
          )
        )
      (error nil))
        (let* ((str (find-tag-default))
               (sym (if str (intern-soft str))))
          (if (and sym (or any-symbol (boundp sym)))
              sym
            (save-match-data
              (when (and str (string-match "\\`\\W*\\(.*?\\)\\W*\\'" str))
                (setq sym (intern-soft (match-string 1 str)))
                (and (or any-symbol (boundp sym)) sym)
                )
              )
            )
          )
        0)
    )
  )

(defun find-variable-noselect (variable &optional file)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of VARIABLE.

Finds the library containing the definition of VARIABLE in a buffer and
the point of the definition.  The buffer is not selected.
If the variable's definition can't be found in the buffer, return (BUFFER).

The library where VARIABLE is defined is searched for in FILE or
`find-function-source-path', if non-nil, otherwise in `load-path'."
  (if (not variable)
      (error "You didn't specify a variable")
    (let ((library (or file
                       (symbol-file variable 'defvar)
                       (help-C-file-name variable 'var))))
      (find-function-search-for-symbol variable 'defvar library)
      )
    )
  )
