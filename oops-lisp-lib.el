(require 'oops-core)

(defun oops-lisp-find-function (function)
  ""
  (let* ((def (symbol-function function))
         (library (cond ((autoloadp def)
                         (nth 1 def))
                        (t
                         (symbol-file function 'defun))
                        )))
    (find-function-search-for-symbol function nil library)
    )
  )

(defun oops-lisp-find-variable (variable)
  ""
  )

(defun oops-lisp-find-symbol (symb &optional type)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of `symb'.
`symb' should be a lisp symbol, string is prohibited.

The `type' should be `defun' or `defvar'. If `type' is nil, indicate the `symb' is a function.

TODO/FIXME:
* Advising function list."
  (interactive)
  (cond
   ;; Function
   ((or (null type)
        (eq type 'defun))
    (oops-lisp-find-function symb)
    )
   ;; Variable
   ((eq type 'defvar)
    (oops-lisp-find-variable symb)
    )
   ;; Unknown
   (t nil)
   )
  )

(defun oops-lisp-find-definition-at-point ()
  ""
  ;; TODO/FIXME:
  ;; * advising function list!
  ;; * variable and function with same name!
  ;; * add local variable navigation!
  (let* ((text (oops-thing-at-point))
         (symb (and (not (null text))
                    (read text)))
         (is-var-fun (and (not (null symb))
                          (boundp symb)
                          (fboundp symb)))
         )
    ;; Force to unselect text.
    (deactivate-mark)
    (cond
     ;; nil
     ((or (null text)
          (null symb))
      ;; DO NOTHING
      nil
      )
     ;; library
     ((and symb (featurep symb))
      (find-library text)
      ;; TODO/FIXME: go to (require 'text) line
      (message "library: %s" text)
      )
     ;; TODO/FIXME =======================================================>
     ;; ;; Variable and Function with the same name:
     ;; (is-var-fun
     ;;  (message "%s is type of \"is-var-fun\" and is not ready yet" text)
     ;;  )
     ;; <==================================================================
     ;; Function:
     ((and symb (fboundp symb))
      (find-function symb)
      (message "function: %s" text)
      )
     ;; Variable:
     ((and symb (boundp symb))
      (find-variable symb)
      (message "variable: %s" text)
      )
     ;; Unknown symbol:
     (t
      (message "unknown symbol: %s" text)
      )
     )
    )
  )

(provide 'oops-lisp-lib)
