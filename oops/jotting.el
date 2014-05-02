;; (help-setup-xref (list #'describe-function function)
;;                  (called-interactively-p 'interactive))

(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
   (let ((fn (function-called-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read (if fn
                                    (format "Describe function (default %s): " fn)
                                  "Describe function: ")
                                obarray 'fboundp t nil nil
                                (and fn (symbol-name fn))))
     (list (if (equal val "")
               fn
             (intern val)
             ))
     )
   )
  (if (null function)
      (message "You didn't specify a function")
    (help-setup-xref (list #'describe-function function)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (prin1 function)
        ;; Use " is " instead of a colon so that
        ;; it is easier to get out the function name using forward-sexp.
        (princ " is ")
        (describe-function-1 function)
        (with-current-buffer standard-output
          ;; Return the text we displayed.
          (buffer-string)))
      )
    )
  )
