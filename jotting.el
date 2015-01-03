(lexical-let ((count 0) (anm "-/|\\-")
              (end 500) (pos (point))
              (wait-time 50))
  (deferred:$
    (deferred:next
      (lambda (x) (message "Animation started.")))

    (deferred:nextc it
      (deferred:lambda (x)
        (save-excursion
          (when (< 0 count)
            (goto-char pos) (delete-char 1))
          (insert (char-to-string 
                   (aref anm (% count (length anm))))))
        (if (> end (incf count)) ; return nil to stop this loop
            (deferred:nextc (deferred:wait wait-time) self)))) ; return the deferred

    (deferred:nextc it
      (lambda (x)
        (save-excursion
          (goto-char pos) (delete-char 1))
        (message "Animation finished.")))

    
    (deferred:process-shell "ls -al")
    (deferred:nextc it
      (lambda (x)
        (message "%s" x)))))|




(start-process "my-process" "foo" "ls" "-l" "/bin")

(progn
  (let* ((proc-buf (get-buffer-create "foo"))
         (proc (start-process-shell-command "foo" proc-buf "find /Users/boyw165/.emacs.d/oops -name \"*.el\"|xargs grep -nH \"def\" || true")))
    (set-process-sentinel
     proc
     (lambda (proc event)
       ;; Do something.
       ))))

(defmacro aaa:lambda (&rest args)
  (declare (indent defun))
  `(search-create-task
    (lambda ,@args)
    nil))

(aaa:lambda ()
  (message "123")
  (setq a 123))

(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
`funcall' or `mapcar', etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of Lisp expressions.

\(fn ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  (declare (doc-string 2) (indent defun)
           (debug (&define lambda-list
                           [&optional stringp]
                           [&optional ("interactive" interactive)]
                           def-body)))
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  (list 'function (cons 'lambda cdr)))


