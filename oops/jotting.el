(keymap
 (menu-bar keymap
           (lisp "Lisp" keymap
                 (ind-sexp menu-item "Indent sexp" indent-sexp :help "Indent each line of the list starting just after point")
                 (ev-def menu-item "Eval defun" lisp-eval-defun :help "Send the current defun to the Lisp process made by M-x run-lisp")
                 (run-lisp menu-item "Run inferior Lisp" run-lisp :help "Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'")
                 "Lisp"))
 (3 keymap
    (26 . run-lisp))
 (27 keymap
     (24 . lisp-eval-defun))
 keymap
 (127 . backward-delete-char-untabify)
 (27 keymap
     (17 . indent-sexp)))
