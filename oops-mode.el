(require 'oops-core)
(require 'oops-lisp-lib)
(require 'oops-settings)

(defun oops-find-definition-at-point ()
  "Find the symbol definition both for function, variable or library."
  (interactive)
  (cond
   ;; major-mode == "emacs-lisp-mode", lisp-interaction-mode
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'lisp-interaction-mode)
        )
    (oops-lisp-find-definition-at-point)
    )
   ;; Clause: major-mode == ...
   ((eq major-mode 'c-mode)
    (message "Yet ready...")
    )
   )
  )

(defun test-hook ()
  ""
  (message "yes")
  )

;;;###autoload
(define-minor-mode oops-mode
  "Constructing..."
  :lighter " Oops"
  (if oops-mode
      (add-hook 'post-command-hook 'test-hook nil t)
    (remove-hook 'post-command-hook 'test-hook t)
    )
  )

;;;###autoload
(add-hook 'emacs-lisp-mode 'oops-mode)
;;;###autoload
(add-hook 'lisp-interaction-mode 'oops-mode)

(provide 'oops-mode)
