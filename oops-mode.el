(require 'oops-core)
(require 'oops-lisp-lib)
(require 'oops-settings)

(defvar oops-idle-timer-for-definition nil
  "")

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

(defun oops-show-definition-at-point ()
  ""
  (message "hello")
  )

(defun oops-show-definition-window ()
  ""
  (interactive)
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
      (setq oops-idle-timer-for-definition (run-with-idle-timer 0.2 t 'oops-show-definition-at-point))
    (cancel-timer oops-idle-timer-for-definition)
    )
  )

;;;###autoload
(add-hook 'emacs-lisp-mode 'oops-mode)
;;;###autoload
(add-hook 'lisp-interaction-mode 'oops-mode)

(provide 'oops-mode)
