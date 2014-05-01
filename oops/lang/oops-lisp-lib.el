;; =============================================================================

(defun oops-lisp-show-definition-atpt ()
  ;; (let* ((orig-point (point))
  ;;        (orig-buf (window-buffer))
  ;;        (orig-buffers (buffer-list))
  ;;        (buffer-point (save-excursion
  ;;                        (find-definition-noselect symbol type)))
  ;;        (new-buf (car buffer-point))
  ;;        (new-point (cdr buffer-point)))
  ;;   (when buffer-point
  ;;     (when (memq new-buf orig-buffers)
  ;;       (push-mark orig-point)
  ;;       )
  ;;     (funcall switch-fn new-buf)
  ;;     (when new-point
  ;;       (goto-char new-point)
  ;;       )
  ;;     (recenter find-function-recenter-line)
  ;;     (run-hooks 'find-function-after-hook))
  ;;   )
  (let* ((text (oops-thing-at-point))
         (symb (and (not (null text))
                    (read text)))
         (buffer-point (cond
                        ;; Native function or variable:
                        ((subrp symb)
                         nil
                         )
                        ;; Library:
                        ((featurep symb)
                         nil
                         )
                        ;; Function:
                        ((fboundp symb)
                         (save-excursion
                           (find-function-noselect symb t)
                           )
                         )
                        ;; Variable:
                        ((boundp symb)
                         (save-excursion
                           (find-variable-noselect symb)
                           )
                         )))
         )
    (when buffer-point
      (message "Symbol (%s) is at %s" symb buffer-point)
      (oops-update-help buffer-point)
      )
    )
  )

;; =============================================================================

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

(defun oops-lisp-jump-to-definition-atpt ()
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
