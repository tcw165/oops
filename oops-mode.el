;; [buffer-local] list containing definition windows.
(make-variable-buffer-local
 (defvar oops-def-window nil
   "Definition window that showing the definition. It's a buffer-local variable.")
 )

;;;###autoload
(define-minor-mode oops-mode
  "Constructing..."
  :lighter " Oops"
  (message "%s" oops-mode)
  )

(add-hook 'emacs-lisp-mode 'oops-mode)
(add-hook 'lisp-interaction-mode 'oops-mode)
