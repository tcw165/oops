
;; [buffer-local] noob-mode
(make-variable-buffer-local
 (defvar noob-mode nil
   "noob-mode is a buffer-local variable.")
 )

;; [buffer-local] list containing definition windows.
(make-variable-buffer-local
 (defvar def-windows nil
   "Definition window that showing the definition. It's a buffer-local variable.")
 )

(defun init-def-windows ()
  "Create definition windows refer to how many valid frames."
  (frame-list)
  )

;;;###autoload
(define-minor-mode noob-mode
  "Constructing..."
  :lighter " noob")

;;;###autoload
(add-hook 'emacs-lisp-mode-hook 'noob-mode)

(provide 'noob-mode)

;; frame remove hook to remove definition window

(setq aaa1 t)
(defun aaa1 ()
  "asdfasdfadsf"
  (interactive)
  (if aaa1
      ; if
      (progn
        (message)
        )
  ; else
  (progn
    (message)
    )
  )

(setq win1 (split-window nil nil 'below))
