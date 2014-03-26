;; Define a variable named name-mode. Make it buffer-local.
(defvar refill-mode nil
  "Mode variable for refill minor mode.")
(make-variable-buffer-local 'refill-mode)

;; Define a command called name-mode.
(defun refill-mode (&optional arg)
  "Refill minor mode."
  (interactive "P")
  (setq refill-mode
        (if (null arg)
            ;; if
            (not refill-mode)
          ;; else
          (> (prefix-numeric-value arg) 0)
          )
        )
;;  (make-local-hook 'after-change-functions)
  (if refill-mode
      ;; code for turning on
      (add-hook 'after-change-functions 'refill nil t)
    ;; code for turning off
    (remove-hook 'after-change-functions 'refill t)
    )
  )

;; mode lighter.
(if (not (assq 'refill-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(refill-mode " Refill") minor-mode-alist))
    )

;; hook in 'after-change-functions
(defun refill (start end len)
  "After a text change, refill the current paragraph."
  (fill-paragraph nil)
  )

(provide 'refill-mode)
