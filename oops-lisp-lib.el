(require 'oops-core)

(defun oops-lisp-is-library ()
  "Check if the cursor or the selection is on which just after 'require statement.
Return t if is on which just after 'require statement. Other is nil."
  (save-excursion
    (backward-up-list)
    (forward-char 1)
    (if (string-equal "require" (buffer-substring-no-properties (point) (+ (point) 7)))
        t ;; return t.
      nil ;; return nil.
      )
    )
  )

(defun oops-lisp-skip-parenthesis ()
  "Move the point forward 1 word if it's just before a \"(\".
Move the point backward 1 word if it's just after a \")\"."
  (unless mark-active
    (cond
     ;; Move the point forward 1 word if it's just before a "(".
     ((char-equal (get-byte nil "(") (char-after (point)))
      (forward-word 1)
      )
     ;; Move the point backward 1 word if it's just after a ")".
     ((char-equal (get-byte nil ")") (char-before (point)))
      (backward-word 1)
      )
     )
    )
  )

(defun oops-lisp-find-definition-at-point ()
  ""
  ;; Force to skip parenthesis.
  (oops-lisp-skip-parenthesis)
  (let* ((is-lib (oops-lisp-is-library))
         (text (oops-thing-at-point))
         (symb (read text))
         )
    ;; Force to unselect text.
    (deactivate-mark)
    (cond
     ;; library
     ((and is-lib (not (string-equal "require" text)))
      (find-library text)
      (message "library: %s" text)
      ;; TODO: go to (require 'text) line
      )
     ;; Function:
     ((fboundp symb)
      (find-function symb)
      (message "function: %s" text)
      )
     ;; Variable:
     ((boundp symb)
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
