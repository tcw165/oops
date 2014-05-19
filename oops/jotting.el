(kill-local-variable 'hl-paren-overlays)
(mapc 'delete-overlay hl-paren-overlays)
(hl-paren-create-overlays)
(defun test ()
  (interactive)
  (goto-char (scan-sexps (point) 1))
  )

(((0 1) (syntax-ppss nil)))
;; (2 693 700 nil nil nil 0 nil nil (692 693))
((((0 1) (syntax-ppss nil) (2 3))))
(parse-partial-sexp)

 ;; 0. depth in parens.
 ;; 1. character address of start of innermost containing list; nil if none.
 ;; 2. character address of start of last complete sexp terminated.
 ;; 3. non-nil if inside a string.
 ;;    (it is the character that will terminate the string,
 ;;     or t if the string should be terminated by a generic string delimiter.)
 ;; 4. nil if outside a comment, t if inside a non-nestable comment,
 ;;    else an integer (the current comment nesting).
 ;; 5. t if following a quote character.
 ;; 6. the minimum paren-depth encountered during this scan.
 ;; 7. style of comment, if any.
 ;; 8. character address of start of comment or string; nil if not in one.
 ;; 9. Intermediate data for continuation of parsing (subject to change).

(defface highlight
  ’((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
   "Basic face for highlighting."
   :group ’basic-faces)

