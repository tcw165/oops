(defun test ()
  (interactive)
  (let ((symbol (intern-soft (oops--lisp-thingatpt))))
    (message "%s" symbol)
    )
  )
(symbol-value :foreground)
(symbol-file :foreground)
(oops--lisp-find-variable :foreground)
(oops--lisp-describe-variable :forground)

(font-family-list)

(add-text-properties)
(link-start)

(face-list)
(propertize "foo" 'face 'italic 'mouse-face 'bold-italic)

(concat (propertize "foo" 'face 'italic 'mouse-face 'bold-italic)
        " and ")

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

