;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(require 'package)
(dolist (arch '(("elpa" . "http://melpa.milkbox.net/packages/")
		("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives arch t))
;; Install required packages.

;; Update `load-path' refer to `package-load-list'.
; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(package-refresh-contents)

; install the missing packages
(dolist (package '(company
		   helm
		   smart-shift
		   deferred
		   exec-path-from-shell))
  (unless (package-installed-p package)
    (package-install package)))

