;; =============================================================================

(defun oops--lisp-thingatpt ()
  "Return string on which the point is or just string of selection."
  (if mark-active
      ;; return the selection.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; else.
    (thing-at-point 'symbol)
    )
  )

(defun oops--lisp-help-buffer (&optional clean-buf)
  (let ((buf (get-buffer-create "**Help**")))
    (when (and clean-buf
               (> clean-buf 0))
      (with-current-buffer buf
        (erase-buffer))
      )
    ;; (with-current-buffer buf
    ;;   (read-only-mode 1))
    buf
    )
  )

(defun oops--lisp-describe-function-1 (function)
  (let* ((advised (and (symbolp function) (featurep 'advice)
                       (ad-get-advice-info function)))
         ;; If the function is advised, use the symbol that has the
         ;; real definition, if that symbol is already set up.
         (real-function (or (and advised
                                 (let ((origname (cdr (assq 'origname advised))))
                                   (and (fboundp origname) origname)))
                            function))
         ;; Get the real definition.
         (def (if (symbolp real-function)
                  (symbol-function real-function)
                function))
         (aliased (symbolp def))
         (real-def (if aliased
                       (let ((f def))
                         (while (and (fboundp f)
                                     (symbolp (symbol-function f)))
                           (setq f (symbol-function f)))
                         f)
                     def))
         (file-name (find-lisp-object-file-name function def))
         ;; (pt1 (with-current-buffer (help-buffer) (point)))
         (beg (if (and (or (byte-code-function-p def)
                           (keymapp def)
                           (memq (car-safe def) '(macro lambda closure)))
                       file-name
                       (help-fns--autoloaded-p function file-name))
                  (if (commandp def)
                      "an interactive autoloaded "
                    "an autoloaded ")
                (if (commandp def) "an interactive " "a ")))
         )

    ;; Print what kind of function-like object FUNCTION is.
    (princ (cond ((or (stringp def) (vectorp def))
                  "a keyboard macro")
                 ((subrp def)
                  (if (eq 'unevalled (cdr (subr-arity def)))
                      (concat beg "special form")
                    (concat beg "built-in function")))
                 ((byte-code-function-p def)
                  (concat beg "compiled Lisp function"))
                 (aliased
                  (format "an alias for `%s'" real-def))
                 ((eq (car-safe def) 'lambda)
                  (concat beg "Lisp function"))
                 ((eq (car-safe def) 'macro)
                  (concat beg "Lisp macro"))
                 ((eq (car-safe def) 'closure)
                  (concat beg "Lisp closure"))
                 ((autoloadp def)
                  (format "%s autoloaded %s"
                          (if (commandp def) "an interactive" "an")
                          (if (eq (nth 4 def) 'keymap) "keymap"
                            (if (nth 4 def) "Lisp macro" "Lisp function"))))
                 ((keymapp def)
                  (let ((is-full nil)
                        (elts (cdr-safe def)))
                    (while elts
                      (if (char-table-p (car-safe elts))
                          (setq is-full t
                                elts nil))
                      (setq elts (cdr-safe elts)))
                    (concat beg (if is-full "keymap" "sparse keymap"))))
                 (t "")))

    (if (and aliased (not (fboundp real-def)))
        (princ ",\nwhich is not defined.  Please make a bug report.")
      (with-current-buffer standard-output
        (save-excursion
          (save-match-data
            (when (re-search-backward "alias for `\\([^`']+\\)'" nil t)
              (help-xref-button 1 'help-function real-def))))
        )

      (when file-name
        (princ " in `")
        ;; We used to add .el to the file name,
        ;; but that's completely wrong when the user used load-file.
        (princ (if (eq file-name 'C-source)
                   "C source code"
                 (file-name-nondirectory file-name)))
        (princ "'")
        ;; Make a hyperlink to the library.
        (with-current-buffer standard-output
          (save-excursion
            (re-search-backward "`\\([^`']+\\)'" nil t)
            (help-xref-button 1 'help-function-def function file-name))
          )
        )
      (princ ".")
      ;; (with-current-buffer (help-buffer)
      ;;   (fill-region-as-paragraph (save-excursion (goto-char pt1) (forward-line 0) (point))
      ;;                             (point)))
      (terpri)(terpri)

      (let* ((doc-raw (documentation function t))
             ;; If the function is autoloaded, and its docstring has
             ;; key substitution constructs, load the library.
             (doc (progn
                    (and (autoloadp real-def) doc-raw
                         help-enable-auto-load
                         (string-match "\\([^\\]=\\|[^=]\\|\\`\\)\\\\[[{<]"
                                       doc-raw)
                         (load (cadr real-def) t))
                    (substitute-command-keys doc-raw))))

        (help-fns--key-bindings function)
        (with-current-buffer standard-output
          (setq doc (help-fns--signature function doc real-def real-function))

          (help-fns--compiler-macro function)
          (help-fns--parent-mode function)
          (help-fns--obsolete function)

          (insert "\n"
                  (or doc "Not documented."))
          )
        )
      )
    )
  )

;; (oops--lisp-describe-function 'subr-arity)
;; standard-output
(defun oops--lisp-describe-function (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (let ((standard-output (oops--lisp-help-buffer 1)))
    (prin1 function)
    (princ " is ")
    (oops--lisp-describe-function-1 function)
    )
  (with-current-buffer (oops--lisp-help-buffer)
    (goto-char 1)
    )
  )

;; (find-lisp-object-file-name 'standard-output 'defvar)
;; (string-match "[.]\\(c\\|cpp\\)$" (find-lisp-object-file-name 'standard-output 'defvar))
(defun oops-lisp-show-help-atpt ()
  (let* ((symb (intern-soft (oops--lisp-thingatpt))))
    (when symb
      (cond
       ;; Library:
       ;; ((featurep symb)
       ;;  nil
       ;;  )
       ;; Variable:
       ((boundp symb)
        (let ((fname (find-lisp-object-file-name symb 'defvar)))
          (if (or (eq fname 'C-source)
                  (string-match "[.]\\(c\\|cpp\\)$" fname))
              (message "Yet support for built-in variable: %s." symb)
            ;; Try not to use `find-variable-noselect'.
            (oops-update-help (save-excursion
                                (find-variable-noselect symb)))
            )
          )
        )
       ;; Function:
       ((fboundp symb)
        (if (subrp (symbol-function symb))
            (progn
              (oops--lisp-describe-function symb)
              (oops-update-help (oops--lisp-help-buffer))
              )
          ;; TODO:
          ;; Try not to use `find-function-noselect'.
          (oops-update-help (save-excursion
                              (find-function-noselect symb t)))
          )
        )
       )
      )
    )
  )

;; =============================================================================

(defun oops--lisp-find-function (function)
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

(defun oops--lisp-find-variable (variable)
  ""
  )

(defun oops-lisp-jump-to-definition-atpt ()
  ""
  ;; TODO/FIXME:
  ;; * advising function list!
  ;; * variable and function with same name!
  ;; * add local variable navigation!
  (let* ((symb (intern-soft (oops--lisp-thingatpt))))
    ;; Force to unselect text.
    (deactivate-mark)
    (cond
     ;; library
     ((featurep symb)
      (find-library (symbol-name symb))
      ;; TODO/FIXME: go to (require 'text) line
      (message "library: %s" symb)
      )
     ;; TODO/FIXME =======================================================>
     ;; ;; Variable and Function with the same name:
     ;; (is-var-fun
     ;;  (message "%s is type of \"is-var-fun\" and is not ready yet" text)
     ;;  )
     ;; <==================================================================
     ;; Function:
     ((fboundp symb)
      (if (subrp (symbol-function symb))
          (message "Not support built-in function: %s." symb)
        (find-function-do-it symb nil 'switch-to-buffer)
        (message "function: %s" symb)
        )
      )
     ;; Variable:
     ((boundp symb)
      (find-function-do-it symb 'defvar 'switch-to-buffer)
      (message "variable: %s" symb)
      )
     ;; Unknown symbol:
     (t (message "unknown symbol: %s" symb))
     )
    )
  )

;; require `imenu'.
(defun oops-lisp-goto-lsymb (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (oops-lisp-goto-lsymb (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Find symbol in file: " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position))))
    )
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (oops-lisp-goto-lsymb symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position)))))
    )
   )
  )

(provide 'oops-lisp-lib)
