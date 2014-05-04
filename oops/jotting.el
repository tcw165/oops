(defun test (function)
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
(let ((standard-output (get-buffer-create "*scratch*")))
  (test 'get)
  )

(documentation-property 'find-function 'variable-documentation)
(symbol-plist 'describe-function)
(documentation 'describe-function)
(documentation-property 'major-mode 'variable-documentation)
(documentation-property 'describe-function 'function-documentation)

(symbol-name 'oops-mode)
(symbol-value 'oops-mode)
(symbol-function 'oops-mode)
(symbol-plist 'oops-mode)

(symbol-value 'get)
(symbol-function 'get)
(symbol-plist 'get)

(symbol-name (read "oops-mode"))
(symbol-value (read "oops-mode"))
(symbol-function (read "oops-mode"))
(symbolp (read "oops-mode"))

;; check if a variable eval to a lisp symbol
(setq x1 123 )
(symbolp x1)                            ;nil
(setq x2 'xx )
(symbolp x2)                            ;t

;; get a symbol's cell values
(symbol-name 'x1)                       ; "x1"
(symbol-value 'x1)                      ; 123
(symbol-function 'x1)                   ; error. because it's void.
(symbol-plist 'x1)                      ;nil

;; get a symbol's cell values
(symbol-name 'setq)                  ; "setq"
(symbol-value 'setq)                 ; error. because it's void.
(symbol-function 'setq)              ;#<subr setq>
(symbol-plist 'setq)                 ;(byte-compile byte-compile-setq)
