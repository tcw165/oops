(require 'find-func)
(require 'help-fns)
(require 'history)

;; Common ======================================================================

(defun oops--lisp-thingatpt ()
  "Return string on which the point is or just string of selection."
  (if mark-active
      ;; return the selection.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; else.
    (thing-at-point 'symbol)))

;; Source Code Navigation ======================================================

(defconst oops--lisp-search-symbol-regexp-alist
  '((defun . find-function-regexp)
    (defvar . find-variable-regexp)
    (defface . find-face-regexp))
  "Alist mapping definition types into regexp variables. Each regexp variable's value should actually be a format string to be used to substitute the desired symbol name into the regexp.")


(defun oops--lisp-search-for-symbol (symbol type library)
  "Search for SYMBOL's definition of type TYPE in LIBRARY. Visit the library in a buffer, and return a list (BUFFER POS-BEG POS-END), or just nil if the definition can't be found in the file.

TYPE specifies the kind of definition, and it is interpreted via `oops--lisp-search-symbol-regexp-alist'.
\(It was written by refering to GNU function, `find-function-search-for-symbol'.\)
"
  ;; Some functions are defined as part of the construct that defines something else.
  (while (and (symbolp symbol)
              (get symbol 'definition-name))
    (setq symbol (get symbol 'definition-name)))

  (cond
   ((string-match "\\.el\\(c\\)\\'" library)
    (setq library (substring library 0 (match-beginning 1))))
   ;; Strip extension from .emacs.el to make sure symbol is searched in
   ;; .emacs too.
   ((string-match "\\.emacs\\(.el\\)" library)
    (setq library (substring library 0 (match-beginning 1)))))

  (let* ((filename (find-library-name library))
         (regexp-symbol (cdr (assq type oops--lisp-search-symbol-regexp-alist))))
    (save-excursion
      (with-current-buffer (find-file-noselect filename)
        (let ((regexp (format (symbol-value regexp-symbol)
                              ;; Entry for ` (backquote) macro in loaddefs.el,
                              ;; (defalias (quote \`)..., has a \ but
                              ;; (symbol-name symbol) doesn't.  Add an
                              ;; optional \ to catch this.
                              (concat "\\\\?"
                                      (regexp-quote (symbol-name symbol)))))
              (case-fold-search))
          (with-syntax-table emacs-lisp-mode-syntax-table
            (goto-char (point-min))
            (if (or (re-search-forward regexp nil t)
                    ;; `regexp' matches definitions using known forms like
                    ;; `defun', or `defvar'.  But some functions/variables
                    ;; are defined using special macros (or functions), so
                    ;; if `regexp' can't find the definition, we look for
                    ;; something of the form "(SOMETHING <symbol> ...)".
                    ;; This fails to distinguish function definitions from
                    ;; variable declarations (or even uses thereof), but is
                    ;; a good pragmatic fallback.
                    (re-search-forward
                     (concat "^([^ ]+" find-function-space-re "['(]?"
                             (regexp-quote (symbol-name symbol))
                             "\\_>")
                     nil t))
                ;; Return the struct.
                (let ((end (point))
                      (beg (progn
                             (forward-thing 'symbol -1)
                             (point)
                             )))
                  (list (current-buffer) beg end))
              ;; Not found, return nil
              nil)))))))

(defun oops--lisp-find-function (symbol)
  "Return a list (BUFFER POS-BEG POS-END) pointing to the definition of FUNCTION. Return nil if symbol is a built-in function.
\(It was written by refering to GNU function, `find-function-noselect'.\)
"
  (let ((def (symbol-function (find-function-advised-original symbol)))
        aliases)
    ;; FIXME for completeness, it might be nice to print something like:
    ;; foo (which is advised), which is an alias for bar (which is advised).
    (while (symbolp def)
      (or (eq def symbol)
          (if aliases
              (setq aliases (concat aliases
                                    (format ", which is an alias for `%s'"
                                            (symbol-name def))))
            (setq aliases (format "`%s' is an alias for `%s'"
                                  symbol (symbol-name def)))))
      (setq symbol (symbol-function (find-function-advised-original symbol))
            def (symbol-function (find-function-advised-original symbol))))
    (and aliases (message "%s" aliases))

    ;; Find library and return the result.
    (let ((library (cond ((autoloadp def)
                          (nth 1 def))
                         ((subrp def) nil)
                         ((symbol-file symbol 'defun)))))
      (and library (oops--lisp-search-for-symbol symbol 'defun library)))))

;; (oops--lisp-find-variable 'hl-paren-face)
(defun oops--lisp-find-variable (symbol)
  "Return a list (BUFFER POS-BEG POS-END) pointing to the definition of VARIABLE. Return nil if symbol is a built-in variable.
\(It was written by refering to GNU function, `find-variable-noselect'.\)
"
  (let ((library (symbol-file symbol 'defvar)))
    (and library (oops--lisp-search-for-symbol symbol 'defun library))))

(defun oops--lisp-find-face (symbol)
  "Return a list (BUFFER POS-BEG POS-END) pointing to the definition of LIBRARY.
\(It was written by refering to GNU function, `find-function-noselect'.\)
"
  (let ((library (symbol-file 'hl-paren-face 'defface)))
    (and library (oops--lisp-search-for-symbol symbol 'defface library))))

(defun oops-lisp-jump-to-definition-atpt ()
  ;; TODO:
  ;; * advising function list!
  ;; * variable, function and feature with same name!
  ;; * add local variable navigation!
  (let* ((symbol (intern-soft (oops--lisp-thingatpt)))
         search-result buffer beg end)
    ;; Disable region.
    (setq mark-active nil)
    (cond
     ;; Function:
     ((fboundp symbol)
      (setq search-result (oops--lisp-find-function symbol)
            buffer (nth 0 search-result)
            beg (nth 1 search-result)
            end (nth 2 search-result))
      (if (not search-result)
          ;; Built-in function.
          (message "[Definition] No support for built-in function: %s." symbol)
        ;; Save current state before switching.
        (his-add-history)
        (switch-to-buffer buffer)
        (goto-char (1- end)) ;; TODO: because `oops--lisp-find-function symbol' will return position after the symbol.
        ;; Save new state after switching.
        (his-add-history)
        ;; Enable region.
        (set-marker (mark-marker) beg)
        (unless (= (marker-position (mark-marker)) (point))
          (setq mark-active t))))

     ;; Variable:
     ((boundp symbol)
      (setq search-result (oops--lisp-find-variable symbol)
            buffer (nth 0 search-result)
            beg (nth 1 search-result)
            end (nth 2 search-result))
      (if (not search-result)
          ;; Built-in variable.
          (message "[Definition] No support for built-in variable: %s." symbol)
        ;; Save current state before switching.
        (his-add-history)
        (switch-to-buffer buffer)
        (goto-char (1- end)) ;; TODO: because `oops--lisp-find-function symbol' will return position after the symbol.
        ;; Save new state after switching.
        (his-add-history)
        ;; Enable region.
        (set-marker (mark-marker) beg)
        (unless (= (marker-position (mark-marker)) (point))
          (setq mark-active t))
        (message "[Definition] variable: %s" symbol)))

     ;; face
     ((facep symbol)
      (setq search-result (oops--lisp-find-face symbol)
            buffer (nth 0 search-result)
            beg (nth 1 search-result)
            end (nth 2 search-result))
      (if (not search-result)
          ;; Built-in variable.
          (message "[Definition] No support for built-in face: %s." symbol)
        ;; Save current state before switching.
        (his-add-history)
        (switch-to-buffer buffer)
        (goto-char (1- end))
        ;; Save new state after switching.
        (his-add-history)
        ;; Enable region.
        (set-marker (mark-marker) beg)
        (unless (= (marker-position (mark-marker)) (point))
          (setq mark-active t))
        (message "[Definition] face: %s" symbol)))

     ;; library
     ((featurep symbol)
      (his-add-history)
      (find-library (symbol-name symbol))
      (his-add-history)))))

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
        (goto-char position)))))
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
          (add-to-list 'name-and-pos (cons name position))))))))

(provide 'oops-lisp-lib)
