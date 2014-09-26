;; Copyright (C) 2014
;;
;; Author: BoyW165
;; Version: 0.0.1
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-10-01 (0.0.1)
;;    Initial release.

(require 'thingatpt)

;;;###autoload
(defun sos-elisp-backend (command &optional arg)
  (case command
    (:symbol
     (when (member major-mode '(emacs-lisp-mode
                                lisp-interaction-mode))
       (let ((symb (sos-elisp-thingatpt)))
         (or (and symb (intern-soft symb))
             :stop))))
    (:candidates
     (when arg
       (let ((symb arg)
             candidates)
         ;; TODO: use tag system.
         ;; The last one gets the top priority.
         (dolist (cand (list (sos-elisp-find-feature symb)
                             (sos-elisp-find-face symb)
                             (sos-elisp-find-variable symb)
                             (sos-elisp-find-function symb)
                             (sos-elisp-find-function-parameter symb)
                             (sos-elisp-find-let-variable symb)))
           (and cand
                (push cand candidates)))
         candidates)))
    (:tips
     (when arg
       (list (format "%s" arg))))))

(defconst sos-elisp-find-function-regexp "^\\s-*(\\(?:def\\(ine-skeleton\\|ine-generic-mode\\|ine-derived-mode\\|ine\\(?:-global\\)?-minor-mode\\|ine-compilation-mode\\|un-cvs-mode\\|foo\\|[^icfgv]\\(\\w\\|\\s_\\)+\\*?\\)\\|easy-mmode-define-[a-z-]+\\|easy-menu-define\\|menu-bar-make-toggle\\)\\(?:\\s-\\|\\|;.*\n\\)+\\(?:'\\|(quote \\)?\\\\?\\(?1:%s\\)\\(?:\\s-\\|$\\|(\\|)\\)"
  "Refer to `find-function-regexp'.")

(defconst sos-elisp-find-variable-regexp "^\\s-*(\\(?:def[^fumag]\\(\\w\\|\\s_\\)+\\*?\\|\easy-mmode-def\\(?:map\\|syntax\\)\\|easy-menu-define\\)\\(?:\\s-\\|\n\\|;.*\n\\)+\\(?1:%s\\)\\(?:\\s-\\|$\\)"
  "Refer to `find-variable-regexp'.")

(defconst sos-elisp-find-face-regexp "^\\s-*(defface\\(?:\\s-\\|\n\\|;.*\n\\)+\\(?1:%s\\)\\(?:\\s-\\|$\\)"
  "Refer to `find-face-regexp'.")

(defconst sos-elisp-find-feature-regexp "^\\s-*(provide '\\(?1:%s\\))")

(defun sos-elisp-thingatpt ()
  "Find symbol string around the point or text selection."
  (let ((bound (if mark-active
                   (cons (region-beginning) (region-end))
                 (unless (memq (get-text-property (point) 'face)
                               '(font-lock-doc-face
                                 font-lock-string-face
                                 font-lock-comment-face))
                   (bounds-of-thing-at-point 'symbol)))))
    (when bound
      (buffer-substring-no-properties (car bound) (cdr bound)))))

(defun sos-elisp-normalize-path (file)
  ;; Convert extension from .elc to .el.
  (when (string-match "\\.el\\(c\\)\\'" file)
    (setq file (substring file 0 (match-beginning 1))))
  ;; Strip extension from .emacs.el to make sure symbol is searched in
  ;; .emacs too.
  (when (string-match "\\.emacs\\(.el\\)" file)
    (setq file (substring file 0 (match-beginning 1))))
  file)

(defun sos-elisp-get-doc&linum (filename symb-name regexp-temp)
  (let ((regexp (format regexp-temp (regexp-quote symb-name)))
        (case-fold-search nil)
        (linum 0)
        doc)
    (with-temp-buffer
      ;; Get doc.
      (when (file-exists-p filename)
        (insert-file-contents filename)
        (setq doc (buffer-string)))
      ;; Get linum.
      (with-syntax-table emacs-lisp-mode-syntax-table
        (goto-char (point-min))
        (when (re-search-forward regexp nil t)
          (setq linum (line-number-at-pos)))))
    ;; document string + line number + keywords.
    `(,doc ,linum ((,regexp 1 ',(sos-hl-symbol-face) prepend)))))

(defun sos-elisp-function-document-keywords (usage)
  (when usage
    (let ((regexp (concat "^\\(?1:" (regexp-quote usage) "\\)$"))
          keyword keywords)
      (setq keyword `((,regexp 1 ',(sos-hl-symbol-face) prepend))
            keywords (append keywords keyword))
      keywords)))

(defun sos-elisp-variable-document-keywords (name)
  (when name
    (let* ((regexp (concat "^\\(?1:" name "\\)$"))
           keyword keywords)
      (setq keyword `((,regexp 1 ',(sos-hl-symbol-face) prepend))
            keywords (append keywords keyword))
      keywords)))

(defun sos-elisp-find-feature (symb)
  "Return the absolute file name of the Emacs Lisp source of LIBRARY.
LIBRARY should be a string (the name of the library)."
  (ignore-errors
    (let* ((name (symbol-name symb))
           (file (or (locate-file name
                                  (or find-function-source-path load-path)
                                  (find-library-suffixes))
                     (locate-file name
                                  (or find-function-source-path load-path)
                                  load-file-rep-suffixes)))
           (doc&linum (sos-elisp-get-doc&linum file name
                                               sos-elisp-find-feature-regexp))
           (doc (nth 0 doc&linum))
           (linum (nth 1 doc&linum))
           (keywords (nth 2 doc&linum)))
      `(:symbol ,name :doc ,doc :type "feature" :file ,file
                :linum ,linum :keywords ,keywords))))

(defun sos-elisp-find-function (symb)
  "Return the candidate pointing to the definition of `symb'. It was written 
refer to `find-function-noselect', `find-function-search-for-symbol' and 
`describe-function'."
  (ignore-errors
    (let ((name (symbol-name symb))
          (real-symb symb))
      ;; Try to dereference the symbol if it's a alias.
      (while (symbolp (symbol-function real-symb))
        (setq real-symb (symbol-function
                         (find-function-advised-original real-symb))
              name (symbol-name real-symb)))
      (if (subrp (symbol-function real-symb))
          ;; Built-in Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (let* ((doc-raw (documentation real-symb t))
                 (data (help-split-fundoc doc-raw real-symb))
                 (usage (car data))
                 (doc (cdr data))
                 (keywords (sos-elisp-function-document-keywords usage)))
            (with-temp-buffer
              (setq standard-output (current-buffer))
              (princ usage)
              (terpri)(terpri)
              (princ doc)
              `(:symbol ,name :doc ,(buffer-string) :type "built-in function"
                        :linum 1 :keywords ,keywords
                        :mode-line ,(format "%s is a built-in Elisp function. "
                                            (propertize name
                                                        'face 'tooltip)))))
        ;; Normal Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (let* ((file (sos-elisp-normalize-path (symbol-file real-symb 'defun)))
               (doc&linum (sos-elisp-get-doc&linum file name
                                                   sos-elisp-find-function-regexp))
               (doc (nth 0 doc&linum))
               (linum (nth 1 doc&linum))
               (keywords (nth 2 doc&linum)))
          `(:symbol ,name :doc ,doc :type "function" :file ,file
                    :linum ,linum :keywords ,keywords
                    :mode-line ,(unless (eq real-symb symb)
                                  (format "%s is an alias of %s "
                                          (propertize (symbol-name symb)
                                                      'face 'tooltip)
                                          (propertize (symbol-name real-symb)
                                                      'face 'tooltip)))))))))

(defun sos-elisp-find-variable (symb)
  "Return the candidate pointing to the definition of `symb'. It was written 
refer to `find-variable-noselect', `find-function-search-for-symbol' and 
`describe-variable'."
  (ignore-errors
    (let ((name (symbol-name symb))
          (file (symbol-file symb 'defvar)))
      (if file
          ;; Normal Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (let* ((file (sos-elisp-normalize-path file))
                 (doc&linum (sos-elisp-get-doc&linum file name
                                                     sos-elisp-find-variable-regexp))
                 (doc (nth 0 doc&linum))
                 (linum (nth 1 doc&linum))
                 (keywords (nth 2 doc&linum)))
            `(:symbol ,name :doc ,doc :type "variable" :file ,file
                :linum ,linum :keywords ,keywords))
        ;; Built-in Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (let* ((doc (documentation-property symb 'variable-documentation))
               (keywords (sos-elisp-variable-document-keywords name))
               val locus)
          (when doc
            (with-selected-frame (selected-frame)
              (with-current-buffer (current-buffer)
                (setq val (symbol-value symb)
                      locus (variable-binding-locus symb))))
            (with-temp-buffer
              (setq standard-output (current-buffer))
              (princ (format "%s\nvalue is " symb))
              (prin1 val)
              (terpri)(terpri)
              ;; Print standard value if any.
              (let* ((sv (get symb 'standard-value))
                     (origval (and (consp sv)
                                   (condition-case nil
                                       (eval (car sv))
                                     (error :help-eval-error)))))
                (when (and (consp sv)
                           (not (equal origval val))
                           (not (equal origval :help-eval-error)))
                  (princ "Original value was ")
                  (prin1 origval)
                  (terpri)))
              ;; Print its locus and global value if any.
              (when locus
                (cond
                 ((bufferp locus)
                  (princ (format "- Local in buffer %s; "
                                 (pp-to-string (buffer-name)))))
                 ((framep locus)
                  (princ (format "- It is a frame-local variable; ")))
                 ((terminal-live-p locus)
                  (princ (format "- It is a terminal-local variable; ")))
                 (t
                  (princ (format "- It is local to %S" locus))))
                (if (not (default-boundp symb))
                    (princ "globally void")
                  (let ((global-val (default-value symb)))
                    (with-current-buffer standard-output
                      (princ "global value is ")
                      (if (eq val global-val)
                          (princ "the same.")
                        (princ (format "%s." (pp-to-string global-val)))))))
                (terpri))
              ;; Print its miscellaneous attributes.
              (let ((permanent-local (get symb 'permanent-local))
                    (safe-var (get symb 'safe-local-variable))
                    extra-line)
                ;; Mention if it's a local variable.
                (cond
                 ((and (local-variable-if-set-p symb)
                       (or (not (local-variable-p symb))
                           (with-temp-buffer
                             (local-variable-if-set-p symb))))
                  (setq extra-line t)
                  (princ "- Automatically becomes ")
                  (if permanent-local
                      (princ "permanently "))
                  (princ "buffer-local when set.\n"))
                 ((not permanent-local))
                 ((bufferp locus)
                  (princ "- This variable's buffer-local value is permanent.\n"))
                 (t
                  (princ "- This variable's value is permanent \
if it is given a local binding.\n")))
                (when (member (cons symb val) file-local-variables-alist)
                  (setq extra-line t)
                  (if (member (cons symb val) dir-local-variables-alist)
                      (princ "- This variable's value is file-local.\n")))
                (when (memq symb ignored-local-variables)
                  (setq extra-line t)
                  (princ "- This variable is ignored as a file-local \
variable.\n"))
                ;; Can be both risky and safe, eg auto-fill-function.
                (when (risky-local-variable-p symb)
                  (setq extra-line t)
                  (princ "- This variable may be risky if used as a \
file-local variable.\n")
                  (when (assq symb safe-local-variable-values)
                    (princ "- However, you have added it to \
`safe-local-variable-values'.\n")))
                (when safe-var
                  (setq extra-line t)
                  (princ "- This variable is safe as a file local variable ")
                  (princ "if its value\n  satisfies the predicate ")
                  (princ (if (byte-code-function-p safe-var)
                             "which is a byte-compiled expression.\n"
                           (format "`%s'.\n" safe-var))))
                (and extra-line (terpri)))
              (princ (format "Documentation:\n%s" doc))
              `(:symbol ,name :doc ,(buffer-string) :type "built-in variable"
                        :linum 1 :keywords ,keywords
                        :mode-line ,(format "%s is a built-in Elisp variable."
                                         (propertize name 'face 'link))))))))))

(defun sos-elisp-find-let-variable (symb)
  ;; TODO: implement it.
  )

(defun sos-elisp-find-function-parameter (symb)
  ;; TODO: implement it.
  )

(defun sos-elisp-find-face (symb)
  (ignore-errors
    (let* ((name (symbol-name symb))
           (file (sos-elisp-normalize-path (symbol-file symb 'defface)))
           (doc&linum (sos-elisp-get-doc&linum file name
                                               sos-elisp-find-face-regexp))
           (doc (nth 0 doc&linum))
           (linum (nth 1 doc&linum))
           (keywords (nth 2 doc&linum)))
      `(:symbol ,name :doc ,doc :type "face" :file ,file
                :linum ,linum :keywords ,keywords))))

(provide 'sos-elisp-backend)
