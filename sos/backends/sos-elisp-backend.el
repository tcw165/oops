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

(defconst sos-elisp-find-feature-regexp "^\\s-*(provide '%s)")

(defun sos-elisp-normalize-path (file)
  ;; Convert extension from .elc to .el.
  (when (string-match "\\.el\\(c\\)\\'" file)
    (setq file (substring file 0 (match-beginning 1))))
  ;; Strip extension from .emacs.el to make sure symbol is searched in
  ;; .emacs too.
  (when (string-match "\\.emacs\\(.el\\)" file)
    (setq file (substring file 0 (match-beginning 1))))
  file)

(defun sos-elisp-count-lines (file name regexp-temp)
  ;; Open the file in order to get line number (waste way).
  (let ((regexp (format regexp-temp
                        (concat "\\\\?"
                                (regexp-quote name))))
        (case-fold-search nil)
        (linum 0))
    (with-temp-buffer
      (insert-file-contents file)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (goto-char (point-min))
        (when (re-search-forward regexp nil t)
          (setq linum (line-number-at-pos)))))
    linum))

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
           (linum (sos-elisp-count-lines file name sos-elisp-find-feature-regexp)))
      `(:file ,file :linum ,linum :type "feature" :hl-word ,name))))

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
          ;; Document struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (let* ((doc-raw (documentation real-symb t))
                 (data (help-split-fundoc doc-raw real-symb))
                 (usage (car data))
                 (doc (cdr data)))
            (with-temp-buffer
              (setq standard-output (current-buffer))
              (princ usage)
              (terpri)(terpri)
              (princ doc)
              `(:doc ,(buffer-string) :linum 1 :type "function" :hl-word ,name
                     :mode-line ,(format "%s is a built-in Elisp function."
                                         (propertize name 'face 'link)))))
        ;; File struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (let* ((file (sos-elisp-normalize-path (symbol-file real-symb 'defun)))
               (linum (sos-elisp-count-lines file name find-function-regexp)))
          `(:file ,file :linum ,linum :type "function" :hl-word ,name
                  :mode-line ,(unless (eq real-symb symb)
                                (format "%s is an alias of %s "
                                        (propertize (symbol-name symb)
                                                    'face 'link)
                                        (propertize (symbol-name real-symb)
                                                    'face 'link)))))))))

(defun sos-elisp-find-variable (symb)
  "Return the candidate pointing to the definition of `symb'. It was written 
refer to `find-variable-noselect', `find-function-search-for-symbol' and 
`describe-variable'."
  (ignore-errors
    (let ((name (symbol-name symb))
          (file (symbol-file symb 'defvar)))
      (if file
          ;; File struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (let* ((file (sos-elisp-normalize-path file))
                 (linum (sos-elisp-count-lines file name find-variable-regexp)))
            `(:file ,file :linum ,linum :type "variable" :hl-word ,name))
        ;; Document struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (let ((doc (documentation-property symb 'variable-documentation))
              val locus)
          (when doc
            (with-selected-frame (selected-frame)
              (with-current-buffer (current-buffer)
                (setq val (symbol-value symb)
                      locus (variable-binding-locus symb))))
            (with-temp-buffer
              (setq standard-output (current-buffer))
              (princ (format "%s value is " symb))
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
              `(:doc ,(buffer-string) :linum 1 :type "variable" :hl-word ,name
                     :mode-line ,(format "%s is a built-in Elisp variable."
                                         (propertize name 'face 'link))))))))))

(defun sos-elisp-find-let-variable (symb)
  ;; TODO: implement it.
  )

(defun sos-elisp-find-face (symb)
  (ignore-errors
    (let* ((file (symbol-file symb 'defface))
           (name (symbol-name symb))
           (file (sos-elisp-normalize-path file))
           (linum (sos-elisp-count-lines file name find-face-regexp)))
      `(:file ,file :linum ,linum :type "face" :hl-word ,name))))

(defun sos-elisp-thingatpt ()
  "Find symbol string around the point or text selection."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; Skip string and comment.
    (unless (memq (get-text-property (point) 'face) '(font-lock-doc-face
                                                      font-lock-string-face
                                                      font-lock-comment-face))
      (let* ((bound (bounds-of-thing-at-point 'symbol)))
        (and bound
             (buffer-substring-no-properties (car bound) (cdr bound)))))))

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
         (dolist (cand (list (sos-elisp-find-function symb)
                             (sos-elisp-find-let-variable symb)
                             (sos-elisp-find-variable symb)
                             (sos-elisp-find-face symb)
                             (sos-elisp-find-feature symb)))
           (and cand
                (push cand candidates)))
         (reverse candidates))))
    (:tips
     (when arg
       (list (format "%s" arg))))))

(provide 'sos-elisp-backend)
