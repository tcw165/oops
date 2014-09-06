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
  )

(defun sos-elisp-find-function (symb)
  "Return the candidate pointing to the definition of `symb'. It was written 
refer to `find-function-noselect' and `find-function-search-for-symbol'."
  (ignore-errors
    (let ((symb (find-function-advised-original symb)))
      (if (subrp (symbol-function symb))
          (progn
            ;; TODO: print document.
            nil)
        (let* ((name (symbol-name symb))
               (file (sos-elisp-normalize-path (symbol-file symb 'defun)))
               (linum (sos-elisp-count-lines file name find-function-regexp)))
          `(:file ,file :linum ,linum :hl-word ,name))))))

(defun sos-elisp-find-variable (symb)
  "Return the candidate pointing to the definition of `symb'. It was written 
refer to `find-function-noselect' and `find-function-search-for-symbol'."
  (ignore-errors
    (let ((file (symbol-file symb 'defvar)))
      (if file
          (let* ((name (symbol-name symb))
                 (file (sos-elisp-normalize-path file))
                 (linum (sos-elisp-count-lines file name find-variable-regexp)))
            `(:file ,file :linum ,linum :hl-word ,name))
        ;; TODO: print document.
        ))))

(defun sos-elisp-find-face (symb)
  (ignore-errors
    (let ((file (symbol-file symb 'defface)))
      (when file
        (let* ((name (symbol-name symb))
               (file (sos-elisp-normalize-path file))
               (linum (sos-elisp-count-lines file name find-face-regexp)))
          `(:file ,file :linum ,linum :hl-word ,name))))))

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
       ;; TODO: remove this test case.
       ;; (let ((symb (intern-soft "sos-definition-window-mode"))
       (let ((symb arg)
             candidates)
         ;; TODO: use tag system.
         (dolist (cand (list (sos-elisp-find-feature symb)
                             (sos-elisp-find-function symb)
                             (sos-elisp-find-variable symb)
                             (sos-elisp-find-face symb)))
           (and cand
                (push cand candidates)))
         (reverse candidates))))
    (:tips
     (when arg
       (list (format "%s" arg))))))

(provide 'sos-elisp-backend)
