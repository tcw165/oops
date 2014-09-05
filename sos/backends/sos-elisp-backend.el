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

(defun sos-elisp-find-feature (symb)
  )

(defun sos-elisp-find-function (symb)
  ;; TODO: use tag system.
  (let ((def (symbol-function (find-function-advised-original symb))))
    (when def
      (if (subrp def)
          nil
        (let ((file (symbol-file symb 'defun))
              (linum 0))
          ;; Convert extension from .elc to .el.
          (when (string-match "\\.el\\(c\\)\\'" file)
            (setq file (substring file 0 (match-beginning 1))))
          ;; Strip extension from .emacs.el to make sure symbol is searched in
          ;; .emacs too.
          (when (string-match "\\.emacs\\(.el\\)" file)
            (setq file (substring file 0 (match-beginning 1))))
          ;; Open the file in order to get line number (waste way).
          (with-temp-buffer
            (insert-file-contents file)
            (let ((regexp (format find-function-regexp
                                  (concat "\\\\?"
                                          (regexp-quote (symbol-name symb)))))
                  (case-fold-search nil))
              (with-syntax-table emacs-lisp-mode-syntax-table
                (goto-char (point-min))
                (when (or (re-search-forward regexp nil t)
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
                                   (regexp-quote (symbol-name symb))
                                   "\\_>") nil t))
                  (setq linum (line-number-at-pos))))))
          `(:file ,file :linum ,linum :hl-word ,(symbol-name symb)))))))

(defun sos-elisp-find-variable (symb)
  )

(defun sos-elisp-find-face (symb)
  )

(defun sos-elisp-find-keyword (symb)
  )

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
         (dolist (cand (list (sos-elisp-find-feature symb)
                             (sos-elisp-find-function symb)
                             (sos-elisp-find-variable symb)
                             (sos-elisp-find-face symb)
                             (sos-elisp-find-keyword symb)))
           ;;
           (and cand
                (push cand candidates)))
         (reverse candidates))))
    (:tips
     (and arg
          (list (format "%s" arg))))))

(provide 'sos-elisp-backend)
