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
  ;; (let ((def (symbol-function (find-function-advised-original symbol)))
  ;;       aliases)
  ;;   ;; FIXME for completeness, it might be nice to print something like:
  ;;   ;; foo (which is advised), which is an alias for bar (which is advised).
  ;;   (while (symbolp def)
  ;;     (or (eq def symbol)
  ;;         (if aliases
  ;;             (setq aliases (concat aliases
  ;;                                   (format ", which is an alias for `%s'"
  ;;                                           (symbol-name def))))
  ;;           (setq aliases (format "`%s' is an alias for `%s'"
  ;;                                 symbol (symbol-name def)))))
  ;;     (setq symbol (symbol-function (find-function-advised-original symbol))
  ;;           def (symbol-function (find-function-advised-original symbol))))
  ;;   (and aliases (message "%s" aliases))

  ;;   ;; Find library and return the result.
  ;;   (let ((library (cond ((autoloadp def)
  ;;                         (nth 1 def))
  ;;                        ((subrp def) nil)
  ;;                        ((symbol-file symbol 'defun)))))
  ;;     (and library (oops--lisp-search-for-symbol symbol 'defun library))))
  )

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
     (and arg
          ;; TODO: remove this test case.
          ;; (let ((symb (intern-soft "sos-definition-window-mode"))
          (let ((symb arg)
                candidates)
            (dolist (cand (list (sos-elisp-find-feature symb)
                                (sos-elisp-find-function symb)
                                (sos-elisp-find-variable symb)
                                (sos-elisp-find-face symb)
                                (sos-elisp-find-keyword symb)))
              (and cand
                   (push cand candidates)))
            (reverse candidates)))
     ;; TODO: remove this test case.
     (list `(:doc "document..." :mode "emacs-lisp-mode")))
    (:tips
     (and arg
          (list (format "%s" arg))))))

(provide 'sos-elisp-backend)
