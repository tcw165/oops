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
    ;; Skip document and comment.
    (unless (memq (get-text-property (point) 'face) '(font-lock-doc-face
                                                      font-lock-string-face
                                                      font-lock-comment-face))
      (let* ((bound (bounds-of-thing-at-point 'symbol)))
        (and bound
             (buffer-substring-no-properties (car bound) (cdr bound)))))))

(defun sos-find-candidates (symb)
  (let* ((symbol (intern-soft (oops--lisp-thingatpt)))
         search-result)
    (when symbol
      (cond
       ;; TODO: Support feature.
       ;; Library:
       ;; ((featurep symbol)
       ;;  nil
       ;;  )
       ;; Function:
       ((fboundp symbol)
        (setq search-result (oops--lisp-find-function symbol)))
       ;; Variable:
       ((boundp symbol)
        (setq search-result (oops--lisp-find-variable symbol)))
       ;; Face:
       ((facep symbol)
        (setq search-result (oops--lisp-find-face symbol))
        (if search-result
            (oops-update-help search-result)
          ;; Built-in variable, show HELP.
          (oops-update-help (oops--lisp-describe-variable symbol))))
       ;; Keyword:
       ((keywordp symbol)
        (message "[Definition] It's a keyword, %s" symbol))))))

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
     (list `(:doc "document..." :mode "emacs-lisp-mode")))
    (:tips
     (and arg
          (list (format "%s" arg))))))

(provide 'sos-elisp-backend)
