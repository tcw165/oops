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
;; 2014-??-?? (0.0.1)
;;    Initial release.

;; (add-to-list 'load-path "./backends")
;; (require 'history-lisp)
(eval-when-compile (require 'cl))

(defgroup history nil
  "History.")

(defcustom his-history-max 64
  "The maximum history count."
  :type 'integer
  :group 'history)

;; (defcustom his-history-backends '(('emacs-lisp-mode . nil)
;;                                   (nil . nil))
;;   "The history backends."
;;   ;; :type '
;;   :group 'history)

(defvar his--history-list nil
  "The history is a list containing records with following format:
\(:symbol    SYMBOL_NAME  BOUND_MARKER_BEG  BOUND_MARKER_END\)
\(:position  MARKER\)")

(defvar his--history-index 0)

(defun his--thingatpt ()
  "Return a list, (REGEXP_STRING BEG END), on which the point is or just string of selection."
  (if mark-active
      ;; return the selection.
      (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
        (list str (region-beginning) (region-end))
        )
    ;; else.
    (let* ((bound (bounds-of-thing-at-point 'symbol))
           (str (and bound
                     (buffer-substring-no-properties (car bound) (cdr bound)))))
      (and str
           (list str (car bound) (cdr bound)))
      )
    )
  )

(defun his--history-p (record)
  (and (memq (car record) (:symbol :position))
       t)
  )

(defun his--recursively-discard-history-index ()
  ;; TODO
  )

(defun his--use-history-index ()
  (his--recursively-discard-history-index)
  ;; TODO
  )

;;;###autoload
(defun his-make-symbol-history (symb-name bound-marker-beg bound-marker-end)
  (when (and (stringp symb-name)
             (markerp bound-marker-beg)
             (markerp bound-marker-end))
    (list :symbol symb-name bound-marker-beg bound-marker-end)
    )
  )

;;;###autoload
(defun his-make-position-history (pos-marker)
  (when (markerp pos-marker)
    (list :position pos-marker)
    )
  )

;;;###autoload
;; TODO: Smartly detect history type, use `his--thingatpt'.
(defun his-add-history (from to)
  ""
  ;; TODO: Check current symbol-name has already in the history.
  (when (and (his--history-p from)
             (his--history-p to))
    ;; Discard old history.
    (when (and his--history-list
               (> his--history-index 0))
      (let ((current (nth his--history-index his--history-list)))
        (setq his--history-list (cdr current))
        )
      )
    ;; Add new history.
    (push from his--history-list)
    (push to his--history-list)
    (setq his--history-index 0)
    ;; Make sure the amount of history is less than maximum limit.
    (when (> (length his--history-list) his-history-max)
      (setcdr (nthcdr (1- his-history-max) his--history-list) nil)
      )
    )
  )
;; (defun his-add-history ()
;;   (let ((thing (his--thingatpt)))
;;     (if thing
;;         ;; Use history of symbol type.
;;         (let ()
;;           )
;;       ;; else, use history of marker type.
;;       )
;;     )
;;   )

;;;###autoload
(defun his-prev-history ()
  "Navigate to previous record in the history."
  (interactive)
  (when (> (length his--history-list) 0)
    (incf his--history-index)
    (and (> his--history-index (1- (length his--history-list)))
         (setq his--history-index (1- (length his--history-list))))
    (his--use-history-index)
    )
  )

;;;###autoload
(defun his-next-history ()
  "Navigate to next record in the history."
  (interactive)
  (when (> (length his--history-list) 0)
    (decf his--history-index)
    (and (< his--history-index 0)
         (setq his--history-index 0))
    (his--use-history-index)
    )
  )

(provide 'history)
