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
  "Return a list, (SYMBOL_NAME BEG END), on which the point is or just string of selection."
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
(defun his-add-history ()
  (let ((thing (his--thingatpt))
        history)
    ;; Create history.
    (if thing
        ;; TODO: Examine current point is in the boundary of 1st or 2nd histories.
        ;; Use history of symbol type.
        (let ((str (nth 0 thing))
              (beg (nth 1 thing))
              (end (nth 2 thing))
              marker1
              marker2)
          (setq marker1 (copy-marker beg t)
                marker2 (copy-marker end t)
                history (list :symbol str marker1 marker2))
          )
      ;; else, use history of marker type.
      ;; TODO: Examine current point is at the same line with 1st history.
      (setq history (list :position (copy-marker (point) t)))
      )
    (when history
      ;; Discard old history.
      (when (and his--history-list
                 (> his--history-index 0))
        (let ((current (nthcdr his--history-index his--history-list)))
          (setq his--history-list (cdr current))
          )
        )
      ;; Add new record.
      (push history his--history-list)
      (setq his--history-index 0)
      ;; Make sure the amount of history is less than maximum limit.
      (when (> (length his--history-list) his-history-max)
        (setcdr (nthcdr (1- his-history-max) his--history-list) nil)
        )
      )
    )
  )

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
