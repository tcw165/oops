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
;; 2014-06-01 (0.0.1)
;;    Initial release.

(eval-when-compile (require 'cl))

(defgroup history nil
  "History.")

(defcustom his-history-max 64
  "The maximum history count."
  :type 'integer
  :group 'history)

(defcustom his-idle-seconds 10
  "Implicitly add history after emacs enter idle state for `his-idle-seconds' seconds."
  :type 'integer
  :group 'history)

(defvar his--history-list nil
  "The history is a list containing records with following format:
   (:region    STRING  BOUND_MARKER_BEG  BOUND_MARKER_END)
   (:position  MARKER)"
  )

(defvar his--history-index 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun his--same-line-p (pos1 pos2)
  (let ((line-pos1 (save-excursion
                     (goto-char pos1)
                     (beginning-of-line)
                     (point)
                     ))
        (line-pos2 (save-excursion
                     (goto-char pos2)
                     (beginning-of-line)
                     (point)
                     )))
    (= line-pos1 line-pos2)
    )
  )

(defun his--discard-invalid-history-recursively ()
  ;; TODO: Check buffer is valid or not.
  ;; TODO: Check region string is valid or not.
  )

(defun his--use-history-at-index ()
  (his--discard-invalid-history-recursively)
  (let ((history (nth his--history-index his--history-list))
        (type (car record))
        buffer
        pos1
        pos2)
    (cond
     ;;; :region type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq type :region)
      (setq buffer (marker-buffer (nth 2 history))
            pos1 (marker-position (nth 2 history))
            pos2 (marker-position (nth 3 history)))
      ;; Switch to buffer.
      (switch-to-buffer buffer)
      ;; Update region.
      (set-marker (mark-marker) pos1)
      (goto-char pos2)
      (setq mark-active t)
      )
     ;;; :position type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq type :position)
      (setq buffer (marker-buffer (nth 1 history))
            pos1 (marker-position (nth 1 history)))
      ;; Switch to buffer.
      (switch-to-buffer buffer)
      ;; Update point.
      (goto-char pos1)
      (setq mark-active nil)
      )
     )
    )
  )

;;;###autoload
(defun his-add-history ()
  (interactive)
  (let ((thing (his--thingatpt))
        (is-add-history t)
        history)
    ;; Create history.
    (if thing
        ;; :region type.
        (let ((str (nth 0 thing))
              (beg (nth 1 thing))
              (end (nth 2 thing))
              marker1
              marker2)
          ;; If 1st or 2nd history exist, test if it is a ":region" history and
          ;; current point is within it.
          ;; If t, there's no need to add the history.
          (dotimes (i 2)
            (let ((history-n (nth i his--history-list)))
              (and history-n
                   is-add-history
                   (eq :region (nth 0 history-n))
                   (let ((buffer-n (marker-buffer (nth 2 history-n)))
                         (beg-n (marker-position (nth 2 history-n)))
                         (end-n (marker-position (nth 3 history-n))))
                     (and (eq (current-buffer) buffer-n)
                          (>= (point) beg-n)
                          (<= (point) end-n)
                          (setq is-add-history nil))
                     ))
              )
            )
          (and is-add-history
               (setq marker1 (copy-marker beg t)
                     marker2 (copy-marker end t)
                     history (list :region str marker1 marker2)))
          )
      ;; else, :position type.
      ;; If 1st history exist, test if it is a ":position" history and is at the
      ;; same line with it.
      ;; If t, there's no need to add the history.
      (dotimes (i 1)
        (let ((history-n (nth i his--history-list)))
          (and history-n
               is-add-history
               (eq :position (nth 0 history-n))
               (let ((buffer-n (marker-buffer (nth 1 history-n)))
                     (position-n (marker-position (nth 1 history-n))))
                 (and (eq (current-buffer) buffer-n)
                      (his--same-line-p (point) position-n)
                      (setq is-add-history nil))
                 ))
          )
        )
      (and is-add-history
           (setq history (list :position (copy-marker (point) t))))
      )
    (when history
      ;; Discard old history.
      (and his--history-list
           (> his--history-index 0)
           (let ((current (nthcdr his--history-index his--history-list)))
             (setq his--history-list (cdr current))
             ))
      ;; Add new record.
      (push history his--history-list)
      (setq his--history-index 0)
      ;; Make sure the amount of history is less than maximum limit.
      (and (> (length his--history-list) his-history-max)
           (setcdr (nthcdr (1- his-history-max) his--history-list) nil))
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
    (his--use-history-at-index)
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
    (his--use-history-at-index)
    )
  )

;;;###autoload
(define-minor-mode auto-history-mode
  ""
  :lighter " his"
  ;; TODO: Add history automatically when emacs enter idle state for n seconds.
  )

(provide 'history)
