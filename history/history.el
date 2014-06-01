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
;; TODO: Add comment for all the functions and variables.
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

(defun his--auto-history-custom-set (symbol value)
  (set symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when 
        (auto-history-mode -1)
        (auto-history-mode 1)
        )
      )
    )
  )

(defcustom his-history-max 64
  "The maximum history count."
  :type 'integer
  :group 'history)

(defcustom his-idle-seconds 10
  "Implicitly add history after emacs enter idle state for `his-idle-seconds' seconds."
  :type 'number
  :initialize 'custom-initialize-default
  :set 'his--auto-history-custom-set
  :group 'history)

(defvar his--history-list nil
  "The history is a list containing records with following format:
   (:region    STRING  BOUND_MARKER_BEG  BOUND_MARKER_END)
   (:position  MARKER)"
  )

(defvar his--history-index 0)

(defvar his--idle-timer nil)

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

(defun his--move-to-valid-history (step)
  (when (> (length his--history-list) 0)
    ;; Update index.
    (setq his--history-index (+ his--history-index step))
    (and (> his--history-index (1- (length his--history-list)))
         (setq his--history-index (1- (length his--history-list))))
    (and (< his--history-index 0)
         (setq his--history-index 0))
    ;; Check if the history is valid.
    (let* ((history (nth his--history-index his--history-list))
           (type (car history))
           (is-discard nil)
           buffer)
      (cond
       ;;; :region type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((eq type :region)
        (setq buffer (marker-buffer (nth 2 history)))
        (if (buffer-live-p buffer)
            ;; Buffer is valid, test its string.
            (let* ((beg (marker-position (nth 2 history)))
                   (end (marker-position (nth 3 history)))
                   (str1 (nth 1 history))
                   (str2 (with-current-buffer buffer
                           (buffer-substring-no-properties beg end)
                           )))
              (and (not (string-equal str1 str2))
                   (setq is-discard t))
              )
          ;; Buffer is invalid.
          (setq is-discard t)
          )
        )
       ;;; :position type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((eq type :position)
        (setq buffer (marker-buffer (nth 1 history)))
        (and (not (buffer-live-p buffer))
             (setq is-discard t))
        )
       )
      ;; Discard history.
      (and is-discard
           (setq his--history-list (delq history his--history-list))
           (his--move-to-valid-history step))
      )
    )
  )

(defun his--use-history-at-index ()
  (if (= (length his--history-list) 0)
      (message "[History] no history.")
    (let* ((history (nth his--history-index his--history-list))
           (type (car history))
           buffer
           pos1
           pos2)
      (cond
       ;;; :region type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
       ;;; :position type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (message "[History] navigate to %s/%s."
             (if (> (length his--history-list) 0)
                 (- (length his--history-list) his--history-index) 0)
             (length his--history-list))
    )
  )

(defun his--idle-timer-function ()
  (and auto-history-mode
       (his-add-history t)
       )
  )

;;;###autoload
(defun his-show-history ()
  (interactive)
  (message "[History] index/total = %s/%s\n[History] history = %s"
           (if (> (length his--history-list) 0)
               (1+ his--history-index) 0)
           (length his--history-list)
           his--history-list)
  )

;;;###autoload
(defun his-add-history (&optional force-position-type)
  (interactive)
  (let ((thing (his--thingatpt))
        history)
    ;; Create history.
    (if (and thing
             (not force-position-type))
        ;;; :region type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (let* ((str (nth 0 thing))
               (beg (nth 1 thing))
               (end (nth 2 thing))
               (marker1 (copy-marker beg t))
               (marker2 (copy-marker end t)))
          (setq history (list :region str marker1 marker2))
          )
      ;;; :position type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; If nth history exist, test if it is a ":position" history and is at the
      ;; same line with it.
      ;; If t, there's no need to add the history.
      ;; But update the its position.
      (let* ((history-n (nth his--history-index his--history-list))
             (type-n (nth 0 history-n))
             (is-add-history t)
             marker-n
             buffer-n
             position-n)
        (when (and history-n
                   (eq :position type-n))
          (setq marker-n (nth 1 history-n)
                buffer-n (marker-buffer marker-n)
                position-n (marker-position marker-n))
          (and (eq (current-buffer) buffer-n)
               (his--same-line-p (point) position-n)
               (set-marker marker-n (point))
               (setq is-add-history nil))
          )
        (and is-add-history
             (setq history (list :position (copy-marker (point) t))))
        )
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
(defun his-discard-all-history ()
  (interactive)
  (setq his--history-index 0
        his--history-list nil)
  )

;;;###autoload
(defun his-prev-history ()
  "Navigate to previous record in the history."
  (interactive)
  (his--move-to-valid-history 1)
  (his--use-history-at-index)
  )

;;;###autoload
(defun his-next-history ()
  "Navigate to next record in the history."
  (interactive)
  (his--move-to-valid-history -1)
  (his--use-history-at-index)
  )

;;;###autoload
(define-minor-mode auto-history-mode
  "Add history automatically when emacs enter idle state for n seconds."
  :lighter " a-his"
  (and (timerp his--idle-timer)
       (setq his--idle-timer (cancel-timer his--idle-timer)))
  (when auto-history-mode
    (setq his--idle-timer (run-with-idle-timer his-idle-seconds t 'his--idle-timer-function))
    )
  )

(provide 'history)
