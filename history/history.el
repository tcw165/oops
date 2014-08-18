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
;; TODO: Add GUI buttom for `his-add-history', `his-next-history' and `his-prev-history'.
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

(defcustom his-history-max 16
  "The maximum history count."
  :type 'integer
  :group 'history)

(defvar his--history-list nil
  "The history is a list containing records with following format:
   (:region    STRING  BOUND_MARKER_BEG  BOUND_MARKER_END)
             - STRING            String between beginning and end.
             - BOUND_MARKER_BEG  Marker for beginning position.
             - BOUND_MARKER_END  Marker for end position.

   (:position  MARKER)
             - MARKER            Marker for a position.
")

(defvar his--history-index 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun his--thingatpt ()
  "Return a list, (SYMBOL_NAME BEG END), on which the point is or just string of selection."
  (if mark-active
      ;; return the selection.
      (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
        (list str (region-beginning) (region-end)))
    ;; else.
    (let* ((bound (bounds-of-thing-at-point 'symbol))
           (str (and bound
                     (buffer-substring-no-properties (car bound) (cdr bound)))))
      (and str
           (list str (car bound) (cdr bound))))))

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
    (= line-pos1 line-pos2)))

(defun his--add-history (history)
  (when history
    ;; Discard old history.
    (and his--history-list
         (> his--history-index 0)
         (let ((current (nthcdr his--history-index his--history-list)))
           (setq his--history-list (cdr current))))
    ;; Add new record.
    (push history his--history-list)
    (setq his--history-index 0)
    ;; Keep total amount of history is less than `his-history-max'.
    (and (> (length his--history-list) his-history-max)
         (setcdr (nthcdr (1- his-history-max) his--history-list) nil))))

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
                           (buffer-substring-no-properties beg end))))
              (and (not (string-equal str1 str2))
                   (setq is-discard t)))
          ;; Buffer is invalid.
          (setq is-discard t)))
       ;;; :position type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((eq type :position)
        (setq buffer (marker-buffer (nth 1 history)))
        (and (not (buffer-live-p buffer))
             (setq is-discard t))))
      ;; Discard history.
      (and is-discard
           (setq his--history-list (delq history his--history-list))
           (his--move-to-valid-history step)))))

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
        (setq mark-active t))
       ;;; :position type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((eq type :position)
        (setq buffer (marker-buffer (nth 1 history))
              pos1 (marker-position (nth 1 history)))
        ;; Switch to buffer.
        (switch-to-buffer buffer)
        ;; Update point.
        (goto-char pos1)
        (setq mark-active nil))))
    (message "[History] navigate to %s/%s."
             (if (> (length his--history-list) 0)
                 (- (length his--history-list) his--history-index) 0)
             (length his--history-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun his-add-region-type-history (beg end)
  "TODO: commentary"
  (interactive)
  ;; Add :region history.
  (let* ((str (buffer-substring-no-properties beg end))
         (marker1 (copy-marker beg t))
         (marker2 (copy-marker end t))
         (history (list :region str marker1 marker2)))
    (his--add-history history)))

;;;###autoload
(defun his-add-position-type-history (&optional position)
  "TODO: commentary"
  (interactive)
  (let* ((pos (or position
		  (point)))
	 (history (list :position (copy-marker pos t))))
    (his--add-history history)))

;;;###autoload
(defun his-show-history ()
  "TODO: commentary"
  (interactive)
  (message "[History] index/total = %s/%s\n[History] history = %s"
           (if (> (length his--history-list) 0)
               (1+ his--history-index) 0)
           (length his--history-list)
           his--history-list))

;;;###autoload
(defun his-add-history ()
  "TODO: commentary"
  (interactive)
  (let ((thing (his--thingatpt))
        history)
    ;; Create history.
    (if thing
        ;;; :region type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (his-add-region-type-history (nth 1 thing) (nth 2 thing))
      ;;; :position type ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (his-add-position-type-history (point)))))

;;;###autoload
(defun his-discard-all-history ()
  "Discard all the history."
  (interactive)
  (setq his--history-index 0
        his--history-list nil))

;;;###autoload
(defun his-prev-history ()
  "Navigate to previous history."
  (interactive)
  (when (> (length his--history-list) 0)
    (let* ((history (nth his--history-index his--history-list))
	   (type (nth 0 history))
	   (n (cond
	       ((eq type :region) 2)
	       ((eq type :position) 1)))
	   (marker (nth n history))
	   (pos (marker-position marker)))
      ;; If point is far away from current history, use current history.
      ;; If point is close from current history, use next/previous history.
      (if (or (null pos)
	      (his--same-line-p (point) pos))
	  (his--move-to-valid-history 1))))
  (his--use-history-at-index))

;;;###autoload
(defun his-next-history ()
  "Navigate to next history."
  (interactive)
  (when (> (length his--history-list) 0)
    (let* ((history (nth his--history-index his--history-list))
	   (type (nth 0 history))
	   (n (cond
	       ((eq type :region) 2)
	       ((eq type :position) 1)))
	   (marker (nth n history))
	   (pos (marker-position marker)))
      ;; If point is far away from current history, use current history.
      ;; If point is close from current history, use next/previous history.
      (if (or (null pos)
	      (his--same-line-p (point) pos))
	  (his--move-to-valid-history -1))))
    (his--use-history-at-index))

;;;###autoload
(defun his-add-menu-and-toolbar-item ()
  ;; TODO: enabled/disabled
  ;; menu bar
  (define-key (default-value 'global-map) [menu-bar edit separator-history]
    '(menu-item "--"))

  ;; .------------------.
  ;; | Next History     |
  ;; | Previous History |
  ;; | History....-----------------.
  ;; |           | Set History     |
  ;; ~ ~ ~ ~ ~ ~ | Show History    |
  ;;             | Discard History |
  ;;             '-----------------'
  (define-key (default-value 'global-map) [menu-bar edit history-more]
    (cons "History..." (make-sparse-keymap "History Miscellaneous Function...")))
  (define-key (default-value 'global-map) [menu-bar edit history-more discard-history]
    '(menu-item "Discard History" his-discard-all-history
		:enable (> (length his--history-list) 0)
		:help "Discard all the history"))
  (define-key (default-value 'global-map) [menu-bar edit history-more show-history]
    '(menu-item "List History" his-show-history
		:help "List history in a buffer"))
  (define-key (default-value 'global-map) [menu-bar edit history-more set-history]
    '(menu-item "Set History" his-add-position-type-history
		:help "Add a history refer to current buffer and point"))

  ;; .------------------.
  ;; | Next History     |
  ;; | Previous History |
  ;; | History...       |
  ;; |                  |
  ;; ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  ;; (define-key global-map [menu-bar edit next-history]
  ;;   '("Next History" . his-next-history))
  (define-key (default-value 'global-map) [menu-bar edit next-history]
    '(menu-item "Next History" his-next-history
		:enable (> (length his--history-list) 0)
		:help "Go to next history"))
  (define-key (default-value 'global-map) [menu-bar edit previous-history]
    '(menu-item "Previous History" his-prev-history
		:enable (> (length his--history-list) 0)
		:help "Go to previous history"))

  ;; tool bar
  (define-key (default-value 'tool-bar-map) [separator-history]
    '("--"))
  (define-key (default-value 'tool-bar-map) [set-history]
    '(menu-item "Set History" his-add-position-type-history
		:image (find-image '((:type xpm :file "set-history.xpm")))
		:help "Add a history refer to current buffer and point"))
  (define-key (default-value 'tool-bar-map) [next-history]
    '(menu-item "Next History" his-next-history
		:image (find-image '((:type xpm :file "next-history.xpm")))
		:enable (> (length his--history-list) 0)
		:help "Go to next history"))
  (define-key (default-value 'tool-bar-map) [previous-history]
    '(menu-item "Previous History" his-prev-history
		:image (find-image '((:type xpm :file "prev-history.xpm")))
		:enable (> (length his--history-list) 0)
		:help "Go to previous history")))
;; Automatically add menu items of this feature.
(his-add-menu-and-toolbar-item)

;;;###autoload
(defun his-remove-menu-and-toolbar-item ()
  ;; menu bar
  (define-key global-map [menu-bar edit separator-history] nil)
  (define-key global-map [menu-bar edit history-more] nil)
  (define-key global-map [menu-bar edit next-history] nil)
  (define-key global-map [menu-bar edit previous-history] nil)
  ;; tool bar
  (define-key global-map [tool-bar prev-history] nil)
  (define-key global-map [tool-bar next-history] nil)
  (define-key global-map [tool-bar set-history] nil))
;; (his-remove-menu-and-toolbar-item)

(provide 'history)
