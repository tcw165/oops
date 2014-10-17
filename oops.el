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
;; 2014-06-07 (0.0.1)
;;    Initial release.

;;; GNU Library ================================================================
(require 'ido)
(require 'imenu)
(require 'thingatpt)
(require 'company)
(require 'saveplace)

;;; 3rd party library ==========================================================
(require 'company)
(require 'hl-anything)
(require 'history)

;;; Oops library ===============================================================
(require 'sos)
(require 'prj)
(require 'oops-lisp-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun oops-undo ()
  "Discard the selection and then undo something."
  (interactive)
  (deactivate-mark)
  (undo))

;;;###autoload
(defun oops-kill-buffer-or-window-or-frame (buffer-window-or-frame)
  "Kill buffer, window or frame with a smart prompt.
`buffer-window-or-frame' must be a string with following cases: \"buffer\", \"window\" or \"frame\"."
  (interactive
   (if (eq (selected-window) (frame-root-window))
       (list "buffer")
     (list (ido-completing-read "[Oops] What to kill? " (list "buffer" "window" "frame")))))
  (cond
   ((string-equal buffer-window-or-frame "buffer")
    (kill-buffer))
   ((string-equal buffer-window-or-frame "window")
    (delete-window))
   ((string-equal buffer-window-or-frame "frame"))
   (t (message "[Oops] Wrong option, Nothing is killed."))))

;;;###autoload
(defun oops-toggle-comment ()
  "Toggle comment smartly. It support comment on both single line and multiple lines."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(defun oops-duplicate-lines ()
  "Duplciate the current line or the lines between point and mark without modifying `kill-ring'."
  (interactive)
  ;; There're two situation:
  ;; (point) ---------------
  ;; -----------------(mark)
  ;; or
  ;; (mark)-----------------
  ;; ----------------(point)
  (let* ((beg (line-beginning-position 1))
         (end (line-beginning-position 2))
         (str (buffer-substring-no-properties beg end)))
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line 1)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (beginning-of-line 2)
                  (point))
            str (buffer-substring-no-properties beg end)))
    (save-excursion
      (if (and mark-active
               (> (mark) (point)))
          (goto-char (region-end)))
      (beginning-of-line 2)
      (insert str))
    ;; Restore selection.
    (setq deactivate-mark nil)))

;;;###autoload
(defun oops-kill-lines ()
  "Kill the current line or the lines between point and mark without modifying `kill-ring'."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line 1)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (beginning-of-line 2)
                  (point))))
    (delete-region beg end)))

;;;###autoload
(defun oops--move-lines (step)
  "Move the current line or the lines covered by region upward or downward without modifying `kill-ring'."
  ;; There're two situation:
  ;; (point) ---------------
  ;; -----------------(mark)
  ;; or
  ;; (mark)-----------------
  ;; ----------------(point)
  (let* ((beg (line-beginning-position 1))
         (end (line-beginning-position 2))
         (line-num 1)
         (point-excursion (- (point) end))
         (mark-excursion 0)
         text)
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line 1)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (beginning-of-line 2)
                  (point))
            line-num (count-lines (region-beginning) (region-end))
            point-excursion (- (point) end)
            mark-excursion (- (mark) (point))))
    ;; Extract region.
    (setq text (delete-and-extract-region beg end))
    ;; Move upward/downward and insert the cut region.
    (forward-line step)
    (insert text)
    ;; Set new point.
    (goto-char (+ (point) point-excursion))
    ;; Set new mark.
    (when mark-active
      (deactivate-mark)
      (set-mark (+ (point) mark-excursion))
      (setq deactivate-mark nil))))

;;;###autoload
(defun oops-move-lines-up ()
  "Move the current line or the lines covered by region upward without modifying `kill-ring'."
  (interactive)
  (oops--move-lines -1))

;;;###autoload
(defun oops-move-lines-down ()
  "Move the current line or the lines covered by region downward without modifying `kill-ring'."
  (interactive)
  (oops--move-lines 1))

;;;###autoload
(defun oops-indent-or-company ()
  (interactive)
  (if (or mark-active
          ;; including: "begining of line"", "end of line"", "space"", "("
          (looking-at "\\(^\\|$\\|\(\\|\\s-+\\)"))
      (indent-for-tab-command)
    (company-complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun oops-goto-global-symbol ()
  ""
  (interactive)
  (cond
   ;; lisp
   ((memq major-mode (list 'emacs-lisp-mode
                           'lisp-interaction-mode)))))

;;;###autoload
(defun oops-goto-local-symbol ()
  (interactive)
  (cond
   ;; lisp
   ((memq major-mode (list 'emacs-lisp-mode
                           'lisp-interaction-mode))
    (oops-lisp-goto-lsymb))))

;;;###autoload
(defun oops-common-escape ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers),
or go back to just one window (by deleting all but the selected window)."
  (interactive)
  (cond
   ((eq last-command 'mode-exited)
    nil)
   ((region-active-p)
    (deactivate-mark))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (current-prefix-arg
    nil)
   ((> (recursion-depth) 0)
    (exit-recursive-edit))
   (buffer-quit-function
    (funcall buffer-quit-function))
   ((string-match "^ \\*" (buffer-name (current-buffer)))
    (bury-buffer))
   ((featurep 'company)
    (company-cancel))))

;;; Window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun oops-windmove-left ()
  "Select the window to the left of the current one."
  (interactive)
  (windmove-left))

;;;###autoload
(defun oops-windmove-right ()
  "Select the window to the right of the current one."
  (interactive)
  (windmove-right))

;;;###autoload
(defun oops-windmove-up ()
  "Select the window to the above of the current one."
  (interactive)
  (windmove-up))

;;;###autoload
(defun oops-windmove-down ()
  "Select the window to the below of the current one."
  (interactive)
  (windmove-down))

;;;###autoload
(defun oops-split-window ()
  (interactive)
  (let ((dir (ido-completing-read "Split direction: "
                                  '("horizontal" "vertical")
                                  nil
                                  t)))
    (cond
      ((string= dir "vertical")
       (split-window (selected-window) nil 'below))
      ((string= dir "horizontal")
       (split-window (selected-window) nil 'right)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration Sets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun oops-default-config ()
  (interactive)
  ;; Mode line.
  (sos-setup-default-mode-line)
  ;; Project management.
  (unless (prj-load-recent-project)
    (prj-load-project))
  (sos-definition-window-mode 1)
  ;; Save place.
  (setq save-place-file "~/.emacs.d/.emacs-places")
  (setq-default save-place t)
  ;; Company extension.
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  ;; (add-hook 'c-mode-hook 'company-mode)
  ;; (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-echo-metadata-frontend
                            company-preview-frontend)))

(provide 'oops)
