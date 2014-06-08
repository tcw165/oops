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
;; 2014-06-07 (0.0.1)
;;    Initial release.

;;; GNU Library ================================================================
(require 'ido)
(require 'imenu)
(require 'thingatpt)
(require 'company)

;;; 3rd party library ==========================================================
(require 'hl-anything)
(require 'history)

;;; Oops library ===============================================================
;; Core libraries:
(require 'oops-win)
(require 'oops-help)
;; Language libraries:
(require 'oops-c-lib)
(require 'oops-cpp-lib)
(require 'oops-lisp-lib)
(require 'oops-python-lib)

;; TODO: customization.
(defconst oops--hooks
  '(;; GNU mode
    (emacs-lisp-mode-hook . imenu-add-menubar-index)
    (lisp-interaction-mode-hook . hl-paren-mode)
    ;; 3rd-party
    (emacs-lisp-mode-hook . hl-paren-mode)
    (emacs-lisp-mode-hook . auto-history-mode))
  "An association list that indicates the bindings of major mode and minor mode. Its format should be (MAJOR-MODE-HOOK . MINOR-MODE-HOOK)")

(defvar oops--idtimer nil
  "An idle timer that ask `oops--idtimer-function' to do something.")

(defun oops--idtimer-function ()
  "The function for `oops--idtimer' do the following things:
  * Detect the symbol nearby the `point', and show symbol's definition in another window. Normally, the definition window is under the current window."
  (when oops-mode
    (cond
     ;; lisp
     ((or (eq major-mode 'emacs-lisp-mode)
          (eq major-mode 'lisp-interaction-mode))
      (when oops--is-basic-perspective
        (oops-lisp-show-help-atpt)
        )
      )
     ;; c
     ;; c++-mode
     ;; python-mode
     ;; eshell
     )
    )
  )

(defun oops--init-idtimer (enable)
  "Create the `oops--idtimer' if `enable' is 1. Destory it if -1.

!DO NOT use this function in your lisp, or there would be side effects!"
  (if (> enable 0)
      ;; Enable idle timer.
      (when (null oops--idtimer)
        (setq oops--idtimer (run-with-idle-timer 0.3 t 'oops--idtimer-function))
        )
    ;; Disable idle timer.
    (when oops--idtimer
      (cancel-timer oops--idtimer)
      (setq oops--idtimer nil)
      )
    )
  )

;;; Text Operation =============================================================

;;;###autoload
(defun oops-undo ()
  "Discard the selection and then undo something."
  (interactive)
  (deactivate-mark)
  (undo)
  )

;;;###autoload
(defun oops-kill-current-buffer-or-window ()
  "Kill the buffer you are focusing which is `current-buffer'."
  (interactive)
  (kill-buffer (current-buffer))
  )

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
                  (point))
            )
      )
    (comment-or-uncomment-region beg end)
    )
  )

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
            str (buffer-substring-no-properties beg end)
            )
      ;; (push-mark)
      )
    (save-excursion
      (if (and mark-active
               (> (mark) (point)))
          (goto-char (region-end)))
      (beginning-of-line 2)
      (insert str)
      )
    ;; Restore selection.
    (setq deactivate-mark nil)
    )
  )

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
                  (point))
            )
      )
    (delete-region beg end)
    )
  )

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
            mark-excursion (- (mark) (point))
            )
      )
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
      (setq deactivate-mark nil)
      )
    )
  )

;;;###autoload
(defun oops-move-lines-up ()
  "Move the current line or the lines covered by region upward without modifying `kill-ring'."
  (interactive)
  (oops--move-lines -1)
  )

;;;###autoload
(defun oops-move-lines-down ()
  "Move the current line or the lines covered by region downward without modifying `kill-ring'."
  (interactive)
  (oops--move-lines 1)
  )

;;;###autoload
(defun oops-indent-or-company ()
  (interactive)
  (if (or mark-active
          ;; including: "begining of line"", "end of line"", "space"", "("
          (looking-at "\\(^\\|$\\|\(\\|\\s-+\\)"))
      (indent-for-tab-command)
    (company-complete-common))
  )

;;; Navigation =================================================================

;;;###autoload
(defun oops-jump-to-definition-atpt ()
  "Find the symbol's definition. If there's only one result, open the file in the current window. If there're multiple results, show a list under the current window.

For lisp, it supports symbol of `defun', `defadvice', `defvar' and `provide'.
For C/C++, it doesn't support yet.
For Python, it doesn't support yet."
  (interactive)
  (cond
   ;; lisp
   ((memq major-mode (list 'emacs-lisp-mode
                           'lisp-interaction-mode))
    (oops-lisp-jump-to-definition-atpt)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

;;;###autoload
(defun oops-goto-global-symbol ()
  ""
  (interactive)
  (cond
   ;; lisp
   ((memq major-mode (list 'emacs-lisp-mode
                           'lisp-interaction-mode))
    )
   ;; c
   ;; c++
   ;; python
   )
  )

;;;###autoload
(defun oops-goto-local-symbol ()
  (interactive)
  (cond
   ;; lisp
   ((memq major-mode (list 'emacs-lisp-mode
                           'lisp-interaction-mode))
    (oops-lisp-goto-lsymb)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

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
   ((eq last-command 'mode-exited) nil
    )
   ((region-active-p)
    (deactivate-mark)
    )
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit)
    )
   (current-prefix-arg
    nil)
   ((> (recursion-depth) 0)
    (exit-recursive-edit)
    )
   (buffer-quit-function
    (funcall buffer-quit-function)
    )
   ;; ((not (one-window-p t))
   ;;  (delete-other-windows)
   ;;  )
   ((string-match "^ \\*" (buffer-name (current-buffer)))
    (bury-buffer)
    )
   )
  )

;;; Others =====================================================================

;;;###autoload
(define-minor-mode oops-mode
  ""
  :lighter " Oops"
  :global t

  (if oops-mode
      ;; Enable.
      (progn (dolist (l oops--hooks)
               (add-hook (car l) (cdr l)))

             (oops--init-idtimer 1)
             (oops-win-mode 1)
             )
    ;; Disable.
    (progn (dolist (l oops--hooks)
             (remove-hook (car l) (cdr l)))

           (oops--init-idtimer -1)
           (oops-win-mode -1)
           )
    )
  )

(provide 'oops)
