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
;; This is a framework that refers to the point and show useful information.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-10-01 (0.0.1)
;;    Initial release.

(defun sos-definition-buffer-frontend (command &rest args)
  (case command
    (:show
     ;; TODO: multiple candidates `sos-is-single-candidate'.
     (if (sos-is-single-candidate)
         (let* ((candidate (car sos-candidates))
                (file (plist-get candidate :file))
                (offset (plist-get candidate :offset))
                (linum (plist-get candidate :linum))
                (hl-line (plist-get candidate :hl-line))
                (hl-word (plist-get candidate :hl-word)))
           (when (file-exists-p file)
             (sos-with-definition-buffer
               (insert-file-contents file nil nil nil t)
               ;; Set it for `sos-nav-mode'.
               (setq sos-file-name file)
               ;; Find a appropriate major-mode for it.
               (dolist (mode auto-mode-alist)
                 (and (not (null (cdr mode)))
                      (string-match (car mode) file)
                      (funcall (cdr mode))))
               ;; Move point and recenter.
               (or (and linum (goto-char (point-min))
                        (forward-line (- linum 1)))
                   (and offset (goto-char offset)))
               (recenter 3)
               ;; Highlight word or line.
               (or (and (stringp hl-word)
                        ;; TODO: hl-word.
                        ;; (move-overlay)
                        )
                   (and hl-line
                        (move-overlay sos-hl-overlay (line-beginning-position) (+ 1 (line-end-position))))))))))
    (:hide nil)
    (:update nil)))

(defun sos-tips-frontend (command &rest args)
  (case command
    (:show
     (when (stringp sos-tips)
       ;; TODO: draw a overlay.
       ;; (message "%s" sos-tips)
       ))
    (:hide nil)
    (:update nil)))

(provide 'sos-basic-frontend)
