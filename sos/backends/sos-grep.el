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

(defun sos-grep-backend (command &optional arg)
  (case command
    (:init t)
    (:symbol
     (when (eq major-mode (or (and (featurep 'prj-grep)
                                   'prj-grep-mode)
                              (and (featurep 'compile)
                                   'compilation-mode)))
       (unless mark-active
         (save-excursion
           (beginning-of-line)
           (if (search-forward-regexp "^[-a-zA-Z0-9._/\\ :]+:[0-9]+:"
                                      (line-end-position) t)
               ;; Return FILEPATH:LINENO string.
               (buffer-substring-no-properties (line-beginning-position) (- (point) 1))
             :stop)))))
    (:candidates
     ;; 1st argument is FILEPATH:LINENO string.
     (let* ((strings (split-string arg ":"))
            (file (car strings))
            (linum (string-to-int (cadr strings))))
       ;; TODO: `:hl-line' to `:hl-word'.
       `((:file ,file :linum ,linum :hl-line t))))
    (:tips nil)
    (:no-cache t)))

(provide 'sos-grep)
