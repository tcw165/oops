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

(defconst sos-grep-prefix ">>>>> ")

(defvar sos-grep-keyword nil
  "The string after \">>>>> \" in the search buffer.")
(make-variable-buffer-local 'sos-grep-keyword)

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
           (if (search-forward-regexp "^.+:[0-9]+:" (line-end-position) t)
               ;; Return FILEPATH?NUM string.
               (let* ((full (buffer-substring-no-properties (line-beginning-position) (- (point) 1)))
                      (offset (string-match ":[0-9]+$" full))
                      (file (substring full 0 offset))
                      (linum (substring full (1+ offset)))
                      (symb (concat file "?" linum)))
                 (unless (string= symb sos-symbol)
                   (kill-local-variable 'sos-grep-keyword))
                 symb)
             :stop)))))
    (:candidates
     ;; 1st argument is FILEPATH?NUM string.
     (let* ((strings (split-string arg "?" t))
            (file (nth 0 strings))
            (linum (string-to-int (nth 1 strings)))
            (keyword (unless sos-grep-keyword
                       (save-excursion
                         (search-backward-regexp (concat "^" sos-grep-prefix ".+$") nil t)
                         (buffer-substring-no-properties (+ (length sos-grep-prefix) (point)) (line-end-position))))))
       ;; Set them for `sos-nav-mode'.
       (setq sos-file-name file
             sos-file-linum linum
             sos-grep-keyword keyword)
       (message "sos-file-linum = %s" sos-file-linum)
       `((:file ,file :linum ,linum :hl-word ,keyword))))
    (:tips nil)
    (:no-cache t)))

(provide 'sos-grep-backend)
