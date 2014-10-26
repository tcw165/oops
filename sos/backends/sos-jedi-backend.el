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

(require 'thingatpt)

(require 'hl-faces)
(require 'jedi)

;;;###autoload
(defun sos-jedi-backend (command &rest args)
  (case command
    (:symbol
     (when (and (eq major-mode 'python-mode)
                jedi-mode)
       (or (thing-at-point 'symbol) :stop)))
    (:candidates
     ;; 1st argument is (FILEPATH . NUM) struct.
     ;; (let* ((symb (car args))
     ;;        (file (car symb))
     ;;        (linum (cdr symb))
     ;;        (match (save-excursion
     ;;                 (search-backward-regexp
     ;;                  (concat "^" sos-grep-prefix ".+$") nil t)
     ;;                 (buffer-substring-no-properties
     ;;                  (+ (length sos-grep-prefix) (point))
     ;;                  (line-end-position))))
     ;;        (keywords `((,match 0 'hl-symbol-face prepend))))
     ;;   `((:file ,file :linum ,linum :keywords ,keywords)))
     )))

(provide 'sos-jedi-backend)
