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

;;;###autoload
(defun company-jedi-backend (command &rest args)
  (interactive (list 'interactive))
  (case command
    (init)
    ;; (interactive (company-jedi-backend))
    (prefix)
    (candidates)
    (location)
    (no-cache t)))

;; `company-files'
;; (defun company-files (command &optional arg &rest ignored)
;;   "`company-mode' completion back-end existing file names.
;; Completions works for proper absolute and relative files paths.
;; File paths with spaces are only supported inside strings."
;;   (interactive (list 'interactive))
;;   (cl-case command
;;     (interactive (company-begin-backend 'company-files))
;;     (prefix (company-files--grab-existing-name))
;;     (candidates (company-files--complete arg))
;;     (location (cons (dired-noselect
;;                      (file-name-directory (directory-file-name arg))) 1))
;;     (sorted t)
;;     (no-cache t)))

(provide 'company-jedi-backend)
