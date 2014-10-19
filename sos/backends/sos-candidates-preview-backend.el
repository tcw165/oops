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

(require 'tabulated-list)

(require 'hl-anything)

;;;###autoload
(defun sos-candidates-preview-backend (command &rest args)
  (case command
    (:symbol
     (when (string= (buffer-name) "*Goto Definition*")
       (let ((entry (tabulated-list-get-entry)))
         (if (and entry (not mark-active))
             ;; Return the entry of `tabulated-list-entries'.
             entry
           :stop))))
    (:candidates
     ;; 1st argument is an entry of `tabulated-list-entries'.
     (let* ((entry (car args))
            (symb (aref entry 0))
            (linum (string-to-int (aref entry 2)))
            (file (aref entry 3))
            (doc (aref entry 4))
            keywords)
       (cond
        (doc
         (setq keywords `((,(regexp-quote (with-temp-buffer
                                            (insert doc)
                                            (goto-char 1)
                                            (buffer-substring-no-properties
                                             1 (line-end-position))))
                           0 'hl-symbol-face prepend)))
         `((:doc ,doc :linum ,linum :keywords ,keywords)))
        (file
         (setq keywords `((,(regexp-quote symb) 0 'hl-symbol-face prepend)))
         `((:file ,file :linum ,linum :keywords ,keywords))))))))

(provide 'sos-candidates-preview-backend)
