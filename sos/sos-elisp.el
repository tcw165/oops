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

(defun sos-elisp-backend (command &rest args)
  (case command
    (:init t)
    (:symbol
     (when (member major-mode '(emacs-lisp-mode lisp-interaction-mode))
       "test-symbol"))
    (:candidates
     ;; Single candidate case:
     (let* ((num (random 1000))
            (i (% num 10)))
       (list (nth i '((:file "~/.emacs.d/oops/sos/text1.txt" :offset 5)
                      (:file "~/.emacs.d/oops/sos/text2.txt" :offset 10)
                      (:file "~/.emacs.d/oops/sos/text3.txt" :offset 15)
                      (:file "~/.emacs.d/oops/sos/text4.txt" :offset 20)
                      (:file "~/.emacs.d/oops/sos/text5.txt" :offset 25)
                      (:file "~/.emacs.d/oops/sos/text6.txt" :offset 30)
                      (:file "~/.emacs.d/oops/sos/text7.txt" :offset 35)
                      (:file "~/.emacs.d/oops/sos/text8.txt" :offset 40)
                      (:file "~/.emacs.d/oops/sos/text9.txt" :offset 45)
                      (:file "~/.emacs.d/oops/sos/text10.txt" :offset 50)
                      )))))
    (:tips
     (format "Test tips %s" (current-time)))
    (:no-cache t)))
;; (sos-elisp-backend :candidates)
;; a b c d
;; (with-current-buffer sos-reference-buffer (goto-char 14))

(provide 'sos-elisp)
