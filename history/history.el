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
;; 2014-??-?? (0.0.1)
;;    Initial release.

(require 'history-lisp)
(eval-when-compile (require 'cl))

(defgroup history nil
  "History.")

(defcustom his-history-max 64
  "The maximum history count."
  :type 'integer
  :group 'history)

(defcustom his-history-backends '(('emacs-lisp-mode . nil)
                                  (nil . nil))
  "The history backends."
  ;; :type '
  :group 'history)

(defvar his--history-list nil
  "")

(defvar his--history-index 0
  "")

(defun his--clean-history ()
  (cond
   ;; lisp
   ((memq major-mode (list 'emacs-lisp-mode
                           'lisp-interaction-mode))
    (oops-lisp-clean-history)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

;;;###autoload
(defun his-prev-history ()
  "Navigate to previous record in the history."
  (interactive)
  (cond
   ;; lisp
   ((memq major-mode (list 'emacs-lisp-mode
                           'lisp-interaction-mode))
    (oops-lisp-prev-history)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

;;;###autoload
(defun his-next-history ()
  "Navigate to next record in the history."
  (interactive)
  (cond
   ;; lisp
   ((memq major-mode (list 'emacs-lisp-mode
                           'lisp-interaction-mode))
    (oops-lisp-next-history)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

(provide 'history)
