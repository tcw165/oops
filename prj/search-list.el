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
;; 2014-08-01 (0.0.1)
;;    Initial release.

;;;###autoload
(define-derived-mode search-list-mode special-mode "search-list"
  "Major mode for search buffers."
  :group 'search-list
  (message "search list~~~")
  )
;; (define-derived-mode emacs-lisp-mode prog-mode "Emacs-Lisp"
;;   "Major mode for editing Lisp code to run in Emacs.
;; Commands:
;; Delete converts tabs to spaces as it moves back.
;; Blank lines separate paragraphs.  Semicolons start comments.

;; \\{emacs-lisp-mode-map}
;; Entry to this mode calls the value of `emacs-lisp-mode-hook'
;; if that value is non-nil."
;;   :group 'lisp
;;   (lisp-mode-variables)
;;   (setq imenu-case-fold-search nil)
;;   (add-hook 'completion-at-point-functions
;;             'lisp-completion-at-point nil 'local))

(provide 'search-list)
