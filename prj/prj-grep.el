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

(defgroup prj-grep-group nil
  "A major mode for the text produced by GREP like applications."
  :tag "Prj Grep")

(defcustom prj-grep-mode-hook `(linum-mode
                                hl-line-mode
                                ,(and (featurep 'sos)
                                      'sos-reference-window-mode))
  "Hook run when entering `prj-grep-mode' mode."
  :type 'hook
  :group 'prj-grep-group)

;;;###autoload
(define-derived-mode prj-grep-mode nil "Grep"
  "Major mode for search buffers."
  :group 'search-list-group
  )

(provide 'prj-grep)
