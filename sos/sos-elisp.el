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
       (list (nth i '((:file "~/.emacs.d/oops/oops.el" :offset 1)
                      (:file "~/.emacs.d/oops/prj/prj.el" :offset 1428)
                      (:file "~/.emacs.d/elpa/projectile-20140811.855/projectile.el" :offset 2)
                      (:file "~/.emacs.d/elpa/async-20140311.747/async.el" :offset 3)
                      (:file "~/.emacs.d/elpa/company-20140817.1917/company.el" :offset 4)
                      (:file "~/.emacs.d/elpa/helm-20140819.759/helm.el" :offset 5)
                      (:file "~/.emacs.d/elpa/helm-20140819.759/helm-buffers.el" :offset 6)
                      (:file "~/.emacs.d/oops/history/history.el" :offset 7)
                      (:file "~/.emacs.d/oops/hl-anything/hl-anything.el" :offset 8)
                      (:file "~/.emacs.d/elpa/company-20140817.1917/company-cmake.el" :offset 9)
                      (:file "~/.emacs.d/elpa/company-20140817.1917/company-template.el" :offset 10))))))
    (:tips
     (format "Test tips %s" (current-time)))
    (:no-cache t)))
;; (sos-elisp-backend :candidates)

(provide 'sos-elisp)
