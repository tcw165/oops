;;; oops-install.el --- Install oops and its prerequisities.
;;
;; Copyright (C) 2014
;;
;; Author: boyw165
;; Package-Requires: ((emacs "24.3"))
;; Compatibility: GNU Emacs 24.3+
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
;; Install oops's prerequsities.
;;
;;; Code:

(require 'package)
(dolist (arch '(("elpa" . "http://melpa.milkbox.net/packages/")
                ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives arch t))
;; activate all the packages (in particular autoloads)
(package-initialize)
;; fetch the list of packages available 
(package-refresh-contents)
;; install the missing packages
(dolist (package '(hl-anything
                   smart-shift
                   deferred
                   grizzl
                   company
                   company-ycmd
                   ycmd
                   helm
                   exec-path-from-shell))
  (unless (package-installed-p package)
    (package-install package)))

(provide 'oops-install)
;;; oops-install.el ends here
