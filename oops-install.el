;;; oops-install.el --- Install oops and its prerequisities.
;;
;; Copyright (C) 2014, 2015
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
(dolist (arch '(("melpa-stable" . "https://stable.melpa.org/packages/")
                ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives arch t))
;; Activate all the packages (in particular autoloads).
(package-initialize)
;; Fetch the list of packages available.
(package-refresh-contents)
;; Install the missing packages.
(dolist (package '(;; Packages Owned by Myself
                   history
                   hl-anything
                   searchq
                   ;; Async
                   deferred
                   exec-path-from-shell
                   ;; Key
                   guide-key
                   ;; Edit.
                   smart-shift
                   ;; Version Control.
                   diff-hl
                   ;; Completion Framework.
                   company
                   helm
                   ;; Yasnippet
                   yasnippet
                   ;; Project Management.
                   projectile
                   ;; YCMD
                   ycmd
                   company-ycmd
                   ;; Json
                   json-mode
                   ;; JavaScript
                   tern
                   company-tern
                   ;; Jade/Styl
                   jade-mode
                   stylus-mode))
  (unless (package-installed-p package)
    (package-install package)))

(provide 'oops-install)
;;; oops-install.el ends here
