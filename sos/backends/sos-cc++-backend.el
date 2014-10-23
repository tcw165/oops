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

;; (require 'etags)

(require 'prj)

(defconst sos-cc++-regexp ".*\\.[ch]\\(pp\\|xx\\)$")

(defvar sos-cc++-process-tag nil)

(defun sos-cc++-tag ()
  (concat (prj-project-config-dir) "/tags/ccpp.tags"))

;; (find-tag)
;; (visit-tags-table sos-cc++-tag)
(defun sos-cc++-async-tag-complete (process message)
  (cond
   ((eq (process-status process) 'run))
   ((memq (process-status process) '(stop exit signal))
    (setq sos-cc++-process-tag nil)
    (message (format "c/c++ tags is created! %s" (process-status process))))))

(defun sos-cc++-async-tag (input-files)
  (setq sos-cc++-process-tag
        (start-process-shell-command
         "sos-cc++-tag"
         nil
         (format "grep \"%s\" \"%s\" | xargs etags -o \"%s\""
                 sos-cc++-regexp
                 input-files
                 (sos-cc++-tag))))
  ;; Add process sentinel.
  (set-process-sentinel sos-cc++-process-tag 'sos-cc++-async-tag-complete))

(defun sos-cc++-init-tag ()
  (let* ((tag-file (sos-cc++-tag))
         (tag-dir (file-name-directory tag-file)))
    (unless (file-exists-p tag-dir)
      (make-directory tag-dir t))
    (unless (file-exists-p tag-file)
      (sos-cc++-async-tag (prj-project-files)))))

;;;###autoload
(defun sos-cc++-backend (command &rest arg)
  (case command
    (:init
     (add-hook 'prj-after-build-database-hook 'sos-cc++-init-tag)
     (sos-cc++-init-tag))
    (:symbol)
    (:candidates)))

(provide 'sos-cc++-backend)
