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

(defconst ws-cc++-regexp ".*\\.[ch]\\(pp\\|xx\\)$")

(defvar ws-cc++-process-tag nil)

(defun ws-cc++-tag ()
  (concat (prj-project-config-dir) "/tags/ccpp.tags"))

;; (find-tag)
;; (visit-tags-table ws-cc++-tag)
(defun ws-cc++-async-tag-complete (process message)
  (cond
   ((eq (process-status process) 'run))
   ((memq (process-status process) '(stop exit signal))
    (setq ws-cc++-process-tag nil)
    (message "c/c++ tags is updated! %s" (process-status process)))))

(defun ws-cc++-async-tag (input-files)
  (setq ws-cc++-process-tag
        (start-process-shell-command
         "ws-cc++-tag"
         nil
         (format "grep \"%s\" \"%s\" | xargs etags -o \"%s\""
                 ws-cc++-regexp
                 input-files
                 (ws-cc++-tag))))
  ;; Add process sentinel.
  (set-process-sentinel ws-cc++-process-tag 'ws-cc++-async-tag-complete))

(defun ws-cc++-build-tag ()
  (let* ((tag-file (ws-cc++-tag))
         (tag-dir (file-name-directory tag-file)))
    (unless (file-exists-p tag-file)
      (and (file-exists-p tag-dir)
           (delete-directory tag-dir t))
      (make-directory tag-dir t)
      (ws-cc++-async-tag (prj-project-files)))))

;;;###autoload
(defun ws-cc++-backend (command &rest arg)
  (case command
    (:init
     ;; (when (require 'prj)
     ;;   (add-hook 'prj-after-build-database-hook 'ws-cc++-build-tag)
     ;;   (ws-cc++-build-tag))
     )
    (:symbol)
    (:candidates)))

(provide 'ws-cc++-backend)
