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

(defvar prj-files-cache nil)

(defvar prj-filedb-cache nil)

(defun prj-export-data (filename data)
  "Export `data' to `filename' file. The saved data can be imported with `prj-import-data'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length)
		(prin1-to-string data))))))

(defun prj-import-data (filename)
  "Read data exported by `prj-export-data' from file `filename'."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (read (buffer-string)))))

(defun prj-convert-matches (doctype)
  "Convert DOCTYPE to string as include-path parameter for FIND.
e.g. *.md;*.el;*.txt => -name *.md -o -name *.el -o -name *.txt"
  (and (stringp doctype)
       (let ((matches (concat "\"-name\" \"" doctype "\"")))
         (replace-regexp-in-string ";" "\" \"-o\" \"-name\" \"" matches))))

(defun prj-convert-excludes (doctype)
  "Convert DOCTYPE to string as exclude-path parameter for FIND.
e.g. .git;.svn => ! -name .git ! -name .svn"
  (and (stringp doctype)
       (let ((matches (concat "\"!\" \"-name\" \"" doctype "\"")))
         (replace-regexp-in-string ";" "\" \"!\" \"-name\" \"" matches))))

(defun prj-process-find (filepaths matches excludes)
  "Use `call-process' to call FIND. FILEPATHS is a file list, (FILE1 FILE2 ...).
 MATCHES and EXCLUDES is strings in format of \"-name pattern\"."
  (let* ((matches (prj-convert-matches matches))
         (excludes (prj-convert-excludes excludes))
         (stream (with-output-to-string
                   (princ "(call-process \"find\" nil (list (current-buffer) nil) nil ")
                   (dolist (filepath filepaths)
                     (prin1 filepath)(princ " "))
                   (princ matches)(princ " ")
                   (princ excludes)(princ ")")))
         (output (with-temp-buffer
                   (eval (read stream))
                   (buffer-string))))
    (and output (split-string output "\n" t))))

(defun prj-process-find-change ()
  ;; TODO:
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-filedb-backend (command &rest args)
  (case command
    (:init
     (unless prj-filedb-cache
       (setq prj-filedb-cache (prj-import-data (prj-filedb-path))))
     (unless prj-files-cache
       (let ((filedb prj-filedb-cache))
         (while filedb
           (setq prj-files-cache (append prj-files-cache (cadr filedb))
                 filedb (cddr filedb))))))
    (:index-files
     (let ((is-rebuild (car args))
           (doctypes (prj-project-doctypes))
           files
           filedb)
       (and is-rebuild (file-exists-p (prj-filedb-path))
            (delete-file (prj-filedb-path)))
       (while doctypes
         (prj-plist-put filedb
                        (car doctypes)
                        (setq files (prj-process-find
                                     (prj-project-filepaths)
                                     (cadr doctypes)
                                     prj-exclude-types)))
         (setq prj-files-cache (append prj-files-cache files)
               ;; Next.
               doctypes (cddr doctypes)))
       ;; Export database.
       (setq prj-filedb-cache filedb)
       (prj-export-data (prj-filedb-path) filedb)))
    (:files
     ;; TODO: Files for specific document type?
     prj-files-cache)
    (:destroy
     (setq prj-filedb-cache nil
           prj-filedb-cache nil)
     (garbage-collect))))

;;;###autoload
(defun prj-tagdb-backend (command &rest args)
  (case command
    (:tag-files)))

(provide 'prj-default-backend)
