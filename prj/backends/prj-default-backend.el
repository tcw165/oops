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

(defconst prj-filedb-name "files.db"
  "The file name of project file-list database. The database is a plist which 
contains files should be concerned.
format:
  (DOCTYPE1 (FILE1_1 FILE1_2 ...)
   DOCTYPE2 (FILE2_1 FILE2_2 ...))")

(defconst prj-searchdb-name "search.grep"
  "The simple text file which caches the search result that users have done 
in the last session.")

(defvar prj-files-cache nil)

(defvar prj-filedb-cache nil)

(defvar prj-process-grep nil)

(defun prj-filedb-path ()
  (expand-file-name (format "%s/%s/%s"
                            prj-workspace-path
                            (prj-project-name)
                            prj-filedb-name)))

(defun prj-searchdb-path ()
  (expand-file-name (format "%s/%s/%s"
                            prj-workspace-path
                            (prj-project-name)
                            prj-searchdb-name)))

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

(defun prj-process-grep (match filepaths sentinel)
  "Use `start-process' to call GREP. MATCH is a string to search. FILEPATHS is 
a file list, (FILE1 FILE2 ...). SENTINEL is the GREP's sentinel."
  ;; (shell-command "find ~/.emacs.d -name \"*.el\"|xargs grep -nH \"garbage-collect\"" (find-file-noselect (prj-searchdb-path)) nil)
  (let ((stream (with-output-to-string
                  (princ "(start-process \"grep\" (current-buffer) \"grep\" ")
                  (prin1 "-snH")(princ " ")
                  (prin1 match)(princ " ")
                  (dolist (filepath filepaths)
                    (prin1 filepath)(princ " "))
                  (princ ")"))))
    (setq prj-process-grep (eval (read stream)))
    (set-process-sentinel prj-process-grep sentinel)))

(defmacro prj-with-search-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(with-current-buffer (find-file-noselect (prj-searchdb-path))
     ,@body))

(defun prj-async-grep-complete (process message)
  "A sentinel for asynchronous process GREP."
  (when (memq (process-status process) '(stop exit signal))
    (prj-with-search-buffer
      (goto-char (point-max))
      (insert "<<<<<\n\n")
      (save-buffer 0)
      (setq buffer-read-only t)
      (message "Search project...done"))))

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
(defun prj-async-grep-backend (command &rest args)
  (case command
    (:search
     (if (and (processp prj-process-grep)
              (process-live-p prj-process-grep))
         (message "Searching is under processing, please wait...")
       (let* ((data (car args))
              (match (plist-get data :match))
              (doctypes (plist-get data :doctypes))
              (filepaths (plist-get data :filepaths))
              (casefold (plist-get data :casefold))
              (word-only (plist-get data :word-only))
              (skip-comment (plist-get data :skip-comment)))
         ;; Update search history.
         (let ((history (prj-project-search-history)))
           (if (member match history)
               ;; Reorder the match.
               (setq history (delete match history)
                     history (push match history))
             (setq history (push match history)))
           ;; Keep maximum history.
           (and (> (length history) prj-search-history-max)
                (setcdr (nthcdr (1- prj-search-history-max) history) nil))
           (prj-plist-put prj-config :search-history history)
           (prj-export-json (prj-config-path) prj-config))
         ;; Start to search.
         (prj-with-search-buffer
           (switch-to-buffer (current-buffer) nil t)
           (goto-char (point-max))
           (insert (format ">>>>> %s\n" match))
           (prj-process-grep match (prj-project-files)
                             'prj-async-grep-complete)))))))

(provide 'prj-default-backend)
