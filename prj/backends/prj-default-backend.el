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

(defvar prj-total-files-cache nil)

(defvar prj-process-grep nil)

(defun prj-filedb-path (&optional name)
  (let ((dir (expand-file-name (format "%s/%s/files"
                                       prj-workspace-path
                                       (prj-project-name)))))
    (if name
        (format "%s/%s.files" dir (or (and (string= name "all") name)
                                      (secure-hash 'sha1 name)))
      dir)))

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

(defun prj-convert-filepaths (filepaths)
  (let ((filepath filepaths)
        ret)
    (while filepath
      (setq ret (concat ret (car filepath))
            filepath (cdr filepath))
      (and filepath
           (setq ret (concat ret " "))))
    ret))

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

(defun prj-process-find (output filepaths &optional matches excludes)
  "Use `call-process' to call FIND. FILEPATHS is a file list, (FILE1 FILE2 ...).
 MATCHES and EXCLUDES is strings in format of \"-name pattern\"."
  (let ((dir (file-name-directory output))
        (stream (shell-command-to-string (format "find %s %s %s"
                                                 filepaths
                                                 (or matches "")
                                                 (or excludes "")))))
    (and dir (make-directory dir t))
    (with-temp-file output
      (insert stream)
      stream)))

(defun prj-process-find-change ()
  ;; TODO:
  t)

(defmacro prj-with-search-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(with-current-buffer (find-file-noselect (prj-searchdb-path))
     ,@body))

(defun prj-process-grep (match input-file)
  (find-file (prj-searchdb-path))
  (setq prj-process-grep (start-process-shell-command
                          "prj-process-grep"
                          (current-buffer)
                          (format "xargs grep -snH \"%s\" < \"%s\" 2>/dev/null"
                                  match
                                  input-file)))
  (and (processp prj-process-grep)
       (set-process-sentinel prj-process-grep 'prj-async-grep-complete)))

(defun prj-async-grep-complete (process message)
  (when (memq (process-status process) '(stop exit signal))
    (prj-with-search-buffer
      (goto-char (point-max))
      (unless (looking-back "\n")
        (insert "\n"))
      (insert "<<<<<\n\n")
      (save-buffer 0)
      (setq buffer-read-only t
            prj-process-grep nil)
      (message "Search project...done"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-filedb-backend (command &rest args)
  (case command
    (:init)
    (:destroy
     (setq prj-total-files-cache nil)
     (garbage-collect))
    (:index-files
     (let ((is-rebuild (car args))
           all-files)
       ;; Collect files in respect of document types.
       (dolist (doctype (prj-project-doctypes))
         (setq all-files (concat all-files (prj-process-find
                                            (prj-filedb-path doctype)
                                            (prj-convert-filepaths (prj-project-filepaths))
                                            (prj-convert-matches (cdr (assoc doctype prj-document-types)))
                                            (prj-convert-excludes prj-exclude-types)))))
       ;; Export all database.
       (when all-files
         (setq prj-total-files-cache (split-string all-files "\n" t))
         (with-temp-file (prj-filedb-path "all")
           (insert all-files)))))
    (:files
     ;; TODO: Files for specific document type?
     prj-total-files-cache)))

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
           ;; TODO: support filepaths and doctypes
           (prj-process-grep match (prj-filedb-path "all"))))))))

(provide 'prj-default-backend)
