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
;; TODO:
;; - Support keymap (menu and toolbar).
;;           `prj-create-project', `prj-delete-project',
;;           `prj-load-project', `prj-unload-project',
;;           `prj-build-database', `prj-find-file'.
;; - Support database using more efficient way (e.g. hash map).
;; - Support category search.
;; - Support project's local variable.
;; - Modulize widget function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-08-01 (0.0.1)
;;    Initial release.

(require 'ido)
(require 'prj-widget)
(require 'search-list)

(defgroup prj-group nil
  "A Project management utility.")

(defun prj-cus-set-workspace (symbol value)
  "Make sure the directory is present."
  (when (stringp value)
    (unless (file-exists-p value)
      (make-directory value))
    (when (file-exists-p value)
      (set symbol (expand-file-name value)))))

(defcustom prj-workspace-path "~/.emacs.d/.workspace"
  "The place storing all the projects' configurations."
  :type '(string)
  :set 'prj-cus-set-workspace
  :group 'prj-group)

(defcustom prj-document-types '(("Text" . "*.txt")
				("Lisp" . ".emacs;*.el")
				("Python" . "*.py")
				("Java" . "*.java")
				("C/C++" . "*.h;*.c;*.hpp;*.cpp")
				("Makfile" . "Makefile;makefile;Configure.ac;configure.ac;*.mk")
				("UEFI metafile" . "*.dsc;*.fdf;*.inf;*.env")
				("UEFI HII metafile" . "*.vfr;*.uni"))
  "Categorize file names refer to specific matches and give them type names. It is a list of (DOC_NAME . MATCHES). Each matches in MATCHES should be delimit with ';'."
  ;; TODO: give GUI a pretty appearance.
  :type '(repeat (cons string string))
  :group 'prj-group)

(defcustom prj-exclude-types ".git;.svn;.#*"
  "Those kinds of file should be excluded in the project. Each matches should be delimit with ';'."
  ;; TODO: give GUI a pretty appearance.
  :type '(string)
  :group 'prj-group)

(defconst prj-config-name "config.db"
  "The file name of project configuration.")

(defconst prj-filedb-name "files.db"
  "The file name of project file-list database.")

(defconst prj-searchdb-name "search.db"
  "The simple text file which caches the search result that users have done in the last session.")

(defvar prj-config nil
  "A hash map which represent project's configuration.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-config-path ()
  (expand-file-name (format "%s/%s/%s" prj-workspace-path (prj-project-name) prj-config-name)))

(defun prj-filedb-path ()
  (expand-file-name (format "%s/%s/%s" prj-workspace-path (prj-project-name) prj-filedb-name)))

(defun prj-searchdb-path ()
  (expand-file-name (format "%s/%s/%s" prj-workspace-path (prj-project-name) prj-searchdb-name)))

(defun prj-project-name ()
  "The current project's name."
  (gethash :name prj-config))

(defun prj-project-doctypes ()
  "The current project's document types value."
  (gethash :doctypes prj-config))

(defun prj-project-filepaths ()
  "The current project's file path value."
  (gethash :filepaths prj-config))

(defun prj-project-searchdb ()
  "The current project's search value."
  (gethash :search prj-config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-project-p ()
  "Return t if any project was loaded (current project)."
  (and (hash-table-p prj-config)
       (gethash :name prj-config)))

(defun prj-new-config ()
  "Return a new config (hash map)."
  (let ((config (make-hash-table :test 'equal)))
    (puthash :name nil config)
    (puthash :doctypes '() config)
    (puthash :filepaths '() config)
    (garbage-collect)
    config))

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

(defun prj-build-filedb-internal (path match)
  "Return a list that is made by recursively scan `dir' with file name which matches the regexp `match'."
  (let* ((fs (and (file-directory-p path)
		  (directory-files path t "[^.]$" t)))
	 res)
    (if fs
	;; A directory.
	(dolist (f fs)
	  (setq res (append res (prj-build-filedb-internal f match))))
      ;; A file.
      (message "Building database and may take a moment.\nScan ...%s" path)
      (and (string-match match path)
	   (setq res (append res (list path)))))
    res))

(defun prj-build-filedb ()
  "Create a list that contains all the files which should be included in the current project. Export the list to a file."
  (let (match db)
    ;; Prepare regexp for filtering file names.
    (setq match (concat
		 ;; Skip file name that starts with '.', '~' and '#'.
		 "^[^.~#].*\\("
		 (let (reg)
		   (dolist (doctype (prj-project-doctypes))
		     (let ((type (cdr doctype)))
		       ;; Replace ';' with "\\|"; "*." or '.' with "\\."
		       (setq reg (concat
				  reg
				  (replace-regexp-in-string "\\(\\.\\|\\*\\.\\)" "\\\\."
							    (replace-regexp-in-string ";" "\\\\|" type t) t)
				  "\\|"))))
		   reg)
		 "\\)$")
	  ;; Replace redundant syntax, "\\|\\)" with "\\)".
	  match (replace-regexp-in-string "\\\\|\\\\)" "\\\\)" match))
    ;; Search recursively in the project.
    (dolist (f (prj-project-filepaths))
      (setq db (append db (prj-build-filedb-internal f match))))
    ;; Export database.
    (prj-export-data (prj-filedb-path) db)
    (message (format "[%s] Database is updated!" (prj-project-name)))))

(defun prj-build-tags ()
  ;; TODO: implemnt it.
  )

;;;###autoload
(defun prj-preference ()
  "Customize document types."
  (interactive)
  (customize-group 'prj-group))

;;;###autoload
(defun prj-create-project ()
  "Show configuration for creating new project."
  (interactive)
  (prj-setup-create-project-widget))

;;;###autoload
(defun prj-delete-project ()
  "Show configuration for deleting projects."
  (interactive)
  (prj-setup-delete-project-widget))

;;;###autoload
(defun prj-edit-project ()
  "Show configuration for editing project's setting."
  (interactive)
  (unless (prj-project-p)
    (error "[Prj] There's no project was loaded."))
  (prj-setup-edit-project-widget))

;;;###autoload
(defun prj-load-project ()
  "List available prjects in current workspace and let user to choose which project to be loaded."
  (interactive)
  (let (choices)
    ;; Find available directories which represent a project.
    (dolist (f (directory-files prj-workspace-path))
      (let ((config-file (format "%s/%s/%s" prj-workspace-path f prj-config-name)))
	(when (file-exists-p config-file)
	  (push f choices))))
    ;; Prompt user to load project.
    (let ((c (ido-completing-read "[Prj] Load project: " choices)))
      (unless (member c choices)
	(error (format "[Prj] Can't load project invalid project, %s" c)))
      ;; Read configuration.
      (setq prj-config (prj-import-data (format "%s/%s/%s" prj-workspace-path c prj-config-name)))
      (message "[%s] Load project ...done" (prj-project-name)))))

;;;###autoload
(defun prj-unload-project ()
  "Unload current project."
  (interactive)
  (when (prj-project-p)
    (message "[%s] Unload project ...done" (prj-project-name))
    (setq prj-config (prj-new-config))))

;;;###autoload
(defun prj-build-database ()
  "Build file list and tags."
  (interactive)
  (unless (prj-project-p)
    (prj-load-project))
  ;; Create file list which is the data base of the project's files.
  (when (prj-project-p)
    (prj-build-filedb)
    (prj-build-tags)))

;;;###autoload
(defun prj-find-file ()
  "Open file by the given file name."
  (interactive)
  ;; Load project if wasn't loaded.
  (unless (prj-project-p)
    (prj-load-project))
  ;; Build database if is wasn't built.
  (unless (file-exists-p (prj-filedb-path))
    (prj-build-database))
  ;; Find file.
  (let* ((filedb (prj-import-data (prj-filedb-path)))
	 (file (ido-completing-read (format "[%s] Find file: " (prj-project-name)) filedb)))
    (unless (member file filedb)
      (error (format "[%s] Can't find %s!" (prj-project-name) file)))
    (find-file file))
  (garbage-collect))

;;;###autoload
(defun prj-search-project ()
  "Search string in the project. Append new search result to the old caches if `new' is nil."
  (interactive)
  ;; Load project if no project was loaded.
  (unless (prj-project-p)
    (prj-load-project))
  (prj-setup-search-project-widget))

;;;###autoload
(defun prj-toggle-search-buffer ()
  (interactive))

(provide 'prj)
