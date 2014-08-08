
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-08-01 (0.0.1)
;;    Initial release.

(require 'ido)
(require 'cus-edit)
(require 'widget)
(require 'wid-edit)
(require 'search-list)

(defgroup prj-group nil
  "Project.")

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

(defcustom prj-document-types '(("Text" . ".txt")
				("Lisp" . ".emacs;*.el")
				("Python" . "*.py")
				("Java" . "*.java")
				("C/C++" . "*.h;*.c;*.hpp;*.cpp")
				("Makfile" . "Makefile;makefile;Configure.ac;configure.ac;*.mk")
				("UEFI metafile" . "*.dsc;*.fdf;*.inf;*.env")
				("UEFI HII metafile" . "*.vfr;*.uni")
				)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-customize-document-types ()
  "Customize document types."
  (interactive)
  (customize-option 'prj-document-types))

;;;###autoload
(defun prj-customize-excluded-types ()
  "Customize excluded types."
  (interactive)
  (customize-option 'prj-exclude-types))

;;;###autoload
(defun prj-customize-workspace-path ()
  "Customize workspace path."
  (interactive)
  (customize-option 'prj-workspace-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-config-path ()
  (expand-file-name (concat prj-workspace-path "/" (prj-project-name) "/" prj-config-name)))

(defun prj-filedb-path ()
  (expand-file-name (concat prj-workspace-path "/" (prj-project-name) "/" prj-filedb-name)))

(defun prj-searchdb-path ()
  (expand-file-name (concat prj-workspace-path "/" (prj-project-name) "/" prj-searchdb-name)))

(defvar prj-config nil
  "A hash map which represent project's configuration.")

(defvar prj-tmp-config nil
  "A temporary hash map.")

(defun prj-project-name ()
  "The current project's name."
  (gethash :name prj-config))

(defun prj-project-doctypes ()
  "The current project's document types."
  (gethash :doctypes prj-config))

(defun prj-project-filepaths ()
  "The current project's file path."
  (gethash :filepaths prj-config))

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
    (puthash :search '() config)
    (garbage-collect)
    config))

(defun prj-validate-config-filepaths ()
  (let ((valid-fp '()))
    (dolist (f (gethash :filepaths prj-config))
      (let ((fp (and (file-exists-p f)
		     (expand-file-name f))))
	(and fp
	     (push fp valid-fp))))
    (and valid-fp
	 (puthash :filepaths valid-fp prj-config))))

(defun prj-export-data (filename data)
  "Export `data' to `filename' file. The saved data can be imported with `prj-import-data'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length)
		(prin1-to-string data))))))

(defun prj-import-data (filename)
  "Read data exported by `prj-export-data' from `filename'."
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
      (when (string-match match path)
	(setq res (append res (list path)))
	(message "Building database and may take a moment.\nAdd ...%s" path)))
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

(defun prj-get-widget-buffer (name)
  "Create a widget buffer with fine setting and switch to it."
  (switch-to-buffer name)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (setq prj-tmp-config (prj-new-config))
  
  (set (make-local-variable 'widget-documentation-face) 'custom-documentation)
  (set (make-local-variable 'widget-button-face) custom-button)
  (set (make-local-variable 'widget-button-pressed-face) custom-button-pressed)
  (set (make-local-variable 'widget-mouse-face) custom-button-mouse)
  ;; When possible, use relief for buttons, not bracketing.
  (when custom-raised-buttons
    (set (make-local-variable 'widget-push-button-prefix) " ")
    (set (make-local-variable 'widget-push-button-suffix) " ")
    (set (make-local-variable 'widget-link-prefix) "")
    (set (make-local-variable 'widget-link-suffix) ""))
  (setq show-trailing-whitespace nil))

;;;###autoload
(defun prj-create-project ()
  "Create a new project."
  (interactive)
  (prj-get-widget-buffer "*Create Project*")

  ;; Widget start ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (widget-insert "=== Create New Project ===\n")
  (widget-insert "\n")
  (widget-create 'editable-field
		 :format "Project Name: %v"
		 :notify (lambda (wid &rest ignore)
			   (puthash :name (widget-value wid) prj-tmp-config)))
  (widget-insert "\n")
  (widget-insert "Document Types (Edit):\n") ;; TODO: add customizable link
  (dolist (type prj-document-types)
    (widget-put (widget-create 'checkbox
			       :format (concat "%[%v%] " (car type) " (" (cdr type) ")\n")
			       :notify (lambda (wid &rest ignore)
					 (if (widget-value wid)
					     (puthash
					      :doctypes
					      (push (widget-get wid :doctype) (gethash :doctypes prj-tmp-config))
					      prj-tmp-config)
					   (puthash
					    :doctypes
					    (delq (widget-get wid :doctype) (gethash :doctypes prj-tmp-config))
					    prj-tmp-config))))
		:doctype type))
  (widget-insert "\n")
  (widget-insert "Include Path:\n")
  (widget-create 'editable-list
		 :entry-format "%i %d %v"
		 :value '("")
		 :notify (lambda (wid &rest ignore)
			   (puthash :filepaths (widget-value wid) prj-tmp-config))
		 '(editable-field :value ""))
  (widget-insert "\n")
  ;; ok and cancel buttons.
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (message "[%s] Creating new project ..." (gethash :name prj-tmp-config))
			   (let* ((config-file-path (expand-file-name (concat
								       prj-workspace-path
								       "/"
								       (gethash :name prj-tmp-config)
								       "/"
								       prj-config-name)))
				  (dir (file-name-directory config-file-path)))
			     ;; Prepare valid file paths.
			     (let ((valid-fp '()))
			       (dolist (f (gethash :filepaths prj-tmp-config))
				 (let ((fp (and (file-exists-p f)
						(expand-file-name f))))
				   (and fp
					(push fp valid-fp))))
			       (and valid-fp
				    (puthash :filepaths valid-fp prj-tmp-config)))
			     ;; Return if there is an invalid info.
			     (unless (and (gethash :name prj-tmp-config)
					  (> (length (gethash :doctypes prj-tmp-config)) 0)
					  (> (length (gethash :filepaths prj-tmp-config)) 0))
			       (error "[Prj] Can't create new project due to invalid information."))
			     ;; Prepare directory. Directory name is also the project name.
			     (unless (file-directory-p dir)
			       (make-directory dir))
			     ;; Export configuration.
			     (prj-export-data config-file-path prj-tmp-config)
			     ;; Kill the "Create Project" form.
			     (kill-buffer)
			     (prj-load-project)))
		 "ok")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (kill-buffer))
		 "cancel")
  ;; Widget end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (goto-char 43)
  (use-local-map widget-keymap)
  (widget-setup))

;;;###autoload
(defun prj-delete-project ()
  "Delete projects which are no more needed."
  (interactive)
  (prj-get-widget-buffer "*Delete Project*")

  ;; Widget start ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (widget-insert "=== Delete Project ===\n")
  (widget-insert "\n")
  (widget-insert "Select projects to be deleted:\n")
  (let (choices)
    (dolist (f (directory-files prj-workspace-path))
      (let ((config-file (concat prj-workspace-path "/" f "/" prj-config-name)))
	(when (file-exists-p config-file)
	  (push f choices))))
    (unless choices
      (kill-buffer)
      (error "[Prj] No projects can be deleted."))
    (dolist (c choices)
      (widget-put (widget-create 'checkbox
				 :format (concat "%[%v%] " c "\n")
				 :notify (lambda (wid &rest ignore)
					   (if (widget-value wid)
					       (puthash :filepaths
							(push (widget-get wid :filepath) (gethash :filepaths prj-tmp-config))
							prj-tmp-config)
					     (puthash :filepaths
						      (delq (widget-get wid :filepath) (gethash :filepaths prj-tmp-config))
						      prj-tmp-config))))
		  :filepath c)))
  (widget-insert "\n")
  ;; ok and cancel buttons.
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (let ((cs (gethash :filepaths prj-tmp-config)))
			     (dolist (c cs)
			       ;; Unload current project if it is selected.
			       (when (and (prj-project-p)
					  (string-equal (prj-project-name) c))
				 (prj-unload-project))
			       ;; Delete directory
			       (delete-directory (expand-file-name (format "%s/%s" prj-workspace-path c)) t t))
			     )
			   (kill-buffer)
			   (message "[Prj] Delet project ...done"))
		 "ok")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (kill-buffer))
		 "cancel")
  ;; Widget end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (goto-char 56)
  (use-local-map widget-keymap)
  (widget-setup))

;;;###autoload
(defun prj-edit-project ()
  "Edit project coniguration."
  (interactive)
  
  (unless (prj-project-p)
    (error "[Prj] There's no project was loaded."))
  
  (prj-get-widget-buffer "*Edit Project*")

  ;; Widget start ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (widget-insert "=== Edit Project ===\n")
  (widget-insert "\n")
  (widget-insert (format "Project Name: %s\n" (prj-project-name)))
  (widget-insert "\n")
  (widget-insert "Document Types (Edit):\n") ;; TODO: add customizable link
  (dolist (type prj-document-types)
    (widget-put (widget-create 'checkbox
			       :format (concat "%[%v%] " (car type) " (" (cdr type) ")\n")
			       :value (let (res)
					(dolist (currtype (gethash :doctypes prj-config))
					  (if (string-equal (car currtype) (car type))
					      (setq res t)))
					res)
			       :notify (lambda (wid &rest ignore)
					 (if (widget-value wid)
					     (puthash
					      :doctypes
					      (push (widget-get wid :doctype) (gethash :doctypes prj-config))
					      prj-tmp-config)
					   (puthash
					    :doctypes
					    (delq (widget-get wid :doctype) (gethash :doctypes prj-config))
					    prj-tmp-config))))
		:doc-type type))
  (widget-insert "\n")
  (widget-insert "Include Path:\n")
  (widget-create 'editable-list
  		 :entry-format "%i %d %v"
  		 :value (gethash :filepaths prj-config)
  		 :notify (lambda (wid &rest ignore)
			   (puthash :filepaths
				    (widget-value wid)
				    prj-config))
  		 '(editable-field))
  (widget-insert "\n")
  ;; ok and cancel buttons.
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   ;; Validate file paths.
			   (prj-validate-config-filepaths)
			   ;; Export configuration.
			   (prj-export-data (prj-config-path) prj-config)
			   ;; Kill the "Edit Project" form.
			   (kill-buffer))
		 "ok")
  (widget-insert " ") 
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (kill-buffer))
		 "cancel")[cancel]
		 ;; Widget end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		 (goto-char 37)
		 (use-local-map widget-keymap)
		 (widget-setup))

;;;###autoload
(defun prj-load-project ()
  "List available prjects in current workspace and let user to choose which project to be loaded."
  (interactive)
  (let (choices)
    ;; Find available directories which represent a project.
    (dolist (f (directory-files prj-workspace-path))
      (let ((config-file (concat prj-workspace-path "/" f "/" prj-config-name)))
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

;; (prj-import-data (prj-filedb-path))
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
  ;; TODO: fix it.
  "Search string in the project. Append new search result to the old caches if `new' is nil."
  (interactive)
  ;; Load project if no project was loaded.
  (unless (prj-project-p)
    (prj-load-project))

  (prj-get-widget-buffer "*Search Project*")

    ;; Widget start ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (widget-insert "=== Search Project ===\n")
  (widget-insert "\n")
  (widget-create 'editable-field
		 :format "Search: %v"
		 :notify (lambda (wid &rest ignore)
			   ))
  (widget-insert "\n")
  (widget-insert "Document Types ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   )
		 "Select All")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   )
		 "Deselect All")
  (widget-insert " :\n")
  (dolist (type (prj-project-doctypes))
    (widget-put (widget-create 'checkbox
			       :format (concat "%[%v%] " (car type) " (" (cdr type) ")\n")
			       :notify (lambda (wid &rest ignore)
					 ))
		:doc-type type))
  (widget-insert "\n")
  ;; ok and cancel buttons.
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   ;; Kill the "Create Project" form.
			   (kill-buffer))
		 "ok")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (kill-buffer))
		 "cancel")
  ;; Widget end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (goto-char 33)
  (use-local-map widget-keymap)
  (widget-setup)
  
  ;; ;; TODO: Support history.
  ;; ;; TODO: Support auto-complete.
  ;; (let ((str (read-from-minibuffer "[Prj] Search string: ")))
  ;;   (when (not (string-equal str ""))
  ;;     (message "[%s] Searching %s..." prj-project-name str)
  ;;     (let* ((search-db-file (find-file (prj-searchdb-path))))
  ;; 	;; Add title.
  ;; 	(end-of-buffer)
  ;; 	(princ (format "=== Search: %s ===\n" str) search-db-file)
  ;; 	;; Search.
  ;; 	(call-process-shell-command (format "xargs grep -nH \"%s\" < %s" str (prj-filedb-path)) nil search-db-file nil)
  ;; 	;; Cache search result.
  ;; 	(princ "\n" search-db-file)
  ;; 	(save-buffer))))
  )

;;;###autoload
(defun prj-toggle-search-buffer ()
  (interactive))

(provide 'prj)
