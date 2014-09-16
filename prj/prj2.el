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
;;           `prj2-create-project', `prj2-delete-project',
;;           `prj2-load-project', `prj2-unload-project',
;;           `prj2-build-database', `prj2-find-file'.
;; - Divide complex computation into piece, let user can interrupt it and save the result before the cancellation.
;; - Support project's local variable.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-08-01 (0.0.1)
;;    Initial release.

(require 'ido)
(require 'json)

(require 'prj-widget)
(require 'prj-grep)

(defgroup prj2-group nil
  "A Project management utility. This utility provides you a workspace and many projects concept. It also provide you a way to easily find file without knowing its full path; Add different directories with specific document types in a project; Powerful selective grep string or regular expression in a project, etc."
  :tag "Prj")

(defun prj2-cus-set-workspace (symbol value)
  "Make sure the directory is present."
  (when (stringp value)
    (unless (file-exists-p value)
      (make-directory value))
    (when (file-exists-p value)
      (set symbol (expand-file-name value)))))

(defcustom prj2-workspace-path "~/.emacs.d/.workspace"
  "The place storing all the projects' configurations."
  :type '(string)
  :set 'prj2-cus-set-workspace
  :group 'prj2-group)

(defcustom prj2-document-types '(("Text"         "*.txt;*.md;*.xml")
                                ("Emacs Lisp"   ".emacs;*.el")
                                ("Python"       "*.py")
                                ("Java"         "*.java")
                                ("C/C++ Header" "*.h;*.hxx;*.hpp")
                                ("C/C++ Source" "*.c;*.cpp")
                                ("Makfile"      "Makefile;makefile;Configure.ac;configure.ac;*.mk"))
  "Categorize file names refer to specific matches and give them type names. It is a list of (DOC_NAME . MATCHES). Each matches in MATCHES should be delimit with ';'."
  ;; TODO: give GUI a pretty appearance.
  :type '(repeat (list (string :tag "Type")
                       (string :tag "File")))
  :group 'prj2-group)

(defcustom prj2-exclude-types ".git;.svn"
  "Those kinds of file should be excluded in the project. Each matches should be delimit with ';'."
  ;; TODO: give GUI a pretty appearance.
  :type '(string :tag "File")
  :group 'prj2-group)

(defconst prj2-config-name "config2.db"
  "The file name of project configuration.")

(defconst prj2-filedb-name "files2.db"
  "The file name of project file-list database.")

(defconst prj2-searchdb-name "search2.db"
  "The simple text file which caches the search result that users have done in the last session.")

(defconst prj2-search-history-max 16
  "Maximin elements count in the searh history cache.")

(defvar prj2-config nil
  "A plist which represent a project's configuration, it will be exported as format of JSON file.
format:
  (:name NAME                                // NAME is a string.
   :filepaths (PATH1 PATH2 ...)              // PATH is a string.
   :doctypes (DOC_NAME1 DOC_TYPE1            // e.g. (\"Emacs Lisp\" \".emacs;*.el\"
              DOC_NAME2 DOC_TYPE2                     \"Text\"       \"*.txt;*.md\"
              ...)                                    ...).
   :recent-files (FILE1 FILE2 ...)           // FILE is a string.
   :search-history (KEYWORD1 KEYWORD2 ...))  // KEYWORD is a string.")

(defmacro prj2-plist-put (plist prop val)
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defun prj2-config-path ()
  (expand-file-name (format "%s/%s/%s"
                            prj2-workspace-path
                            (prj2-project-name)
                            prj2-config-name)))

(defun prj2-filedb-path ()
  "A plist which contains files should be concerned.
format:
  (\"doctype1\" (FILE1_1 FILE1_2 ...)
   \"doctype2\" (FILE2_1 FILE2_2 ...))"
  (expand-file-name (format "%s/%s/%s"
                            prj2-workspace-path
                            (prj2-project-name)
                            prj2-filedb-name)))

(defun prj2-searchdb-path ()
  (expand-file-name (format "%s/%s/%s"
                            prj2-workspace-path
                            (prj2-project-name)
                            prj2-searchdb-name)))

(defun prj2-project-name ()
  (plist-get prj2-config :name))

(defun prj2-project-doctypes ()
  (plist-get prj2-config :doctypes))

(defun prj2-project-filepaths ()
  (plist-get prj2-config :filepaths))

(defun prj2-project-recent-files ()
  (plist-get prj2-config :recent-files))

(defun prj2-project-search-history ()
  (plist-get prj2-config :search-history))

(defun prj2-project-p ()
  "Return t if any project was loaded (current project)."
  (and prj2-config
       (plist-get :name prj2-config)))

(defun prj2-new-config ()
  "Return a config template."
  (let (config)
    (prj2-plist-put config :name "")
    (prj2-plist-put config :filepaths '())
    (prj2-plist-put config :doctypes '())
    (prj2-plist-put config :recent-files '())
    (prj2-plist-put config :search-history '())
    config))

(defun prj2-export-json (filename data)
  "Export `data' to `filename' file. The saved data can be imported with `prj2-import-data'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (json-encode-plist data)))))
;; (prj2-export-json "~/.emacs.d/.workspace/Test/config2.db" '(:name "Test" :doctypes ("Emacs Lisp" ".emacs;*.el" "Text" "*.text;*.md;*.xml") :filepaths ("~/.emacs" "~/.emacs.d" "c:/emacs-24.3")))

(defun prj2-import-json (filename)
  "Read data exported by `prj2-export-json' from file `filename'."
  (when (file-exists-p filename)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'list))
      (json-read-file filename))))
;; (setq prj2-config (prj2-import-json "~/.emacs.d/.workspace/Test/config2.db"))

(defun prj2-export-data (filename data)
  "Export `data' to `filename' file. The saved data can be imported with `prj2-import-data'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length)
		(prin1-to-string data))))))

(defun prj2-import-data (filename)
  "Read data exported by `prj2-export-data' from file `filename'."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (read (buffer-string)))))

(defun prj2-concat-filepath (dir file)
  "Return a full path combined with `dir' and `file'. It saves you the worry of whether to append '/' or not."
  (concat dir
          (unless (eq (aref dir (1- (length dir))) ?/) "/")
          file))

(defun prj2-directory-files (dir &optional exclude)
  "Return a list containing file names under `dir' but exlcudes files that match `exclude'."
  (let (files)
    (dolist (file (directory-files dir))
      (and (not (member file '("." "..")))
           (not (string-match exclude file))
           (setq files (cons file files))))
    (setq files (reverse files))))

(defun prj2-convert-filepaths (filepaths)
  (unless (listp filepaths)
    (error))
  (let ((path filepaths)
        paths)
    (while path
      (setq paths (concat paths
                          "\"" (expand-file-name (car path)) "\"")
            path (cdr path))
      (and path
           (setq paths (concat paths " "))))
    paths))
;; (prj2-convert-filepaths (prj2-project-filepaths))

(defun prj2-convert-matches (doctype)
  (unless (stringp doctype)
    (error))
  (let ((matches (concat "\"-name\" \"" doctype "\"")))
    (replace-regexp-in-string ";" "\" \"-o\" \"-name\" \"" matches)))
;; (prj2-convert-matches ".emacs;*.el;*.txt;*.md")

(defun prj2-convert-excludes (doctype)
  (unless (stringp doctype)
    (error))
  (let ((matches (concat "\"!\" \"-name\" \"" doctype "\"")))
    (replace-regexp-in-string ";" "\" \"!\" \"-name\" \"" matches)))
;; (prj2-convert-excludes prj2-exclude-types)

(defun prj2-process-find (filepaths matches excludes)
  (ignore-errors
    (let ((filepaths (prj2-convert-filepaths filepaths))
          (matches (prj2-convert-matches matches))
          (excludes (prj2-convert-excludes excludes))
          stream)
      (setq stream (concat "(with-temp-buffer "
                           "(call-process \"find\" nil (list (current-buffer) nil) nil "
                           filepaths " "
                           matches " "
                           excludes ")"
                           "(buffer-string))"))
      ;; (eval (read stream))
      (prin1 (read stream))
      )))
;; (prj2-process-find (prj2-project-filepaths) "*.el;.emacs;*.txt" "*.git;*.svn")
;; (with-temp-buffer (call-process "find" nil (list nil nil) t "c:/emacs.home/.emacs" "c:/emacs.home/.emacs.d" "c:/emacs-24.3" "-name" "*.el" "-o" "-name" ".emacs" "-o" "-name" "*.txt" "!" "-name" "*.git" "!" "-name" "*.svn") (buffer-string))
;; (with-temp-buffer (call-process "find" nil (list (current-buffer) nil) t "c:/emacs.home/.emacs.d" "-name" "*.el") (buffer-string))

(defun prj2-build-filedb ()
  "Create a list that contains all the files which should be included in the current project. Export the list to a file."
  (let ((filepaths (prj2-project-filepaths))
        (excludes prj2-exclude-types)
        db)
    ;; Iterate doctypes.
    (while doctypes
      (let* ((files (prj2-process-find filepaths
                                       (cadr doctypes)
                                       excludes)))
        (prj2-plist-put db (car doctypes)
                        (split-string files "\n" t)))
      ;; Next.
      (setq doctypes (cddr doctypes)))
    ;; Export database.
    ;; (prj2-export-data (prj2-filedb-path) db)
    ))
;; (prj2-build-filedb)

(defun prj2-build-tags ()
  ;; TODO: implemnt it.
  )

(defun prj2-thingatpt ()
  "Return a list, (REGEXP_STRING BEG END), on which the point is or just string of selection."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((bound (bounds-of-thing-at-point 'symbol)))
      (and bound
           (buffer-substring-no-properties (car bound) (cdr bound))))))

(defun prj2-clean ()
  "Clean search buffer or widget buffers which belongs to other project when user loads a project or unload a project."
  ;; Clean widgets.
  (prj2-widget-clean)
  ;; Kill search buffer.
  (let ((search (get-buffer "*Search*")))
    (and search
         (with-current-buffer search
           (save-buffer)
           (kill-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro prj2-with-search-buffer (&rest body)
  "Switch to search buffer and setup specific major mode and minor modes. Create a new one if it doesn't exist."
  (declare (indent 0) (debug t))
  `(progn
     (find-file (prj2-searchdb-path))
     (rename-buffer "*Search*")
     (goto-char (point-max))
     (save-excursion
       (progn ,@body))
     (and (buffer-modified-p)
          (save-buffer 0))
     ;; TODO: goto last search result.
     ;; Change major mode.
     (prj2-grep-mode)))

(defun prj2-create-project-internal (name doctypes filepaths)
  "Internal function to create project. It might be called by widget or other GUI framework."
  (let* ((path (format "%s/%s/%s" prj2-workspace-path name prj2-config-name))
         (fullpath (expand-file-name path))
         (dir (file-name-directory fullpath))
         (config (prj2-new-config)))
    ;; Prepare project directory.
    (unless (file-directory-p dir)
      (make-directory dir))
    ;; Export configuration.
    (prj2-plist-put config :name name)
    (prj2-plist-put config :doctypes doctypes)
    (prj2-plist-put config :filepaths filepaths)
    (prj2-export-json path config)
    ;; Load project.
    (prj2-load-project)
    ;; Build database if the prject which was just created is present.
    (and (equal (prj2-project-name) prj2-tmp-string)
	 (prj2-build-database))))

(defun prj2-edit-project-internal (doctypes filepaths updatedb)
  "Internal function to edit project. It might be called by widget or other GUI framework."
  (puthash :version (current-time) prj2-config)
  (puthash :doctypes doctypes prj2-config)
  (puthash :filepaths filepaths prj2-config)
  (prj2-export-json (prj2-config-path) prj2-config)
  ;; Update file database.
  (and updatedb
       (prj2-build-filedb)))

(defun prj2-delete-project-internal (projects)
  "Internal function to delete project. It might be called by widget or other GUI framework."
  (dolist (c projects)
    ;; Unload current project if it is selected.
    (when (and (prj2-project-p)
	       (equal c (prj2-project-name)))
      (prj2-unload-project))
    ;; Delete directory
    (delete-directory (format "%s/%s" prj2-workspace-path c) t t))
  (message "Delet project ...done"))

(defun prj2-search-project-internal (match projects)
  "Internal function to search project. It might be called by widget or other GUI framework."
  ;; Cache search string.
  (let ((cache (prj2-project-search-history)))
    (push match cache)
    (and (> (length cache) prj2-search-history-max)
         (setcdr (nthcdr (1- prj2-search-history-max) cache) nil))
    (puthash :search-cache cache prj2-config)
    (prj2-export-json (prj2-config-path) prj2-config))
  ;; Create search buffer.
  (prj2-with-search-buffer
    (let ((db (prj2-import-data (prj2-filedb-path)))
          (files '()))
      (insert (format ">>>>> %s\n" match))
      ;; Prepare file list.
      (dolist (elm projects)
        (dolist (f (gethash elm db))
          (message "[%s] Searching ...%s" (prj2-project-name) f)
          (goto-char (point-max))
          (call-process "grep" nil (list (current-buffer) nil) t "-nH" match f)))
      (insert "<<<<<\n\n")
      (message (format "[%s] Search ...done" (prj2-project-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj2-preference ()
  "Customize document types."
  (interactive)
  (customize-group 'prj2-group))

;;;###autoload
(defun prj2-create-project ()
  "Show configuration for creating new project."
  ;; TODO:
  ;; (let* ((json-object-type 'plist)
  ;;      (json-key-type 'string)
  ;;      (json-array-type 'list)
  ;;      (config (json-read-from-string "{\"name\":\"Emacs\", \"filepath\":[\"~/.emacs\", \"~/.emacs.d/elpa\", \"~/.emacs.d/etc\"], \"doctypes\":[[\"Text\", \"*.txt;*.md\"], [\"Lisp\", \"*.el\"], [\"Python\", \"*.py\"]]}")))
  ;; (format "%s" config))
  (interactive)
  (or (and (featurep 'prj2-widget)
           (prj2-setup-create-project-widget))))

;;;###autoload
(defun prj2-delete-project ()
  "Show configuration for deleting projects."
  (interactive)
  (or (and (featurep 'prj2-widget)
           (prj2-setup-delete-project-widget))))

;;;###autoload
(defun prj2-edit-project ()
  "Show configuration for editing project's setting."
  (interactive)
  ;; Load project if wasn't loaded.
  (unless (prj2-project-p)
    (prj2-load-project))
  (or (and (featurep 'prj2-widget)
           (prj2-setup-edit-project-widget))))

;;;###autoload
(defun prj2-load-project ()
  "List available prjects in current workspace and let user to choose which 
project to be loaded."
  (interactive)
  (let (choices)
    ;; Find available directories which represent a project.
    (dolist (file (directory-files prj2-workspace-path))
      (let ((config-file (format "%s/%s/%s"
                                 prj2-workspace-path
                                 file
                                 prj2-config-name)))
	(when (file-exists-p config-file)
	  (push file choices))))
    ;; Prompt user to create project if no projects is in workspace.
    (and (= 0 (length choices))
         (prj2-create-project))
    ;; Prompt user to load project.
    (let ((name (ido-completing-read "Load project: " choices)))
      (unless (member c choices)
	(error (format "Can't load project invalid project, %s" name)))
      (prj2-clean)
      ;; Read configuration.
      (setq prj2-config (prj2-import-json (format "%s/%s/%s"
                                                prj2-workspace-path
                                                name
                                                prj2-config-name)))
      (and (featurep 'sos)
           (sos-definition-window-mode 1))
      (message "Load [%s] ...done" (prj2-project-name)))))

;;;###autoload
(defun prj2-load-recent-project ()
  "Load the project which user exits emacs last time."
  (interactive)
  ;; TODO:
  nil)

;;;###autoload
(defun prj2-unload-project ()
  "Unload current project."
  (interactive)
  (when (prj2-project-p)
    (prj2-clean)
    (message "[%s] Unload project ...done" (prj2-project-name))
    (setq prj2-config (prj2-new-config))))

;;;###autoload
(defun prj2-build-database ()
  "Build file list and tags."
  (interactive)
  (unless (prj2-project-p)
    (prj2-load-project))
  ;; Create file list which is the data base of the project's files.
  (when (prj2-project-p)
    (message "It might take a minutes, please wait ...")
    (prj2-build-filedb)
    (prj2-build-tags)
    (message "Database is updated!")))

;;;###autoload
(defun prj2-find-file ()
  "Open file by the given file name."
  (interactive)
  ;; Load project if wasn't loaded.
  (unless (prj2-project-p)
    (prj2-load-project))
  ;; Build database if is wasn't built.
  (unless (file-exists-p (prj2-filedb-path))
    (prj2-build-database))
  ;; Find file.
  (let ((filedb (prj2-import-data (prj2-filedb-path)))
	(filelist '()))
    (dolist (elm (prj2-project-doctypes))
      (setq filelist (append filelist (gethash elm filedb))))
    (find-file (ido-completing-read "Find file: " filelist))))

;;;###autoload
(defun prj2-search-project ()
  "Search string in the project. Append new search result to the old caches if `new' is nil."
  (interactive)
  ;; Load project if no project was loaded.
  (unless (prj2-project-p)
    (prj2-load-project))
  (or (and (featurep 'prj2-widget)
           (prj2-setup-search-project-widget (prj2-thingatpt)))))

;;;###autoload
(defun prj2-toggle-search-buffer ()
  (interactive)
  ;; TODO: bug when user is select definition window and try to toggle search buffer off.
  (if (equal (buffer-name (current-buffer)) "*Search*")
      ;; Back to previous buffer of current window.
      (progn
        (and (buffer-modified-p)
             (save-buffer 0))
        (kill-buffer))
    ;; Go to search buffer.
    (unless (prj2-project-p)
      (prj2-load-project))
    (prj2-with-search-buffer)))

;;;###autoload
(define-minor-mode prj2-project-mode
  "Provide convenient menu items and tool-bar items for project feature."
  :lighter " Project"
  :global t
  )

(provide 'prj2)
