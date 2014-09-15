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

(defgroup prj-group nil
  "A Project management utility. This utility provides you a workspace and many projects concept. It also provide you a way to easily find file without knowing its full path; Add different directories with specific document types in a project; Powerful selective grep string or regular expression in a project, etc."
  :tag "Prj")

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

(defcustom prj-document-types '(("Text"         "*.txt;*.md;*.xml")
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
  :group 'prj-group)

(defcustom prj-exclude-types ".git;.svn;auto-save-list;*.cache;*.db;.save*;~#*;#*#"
  "Those kinds of file should be excluded in the project. Each matches should be delimit with ';'."
  ;; TODO: give GUI a pretty appearance.
  :type '(string :tag "File")
  :group 'prj-group)

(defconst prj-config-name "config.db"
  "The file name of project configuration.")

(defconst prj-filedb-name "files.db"
  "The file name of project file-list database.")

(defconst prj-searchdb-name "search.db"
  "The simple text file which caches the search result that users have done in the last session.")

(defconst prj-search-history-max 16
  "Maximin elements count in the searh history cache.")

(defvar prj-config nil
  "A plist which represent a project's configuration, it will be exported as format of JSON file.
list format:
  (:name NAME                                // NAME is a string.
   :filepaths (PATH1 PATH2 ...)              // PATH is a string.
   :doctypes (DOC_NAME1 DOC_TYPE1            // e.g. (\"Emacs Lisp\" \".emacs;*.el\"
              DOC_NAME2 DOC_TYPE2                     \"Text\" \"*.txt;*.md\"
              ...)                                    ...).
   :recent-files (FILE1 FILE2 ...)           // FILE is a string.
   :search-history (KEYWORD1 KEYWORD2 ...))  // KEYWORD is a string.")

(defmacro prj-plist-put (plist prop val)
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defun prj-config-path ()
  (expand-file-name (format "%s/%s/%s" prj-workspace-path (prj-project-name) prj-config-name)))

(defun prj-filedb-path ()
  (expand-file-name (format "%s/%s/%s" prj-workspace-path (prj-project-name) prj-filedb-name)))

(defun prj-searchdb-path ()
  (expand-file-name (format "%s/%s/%s" prj-workspace-path (prj-project-name) prj-searchdb-name)))

(defun prj-project-name ()
  (plist-get prj-config :name))

(defun prj-project-doctypes ()
  (plist-get prj-config :doctypes))

(defun prj-project-filepaths ()
  (plist-get prj-config :filepaths))

(defun prj-project-recent-files ()
  (plist-get prj-config :recent-files))

(defun prj-project-search-history ()
  (plist-get prj-config :search-history))

(defun prj-project-p ()
  "Return t if any project was loaded (current project)."
  (and prj-config
       (plist-get :name prj-config)))

(defun prj-new-config ()
  "Return a config template."
  (let (config)
    (prj-plist-put config :name "")
    (prj-plist-put config :filepaths '())
    (prj-plist-put config :doctypes '())
    (prj-plist-put config :recent-files '())
    (prj-plist-put config :search-history '())
    config))

(defun prj-export-json (filename data)
  "Export `data' to `filename' file. The saved data can be imported with `prj-import-data'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (json-encode-plist data)))))
;; (prj-export-json "~/test.txt" '(:name "Emacs" :doctypes ("Text" "*.txt;*.md" "Lisp" "*.el" "Python" "*.py") :filepath ("~/.emacs" "~/.emacs.d/elpa" "~/.emacs.d/etc")))

(defun prj-import-json (filename)
  "Read data exported by `prj-export-json' from file `filename'."
  (when (file-exists-p filename)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'list))
      (json-read-file filename))))
;; (prj-import-json "~/test.txt")

(defun prj-export-data (filename data)
  "Export `data' to `filename' file. The saved data can be imported with `prj-import-data'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length)
		(prin1-to-string data))))))

(defun prj-import-data (filename)
  "Read data exported by `prj-export-json' from file `filename'."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (read (buffer-string)))))

(defun prj-wildcardexp-to-regexp (wildcardexp)
  "Translate wildcard expression to Emacs regular expression."
  (let (regexp)
    (dolist (el (split-string wildcardexp ";"))
      (cond
       ;; ex: *.el
       ((string-match "^\\*\\..+" el)
        (setq el (replace-regexp-in-string "^\\*\\." ".*\\\\." el)
              el (concat el "$")))
       ;; ex: cache-*
       ((string-match ".+\\*$" el)
        (setq el (concat "^" el)
              el (replace-regexp-in-string "\\*" ".*" el)))
       ;; ex: ABC*DEF
       ((string-match ".+\\*.+" el)
        (setq el (concat "^" el)
              el (replace-regexp-in-string "\\*" ".*" el)
              el (concat el "$")))
       ;; ex: .git or .svn
       ((string-match "\\." el)
        (setq el (replace-regexp-in-string "\\." "\\\\." el))))
      (setq regexp (concat regexp el "\\|")))
    (setq regexp (replace-regexp-in-string "\\\\|$" "" regexp)
          regexp (concat "\\(" regexp "\\)"))))

(defun prj-concat-filepath (dir file)
  "Return a full path combined with `dir' and `file'. It saves you the worry of whether to append '/' or not."
  (concat dir
          (unless (eq (aref dir (1- (length dir))) ?/) "/")
          file))

(defun prj-directory-files (dir &optional exclude)
  "Return a list containing file names under `dir' but exlcudes files that match `exclude'."
  (let (files)
    (dolist (file (directory-files dir))
      (and (not (member file '("." "..")))
           (not (string-match exclude file))
           (setq files (cons file files))))
    (setq files (reverse files))))

(defun prj-build-filedb-internal (path matches exclude db)
  "Return a list that is made by recursively scan `dir' with file name which matches the regexp `matches'."
  (let* ((dir (file-name-directory path))
         (file (file-name-nondirectory path)))
    (if (file-directory-p path)
        ;; A directory.
        (dolist (file (prj-directory-files path exclude))
          (prj-build-filedb-internal (prj-concat-filepath path file) matches exclude db))
      ;; A file.
      (message "Building database and may take a moment.\nScan ...%s" path)
      (dolist (match matches)
        (let* ((doctype (car match))
               (match (cdr match))
               (files (gethash doctype db)))
          (and (string-match match file)
               (puthash doctype (push (prj-concat-filepath dir file) files) db)))))))

(defun prj-build-filedb ()
  "Create a list that contains all the files which should be included in the current project. Export the list to a file."
  (let ((db (make-hash-table :test 'equal))
	exclude
	match
	matches)
    ;; Prepare regexp.
    (setq exclude (prj-wildcardexp-to-regexp prj-exclude-types))
    ;; Initialize file database:
    ;; ex: (hashmap ... ((doctypes1) (files set 1...)
    ;;                   (doctypes2) (files set 2...)))
    (dolist (doctype (prj-project-doctypes))
      (setq match (prj-wildcardexp-to-regexp (cdr doctype)))
      ;; Create hash key with nil value.
      (puthash doctype nil db)
      ;; Create match sets (matches).
      (push (cons doctype match) matches))
    (setq matches (reverse matches))
    ;; Search directories and files.
    (dolist (f (prj-project-filepaths))
      (prj-build-filedb-internal f matches exclude db))
    ;; Export database.
    (puthash :version (current-time) db)
    (prj-export-json (prj-filedb-path) db)
    (message (format "[%s] Database is updated!" (prj-project-name)))))

(defun prj-build-tags ()
  ;; TODO: implemnt it.
  )

(defun prj-thingatpt ()
  "Return a list, (REGEXP_STRING BEG END), on which the point is or just string of selection."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((bound (bounds-of-thing-at-point 'symbol)))
      (and bound
           (buffer-substring-no-properties (car bound) (cdr bound))))))

(defun prj-clean ()
  "Clean search buffer or widget buffers which belongs to other project when user loads a project or unload a project."
  ;; Clean widgets.
  (prj-widget-clean)
  ;; Kill search buffer.
  (let ((search (get-buffer "*Search*")))
    (and search
         (with-current-buffer search
           (save-buffer)
           (kill-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro prj-with-search-buffer (&rest body)
  "Switch to search buffer and setup specific major mode and minor modes. Create a new one if it doesn't exist."
  (declare (indent 0) (debug t))
  `(progn
     (find-file (prj-searchdb-path))
     (rename-buffer "*Search*")
     (goto-char (point-max))
     (save-excursion
       (progn ,@body))
     (and (buffer-modified-p)
          (save-buffer 0))
     ;; TODO: goto last search result.
     ;; Change major mode.
     (prj-grep-mode)))

(defun prj-create-project-internal (name doctypes filepaths)
  "Internal function to create project. It might be called by widget or other GUI framework."
  (let* ((path (format "%s/%s/%s" prj-workspace-path name prj-config-name))
         (fullpath (expand-file-name path))
         (dir (file-name-directory fullpath))
         (config (prj-new-config)))
    ;; Prepare project directory.
    (unless (file-directory-p dir)
      (make-directory dir))
    ;; Export configuration.
    (puthash :name name config)
    (puthash :doctypes doctypes config)
    (puthash :filepaths filepaths config)
    (prj-export-json path config)
    ;; Load project.
    (prj-load-project)
    ;; Build database if the prject which was just created is present.
    (and (equal (prj-project-name) prj-tmp-string)
	 (prj-build-database))))

(defun prj-edit-project-internal (doctypes filepaths updatedb)
  "Internal function to edit project. It might be called by widget or other GUI framework."
  (puthash :version (current-time) prj-config)
  (puthash :doctypes doctypes prj-config)
  (puthash :filepaths filepaths prj-config)
  (prj-export-json (prj-config-path) prj-config)
  ;; Update file database.
  (and updatedb
       (prj-build-filedb)))

(defun prj-delete-project-internal (projects)
  "Internal function to delete project. It might be called by widget or other GUI framework."
  (dolist (c projects)
    ;; Unload current project if it is selected.
    (when (and (prj-project-p)
	       (equal c (prj-project-name)))
      (prj-unload-project))
    ;; Delete directory
    (delete-directory (format "%s/%s" prj-workspace-path c) t t))
  (message "[Prj] Delet project ...done"))

(defun prj-search-project-internal (match projects)
  "Internal function to search project. It might be called by widget or other GUI framework."
  ;; Cache search string.
  (let ((cache (prj-project-search-history)))
    (push match cache)
    (and (> (length cache) prj-search-history-max)
         (setcdr (nthcdr (1- prj-search-history-max) cache) nil))
    (puthash :search-cache cache prj-config)
    (prj-export-json (prj-config-path) prj-config))
  ;; Create search buffer.
  (prj-with-search-buffer
    (let ((db (prj-import-json (prj-filedb-path)))
          (files '()))
      (insert (format ">>>>> %s\n" match))
      ;; Prepare file list.
      (dolist (elm projects)
        (dolist (f (gethash elm db))
          (message "[%s] Searching ...%s" (prj-project-name) f)
          (goto-char (point-max))
          (call-process "grep" nil (list (current-buffer) nil) t "-nH" match f)))
      (insert "<<<<<\n\n")
      (message (format "[%s] Search ...done" (prj-project-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-preference ()
  "Customize document types."
  (interactive)
  (customize-group 'prj-group))

;;;###autoload
(defun prj-create-project ()
  "Show configuration for creating new project."
  ;; TODO:
  ;; (let* ((json-object-type 'plist)
  ;;      (json-key-type 'string)
  ;;      (json-array-type 'list)
  ;;      (config (json-read-from-string "{\"name\":\"Emacs\", \"filepath\":[\"~/.emacs\", \"~/.emacs.d/elpa\", \"~/.emacs.d/etc\"], \"doctypes\":[[\"Text\", \"*.txt;*.md\"], [\"Lisp\", \"*.el\"], [\"Python\", \"*.py\"]]}")))
  ;; (format "%s" config))
  (interactive)
  (or (and (featurep 'prj-widget)
           (prj-setup-create-project-widget))))

;;;###autoload
(defun prj-delete-project ()
  "Show configuration for deleting projects."
  (interactive)
  (or (and (featurep 'prj-widget)
           (prj-setup-delete-project-widget))))

;;;###autoload
(defun prj-edit-project ()
  "Show configuration for editing project's setting."
  (interactive)
  ;; Load project if wasn't loaded.
  (unless (prj-project-p)
    (prj-load-project))
  (or (and (featurep 'prj-widget)
           (prj-setup-edit-project-widget))))

;;;###autoload
(defun prj-load-project ()
  "List available prjects in current workspace and let user to choose which 
project to be loaded."
  (interactive)
  (let (choices)
    ;; Find available directories which represent a project.
    (dolist (f (directory-files prj-workspace-path))
      (let ((config-file (format "%s/%s/%s" prj-workspace-path f prj-config-name)))
	(when (file-exists-p config-file)
	  (push f choices))))
    ;; Prompt user to create project if no projects is in workspace.
    (and (= 0 (length choices))
         (prj-create-project))
    ;; Prompt user to load project.
    (let ((c (ido-completing-read "[Prj] Load project: " choices)))
      (unless (member c choices)
	(error (format "[Prj] Can't load project invalid project, %s" c)))
      (prj-clean)
      ;; Read configuration.
      (setq prj-config (prj-import-json (format "%s/%s/%s" prj-workspace-path c prj-config-name)))
      (and (featurep 'sos)
           (symbolp 'sos-definition-window)
           (sos-definition-window-mode 1))
      (message "[%s] Load project ...done" (prj-project-name)))))

;;;###autoload
(defun prj-load-recent-project ()
  "Load the project which user exits emacs last time."
  (interactive)
  ;; TODO:
  nil)

;;;###autoload
(defun prj-unload-project ()
  "Unload current project."
  (interactive)
  (when (prj-project-p)
    (prj-clean)
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
  (let ((filedb (prj-import-json (prj-filedb-path)))
	(filelist '()))
    (dolist (elm (prj-project-doctypes))
      (setq filelist (append filelist (gethash elm filedb))))
    (find-file (ido-completing-read (format "[%s] Find file: " (prj-project-name)) filelist)))
  (garbage-collect))

;;;###autoload
(defun prj-search-project ()
  "Search string in the project. Append new search result to the old caches if `new' is nil."
  (interactive)
  ;; Load project if no project was loaded.
  (unless (prj-project-p)
    (prj-load-project))
  (or (and (featurep 'prj-widget)
           (prj-setup-search-project-widget (prj-thingatpt)))))

;;;###autoload
(defun prj-toggle-search-buffer ()
  (interactive)
  ;; TODO: bug when user is select definition window and try to toggle search buffer off.
  (if (equal (buffer-name (current-buffer)) "*Search*")
      ;; Back to previous buffer of current window.
      (progn
        (and (buffer-modified-p)
             (save-buffer 0))
        (kill-buffer))
    ;; Go to search buffer.
    (unless (prj-project-p)
      (prj-load-project))
    (prj-with-search-buffer)))

;;;###autoload
(define-minor-mode prj-project-mode
  "Provide convenient menu items and tool-bar items for project feature."
  :lighter " Project"
  :global t
  )

(provide 'prj)
