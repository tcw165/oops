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
;; - Divide complex computation into piece, let user can interrupt it and save
;;   the result before the cancellation.
;; - Support project's local variable.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-08-01 (0.0.1)
;;    Initial release.

(require 'cl)
(require 'ido)
(require 'json)

(require 'prj-grep)
(require 'prj-widget-frontend)

(defgroup prj-group nil
  "A Project management utility. This utility provides you a workspace and many 
projects concept. It also provide you a way to easily find file without knowing 
its full path; Add different directories with specific document types in a 
project; Powerful selective grep string or regular expression in a project, etc."
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

(defcustom prj-document-types '(("Text" . "*.txt;*.md;*.xml")
                                ("Emacs Lisp" . ".emacs;*.el")
                                ("Python" . "*.py")
                                ("Java" . "*.java")
                                ("C/C++ Header" . "*.h;*.hxx;*.hpp")
                                ("C/C++ Source" . "*.c;*.cpp")
                                ("Makfile" . "Makefile;makefile;Configure.ac;configure.ac;*.mk"))
  "Categorize file names refer to specific matches and give them type names. 
It is a alist of (DOC_NAME MATCHES). Each matches in MATCHES should be delimit 
with ';'."
  ;; TODO: give GUI a pretty appearance.
  :type '(repeat (cons (string :tag "Type")
                       (string :tag "File")))
  :group 'prj-group)

(defcustom prj-exclude-types ".git;.svn"
  "Those kinds of file should be excluded in the project. Each matches should 
be delimit with ';'."
  ;; TODO: give GUI a pretty appearance.
  :type '(string :tag "File")
  :group 'prj-group)

(defcustom prj-create-project-frontend 'prj-create-project-widget-frontend
  "The front-ends should return a struct:
  (:name NAME
   :doctypes DOCTYPES
   :filepaths FILEPATHS)"
  :type '(symbol :tag "Front-end")
  :group 'prj-group)

(defcustom prj-delete-project-frontend 'prj-delete-project-widget-frontend
  "The front-ends should return "
  :type '(symbol :tag "Front-end")
  :group 'prj-group)

(defcustom prj-load-project-frontend 'prj-load-project-widget-frontend
  ""
  :type '(symbol :tag "Front-end")
  :group 'prj-group)

(defcustom prj-edit-project-frontend 'prj-edit-project-widget-frontend
  ""
  :type '(symbol :tag "Front-end")
  :group 'prj-group)

(defcustom prj-search-project-frontend 'prj-search-project-widget-frontend
  ""
  :type '(symbol :tag "Front-end")
  :group 'prj-group)

(defcustom prj-find-file-frontend 'prj-find-file-basic-frontend
  ""
  :type '(symbol :tag "Front-end")
  :group 'prj-group)

;; (defcustom prj-find-file-backend)

(defvar prj-config nil
  "A plist which represent a project's configuration, it will be exported as 
format of JSON file.
format:
  (:name NAME                                      // NAME is a string.
   :filepaths (PATH1 PATH2 ...)                    // PATH is a string.
   :doctypes (DOC_NAME1 DOC_TYPE1                  // e.g. (\"Emacs Lisp\" \".emacs;*.el\"
              DOC_NAME2 DOC_TYPE2                           \"Text\"       \"*.txt;*.md\"
              ...)                                          ...).
   :recent-files (FILE1 FILE2 ...)                 // FILE is a string.
   :search-history (POINT KEYWORD1 KEYWORD2 ...))  // KEYWORD is a string.")

(defconst prj-config-name "config.db"
  "The file name of project configuration. see `prj-config' for detail.")

(defconst prj-filedb-name "files.db"
  "The file name of project file-list database. The database is a plist which 
contains files should be concerned.
format:
  (DOCTYPE1 (FILE1_1 FILE1_2 ...)
   DOCTYPE2 (FILE2_1 FILE2_2 ...))")

(defconst prj-searchdb-name "search.db"
  "The simple text file which caches the search result that users have done 
in the last session.")

(defconst prj-search-history-max 16
  "Maximin elements count in the searh history cache.")

(defconst prj-idle-delay 0.15)

(defvar prj-timer nil)

(defvar prj-process-grep nil)

(defmacro prj-plist-put (plist prop val)
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defmacro prj-ok-delay-begin (ok-impl)
  "Create a lambda function that call OK-IMPL function with  one parameters 
after `prj-idle-delay' seconds."
  `(lambda (data)
     (when prj-timer
       (cancel-timer prj-timer)
       (setq prj-timer nil))
     (setq prj-timer (run-with-timer prj-idle-delay nil
                                     ,ok-impl
                                     data))))

(defmacro prj-with-search-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(let ((buffer (find-file-noselect (prj-searchdb-path))))
     (with-current-buffer buffer
       (rename-buffer "*Search*")
       (prj-grep-mode)
       ,@body)))

(defun prj-call-frontend (command frontend &optional ok)
  "Call frontends and pass ok callback functions to them. If one of them 
returns non nil, the loop will break."
  (funcall frontend command ok))

(defun prj-clean-frontends ()
  (dolist (frontend `(,prj-create-project-frontend
                      ,prj-delete-project-frontend
                      ,prj-edit-project-frontend
                      ,prj-search-project-frontend
                      ,prj-find-file-frontend))
    (prj-call-frontend :hide frontend)))

(defun prj-clean-all ()
  "Clean search buffer or widget buffers which belongs to other project when 
user loads a project or unload a project."
  ;; Clean frontends.
  (prj-clean-frontends)
  ;; Kill search buffer.
  (let ((search (get-buffer "*Search*")))
    (and search
         (with-current-buffer search
           (save-buffer)
           (kill-buffer))))
  ;; Reset configuration.
  (setq prj-config nil))

(defun prj-create-project-internal (data)
  "Internal function to create project. It is called by functions in the 
`prj-create-project-frontend'."
  (let* ((name (plist-get data :name))
         (doctypes (plist-get data :doctypes))
         (filepaths (plist-get data :filepaths))
         (path (prj-config-path name))
         (fullpath (expand-file-name path))
         (dir (file-name-directory fullpath))
         (config (prj-new-config)))
    ;; Prepare project directory.
    (unless (file-directory-p dir)
      (make-directory dir))
    ;; Export configuration.
    (prj-plist-put config :name name)
    (prj-plist-put config :doctypes doctypes)
    (prj-plist-put config :filepaths filepaths)
    (prj-export-json path config)
    ;; Load project.
    (prj-load-project name)
    ;; Build database.
    (prj-build-database t)))

(defun prj-delete-project-internal (data)
  "Internal function to delete project. It is called by functions in the 
`prj-delete-project-frontend'."
  (let ((projects data))
    (dolist (project projects)
      ;; Unload current project if it is selected.
      (when (and (prj-project-p)
                 (string= project (prj-project-name)))
        (prj-unload-project))
      ;; Delete directory
      (delete-directory (format "%s/%s" prj-workspace-path project) t t))
    (message "Delet project ...done")))

(defun prj-load-project-internal (data)
  "Internal function to edit project. It is called by functions in the 
`prj-load-project-frontend'."
  (let ((name data))
    ;; Save files in the last session.
    (prj-save-file-names t)
    (prj-clean-all)
    ;; Read configuration.
    (setq prj-config (prj-import-json (prj-config-path name)))
    ;; Open files.
    (let ((files (prj-project-recent-files)))
      (when files
        (dolist (file (cdr files))
          (find-file-existing file))
        (find-file-existing (car files))))
    ;; Update database
    (prj-build-database)
    (message "Load [%s] ...done" (prj-project-name))))

(defun prj-edit-project-internal (data)
  "Internal function to edit project. It is called by functions in the 
`prj-edit-project-frontend'."
  (let ((doctypes (plist-get data :doctypes))
        (filepaths (plist-get data :filepaths))
        update-db)
    ;; Compare new and old doctypes.
    ;; Compare new and old filepaths
    (let ((item1 (prj-project-doctypes))
          (item2 (prj-project-filepaths)))
      (when (or (/= (length doctypes) (length item1))
                (not (every 'equal doctypes item1))
                (/= (length filepaths) (length item2))
                (not (every 'equal filepaths item2)))
        (setq update-db t)))
    (prj-plist-put prj-config :doctypes doctypes)
    (prj-plist-put prj-config :filepaths filepaths)
    (prj-export-json (prj-config-path) prj-config)
    ;; Update database.
    (prj-build-database update-db)))

(defun prj-search-project-internal-1 (data)
  "Internal function to edit project. It is called by functions in the 
`prj-search-project-frontend'."
  (let ((match (plist-get data :match))
        (doctypes (plist-get data :doctypes))
        (filepaths (plist-get data :filepaths))
        (casefold (plist-get data :casefold))
        (word-only (plist-get data :word-only))
        (skip-comment (plist-get data :skip-comment)))
    ;; Update search history.
    (let ((last-pos (car (prj-project-search-history)))
          (history (cdr (prj-project-search-history))))
      (if (member match history)
          ;; Reorder the match.
          (setq history (delete match history)
                history (push match history))
        (setq history (push match history)))
      ;; Keep maximum history.
      (and (> (length history) prj-search-history-max)
           (setcdr (nthcdr (1- prj-search-history-max) history) nil))
      (prj-plist-put prj-config :search-history (append `(,last-pos) history))
      (prj-export-json (prj-config-path) prj-config))
    ;; Start to search.
    (let ((db (prj-import-data (prj-filedb-path)))
          files)
      ;; TODO: support doctypes, filepaths, casefold, word-only, skip-comment.
      ;; Collect files.
      (while db
        (setq files (append files (cadr db))
              db (cddr db)))
      ;; Search.
      (prj-with-search-buffer
        (switch-to-buffer (current-buffer) nil t)
        (goto-char (point-max))
        (insert (format ">>>>> %s\n" match))
        (prj-process-grep match files
                          'prj-search-project-internal-2)))))

(defun prj-search-project-internal-2 (process message)
  "A sentinel for asynchronous process GREP."
  (when (memq (process-status process) '(stop exit signal))
    (prj-with-search-buffer
      (goto-char (point-max))
      (insert "<<<<<\n\n")
      (save-buffer 0)
      (setq buffer-read-only t)
      (message "Search project...done"))))

(defun prj-process-grep (match filepaths sentinel)
  "Use `start-process' to call GREP. MATCH is a string to search. FILEPATHS is 
a file list, (FILE1 FILE2 ...). SENTINEL is the GREP's sentinel."
  (let ((stream (with-output-to-string
                  (princ "(start-process \"grep\" (current-buffer) \"grep\" ")
                  (prin1 "-snH")(princ " ")
                  (prin1 match)(princ " ")
                  (dolist (filepath filepaths)
                    (prin1 filepath)(princ " "))
                  (princ ")"))))
    (setq prj-process-grep (eval (read stream)))
    (set-process-sentinel prj-process-grep sentinel)))

(defun prj-find-file-internal (file)
  (and (featurep 'history)
       (his-add-position-type-history))
  (find-file file)
  (and (featurep 'history)
       (his-add-position-type-history)))

(defun prj-build-filedb (&optional all)
  "Create a list that contains all the files which should be included in the 
current project. Export the list to a file."
  ;; TODO: Find those files which are newer than database, update them.
  (let ((filepaths (prj-project-filepaths))
        (doctypes (prj-project-doctypes))
        (excludes prj-exclude-types)
        db)
    (if (or (null (file-exists-p (prj-filedb-path)))
            all)
        (progn
          ;; Iterate doctypes.
          (while doctypes
            (prj-plist-put db
                           (car doctypes)
                           (prj-process-find filepaths
                                             (cadr doctypes)
                                             excludes))
            ;; Next.
            (setq doctypes (cddr doctypes)))
          ;; Export database.
          (prj-export-data (prj-filedb-path) db))
      ;; TODO: partial update
      )))

(defun prj-build-tags (&optional all)
  ;; TODO: implemnt it.
  ;; TODO: Find those files which are newer than database, update them.
  )

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
                   (princ excludes)(princ ")"))))
    (let ((output (with-temp-buffer
                    (eval (read stream))
                    (buffer-string))))
      (and output (split-string output "\n" t)))))

(defun prj-process-find-change ()
  ;; TODO:
  t)

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

(defun prj-project-p ()
  "Return t if any project was loaded (current project)."
  (and prj-config
       (plist-get prj-config :name)))

(defun prj-config-path (&optional name)
  (expand-file-name (format "%s/%s/%s"
                            prj-workspace-path
                            (or name
                                (prj-project-name))
                            prj-config-name)))

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

(defun prj-export-json (filename data)
  "Export `data' to `filename' file. The saved data can be imported with `prj-import-data'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (json-encode-plist data)))))

(defun prj-import-json (filename)
  "Read data exported by `prj-export-json' from file `filename'."
  (when (file-exists-p filename)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'list))
      (json-read-file filename))))

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

(defun prj-new-config ()
  "Return a config template. Check `prj-config' for detail."
  (let (config)
    (prj-plist-put config :name "")
    (prj-plist-put config :filepaths '())
    (prj-plist-put config :doctypes '())
    (prj-plist-put config :recent-files '())
    (prj-plist-put config :search-history '(0))
    config))

;;;###autoload
(defun prj-preference ()
  "Customize document types."
  (interactive)
  (prj-clean-frontends)
  (customize-group 'prj-group))

;;;###autoload
(defun prj-create-project ()
  "Show configuration for creating new project."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-clean-frontends)
  (prj-call-frontend :show
                     prj-create-project-frontend
                     (prj-ok-delay-begin
                      'prj-create-project-internal)))

;;;###autoload
(defun prj-delete-project ()
  "Show configuration for deleting projects."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-clean-frontends)
  (prj-call-frontend :show
                     prj-delete-project-frontend
                     (prj-ok-delay-begin
                      'prj-delete-project-internal)))

;;;###autoload
(defun prj-edit-project ()
  "Show configuration for editing project's setting."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-clean-frontends)
  ;; Load project if wasn't loaded.
  (unless (prj-project-p)
    (prj-load-project))
  (prj-call-frontend :show
                     prj-edit-project-frontend
                     (prj-ok-delay-begin
                      'prj-edit-project-internal)))

;;;###autoload
(defun prj-load-project (&optional name)
  "List available prjects in current workspace and let user to choose which 
project to be loaded."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-clean-frontends)
  (if name
      (prj-load-project-internal name)
    (prj-call-frontend :show
                       prj-load-project-frontend
                       (prj-ok-delay-begin
                        'prj-load-project-internal))))

;;;###autoload
(defun prj-load-recent-project ()
  "Load the project which user exits emacs last time."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-clean-frontends)
  ;; TODO:
  nil)

;;;###autoload
(defun prj-unload-project ()
  "Unload current project."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  ;; Save files in the last session.
  (prj-save-file-names t)
  (prj-clean-frontends)
  (let ((name (prj-project-name)))
    (prj-clean-all)
    (message "Unload [%s] ...done" name)))

;;;###autoload
(defun prj-build-database (&optional all)
  "Build file list and tags."
  (interactive '(t))
  (prj-clean-frontends)
  (unless (prj-project-p)
    (prj-load-project))
  ;; Create file list which is the data base of the project's files.
  (when (prj-project-p)
    (message "Build database might take a minutes, please wait ...")
    (prj-build-filedb all)
    (prj-build-tags all)
    (message "Database is updated!")))

;;;###autoload
(defun prj-find-file ()
  "Open file by the given file name."
  (interactive)
  (prj-clean-frontends)
  ;; Load project if wasn't loaded.
  (unless (prj-project-p)
    (prj-load-project))
  (prj-call-frontend :show
                       prj-find-file-frontend
                       (prj-ok-delay-begin
                        'prj-find-file-internal)))

;;;###autoload
(defun prj-search-project ()
  "Search string in the project. Append new search result to the old caches."
  (interactive)
  (prj-clean-frontends)
  ;; Load project if no project was loaded.
  (unless (prj-project-p)
    (prj-load-project))
  (if (and (processp prj-process-grep)
           (process-live-p prj-process-grep))
      (message "Searching is under processing, please wait...")
    (prj-call-frontend :show
                        prj-search-project-frontend
                        (prj-ok-delay-begin
                         'prj-search-project-internal-1))))

;;;###autoload
(defun prj-toggle-search-buffer ()
  (interactive)
  ;; TODO: bug when user is select definition window and try to toggle search buffer off.
  ;; TODO: use front-end???
  (if (equal (buffer-name (current-buffer)) "*Search*")
      ;; Back to previous buffer of current window.
      (progn
        (and (buffer-modified-p)
             (save-buffer 0))
        (kill-buffer))
    ;; Go to search buffer.
    (unless (prj-project-p)
      (prj-load-project))
    (and (featurep 'history)
         (his-add-position-type-history))
    (prj-with-search-buffer
      (switch-to-buffer (current-buffer) nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-kill-emacs-hook ()
  (prj-save-file-names))

(defun prj-save-file-names (&optional close)
  (when (prj-project-p)
    ;; Save file names opened in current session.
    (let (file files buffers-to-kill)
      (dolist (buffer (buffer-list))
        (when (and (setq file (buffer-file-name buffer))
                   ;; Skip search database.
                   (not (string= file (prj-searchdb-path))))
          (setq files (append files `(,file))))
        (when (and close file)
          (setq buffers-to-kill (append buffers-to-kill `(,buffer)))))
      ;; Close buffers.
      (dolist (buffer buffers-to-kill)
        (kill-buffer buffer))
      ;; Export configuration.
      (prj-plist-put prj-config :recent-files files)
      (prj-export-json (prj-config-path) prj-config))))

;;;###autoload
(define-minor-mode prj-project-mode
  "Provide convenient menu items and tool-bar items for project feature."
  :lighter " Project"
  :global t
  (if prj-project-mode
      (progn
        (add-hook 'kill-emacs-hook 'prj-kill-emacs-hook t nil))
    (remove-hook 'kill-emacs-hook 'prj-kill-emacs-hook nil)))

(provide 'prj)
