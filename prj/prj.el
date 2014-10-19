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

(require 'prj-widget-frontend)
(require 'prj-default-backend)

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

;; TODO: use mode and extensions `auto-mode-alist'.
(defcustom prj-document-types '(("Text" . "*.txt;*.md;*.xml")
                                ("Emacs Lisp" . ".emacs;*.el")
                                ("Python" . "*.py")
                                ("C/C++ Header" . "*.h;*.hxx;*.hpp")
                                ("C/C++ Source" . "*.c;*.cxx;*.cpp"))
  "Categorize file names refer to specific matches and give them type names. 
It is a alist of (DOC_NAME MATCHES). Each matches in MATCHES should be delimit 
with ';'."
  :type '(repeat (cons (string :tag "Type")
                       (string :tag "File")))
  :group 'prj-group)

(defcustom prj-exclude-types ".git;.svn"
  "Those kinds of file should be excluded in the project. Each matches should 
be delimit with ';'."
  ;; TODO: give GUI a pretty appearance.
  :type '(string :tag "File")
  :group 'prj-group)

(defcustom prj-frontends '(prj-create-project-widget-frontend
                           prj-delete-project-widget-frontend
                           prj-edit-project-widget-frontend
                           prj-search-project-widget-frontend
                           prj-load-project-widget-frontend
                           prj-find-file-frontend)
  "The list of front-ends for the purpose of visualization. Every front-end takes
at least one argument, command. Optional arguement callback is the function which
 the front-end will call when command task is ready to be exectued.

### Commands:
`:init'             - Initialize frontends.
`:destroy'          - Ask frontends to destroy their GUIs.
`:create-project'   - Show GUI for creating project.
`:delete-project'   - Show GUI for deleting project(s).
`:edit-project'     - Show GUI for editing project.
`:search-project'   - Show GUI for searching project (normally implemented by grep).
`:find-file'        - Show GUI for finding files in the project. The 3rd argument is 
                      a files list of current project.

### The sample of a front-end:
  (defun some-frontend (command callback &rest args)
    (case command
      (:creat-project (progn ...))
      (:destroy (kill-buffer)))
"
  :type '(repeat (symbol :tag "Front-end"))
  :group 'prj-group)

(defcustom prj-backends '(prj-filedb-backend
                          prj-async-grep-backend)
  "The list of back-ends for the purpose of indexing files or tagging files which
should be included in current project. Every back-end takes at least one argument, 
command.

### Basic Commands:
`:init'             - Initialize back-ends .
`:destroy'          - Ask back-ends to destroy something or free memory.

`:index-files'      - Collect files recursively under directories given by project
                      file-path setting.
`:files'            - Return a files list of current project. Optional arguemnt is
                      a list of document types when you want specific files.

`:search'           - Search string in the current project.

### The sample of a front-end:
  (defun some-backend (command &rest args)
    (case command
      (:index-files
       (progn ...))
      (:files
       (progn ...))))
"
  :type '(repeat (symbol :tag "Back-end"))
  :group 'prj-group)

(defcustom prj-after-build-database-hook nil
  "Hook run when entering `grep-mode' mode."
  :type 'hook
  :group 'prj-group)

(defvar prj-config nil
  "A plist which represent a project's configuration, it will be exported as 
format of JSON file.

### Plist Format:
`:name'             - Project's name. A string.
`:filepaths'        - A list containing file-path(s) string(s).
`:doctypes'         - A plist containing pairs of document type and document 
                      extensions. see `prj-document-types'.
`:recent-files'     - A list containing recent file-path(s) string(s).
`:search-history'   - A list containing recent searching string(s).
")

(defconst prj-config-name "config.db"
  "The file name of project configuration. see `prj-config' for detail.")

(defconst prj-search-history-max 8
  "Maximin elements count in the searh history cache.")

(defmacro prj-plist-put (plist prop val)
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defun prj-new-config ()
  "Return a config template. Check `prj-config' for detail."
  (let (config)
    (prj-plist-put config :name "")
    (prj-plist-put config :filepaths '())
    (prj-plist-put config :doctypes '())
    (prj-plist-put config :recent-files '())
    (prj-plist-put config :search-history '())
    config))

(defun prj-call-frontends (command &optional callback &rest args)
  "Call frontends and pass command and callback to them."
  (dolist (frontend prj-frontends)
    (apply frontend command callback args)))

(defun prj-call-backends (command &rest args)
  "Call backends and pass command to them. If any backend return non-nil will 
terminate the iteration and return its result unless commands is `:init' or 
`:destroy'."
  (let (ret)
    (catch 'break
      (dolist (backend prj-backends)
        (let ((ret (apply backend command args)))
          (and ret
               (not (member command '(:init :destroy)))
               (throw 'break ret)))))))

(defun prj-init-frontends ()
  (prj-call-frontends :init))

(defun prj-destroy-frontends ()
  (prj-call-frontends :destroy))

(defun prj-init-backends ()
  (prj-call-backends :init))

(defun prj-destroy-backends ()
  (prj-call-backends :destroy))

(defun prj-import-json (filename)
  "Read data exported by `prj-export-json' from file `filename'."
  (when (file-exists-p filename)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'list))
      (json-read-file filename))))

(defun prj-export-json (filename data)
  "Export `data' to `filename' file.."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (json-encode-plist data)))))

(defun prj-destroy-all ()
  "Clean search buffer or widget buffers which belongs to other project when 
user loads a project or unload a project."
  ;; Clean frontends.
  (prj-destroy-frontends)
  ;; Kill search buffer.
  (let ((search (get-buffer prj-searchdb-name)))
    (and search
         (with-current-buffer search
           (save-buffer)
           (kill-buffer))))
  ;; Reset configuration.
  (setq prj-config nil))

(defun prj-create-project-internal (data)
  "Internal function to create project. It is called by functions in the 
`prj-create-project-frontend'."
  (when data
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
      (prj-build-database))))

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
    (prj-destroy-all)
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
    (prj-init-backends)
    (prj-init-frontends)
    (message "Load [%s] ...done" (prj-project-name))))

(defun prj-edit-project-internal (data)
  "Internal function to edit project. It is called by functions in the 
`prj-edit-project-frontend'."
  (let ((doctypes (plist-get data :doctypes))
        (filepaths (plist-get data :filepaths)))
    (prj-plist-put prj-config :doctypes doctypes)
    (prj-plist-put prj-config :filepaths filepaths)
    (prj-export-json (prj-config-path) prj-config)
    ;; Update database.
    (let ((old-doctypes (prj-project-doctypes))
          (old-filepaths (prj-project-filepaths)))
      (if (or (/= (length doctypes) (length old-doctypes))
              (not (every 'equal doctypes old-doctypes))
              (/= (length filepaths) (length old-filepaths))
              (not (every 'equal filepaths old-filepaths)))
          (prj-build-database t)
        (prj-build-database)))))

(defun prj-search-project-internal (data)
  "Internal function to edit project. It is called by functions in the 
`prj-search-project-frontend'."
  (prj-call-backends :search data))

(defun prj-find-file-internal (file)
  (when (file-exists-p file)
    (his-add-position-type-history)
    (find-file file)
    (his-add-position-type-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defconst prj-searchdb-name "search.grep")

;;;###autoload
(defun prj-project-p ()
  "Return t if any project was loaded (current project)."
  (and prj-config
       (plist-get prj-config :name)))

;;;###autoload
(defun prj-config-path (&optional name)
  (expand-file-name (format "%s/%s/%s"
                            prj-workspace-path
                            (or name
                                (prj-project-name))
                            prj-config-name)))

;;;###autoload
(defun prj-searchdb-path ()
  (expand-file-name (format "%s/%s/%s"
                            prj-workspace-path
                            (prj-project-name)
                            prj-searchdb-name)))

;;;###autoload
(defun prj-project-name ()
  (plist-get prj-config :name))

;;;###autoload
(defun prj-project-doctypes ()
  (plist-get prj-config :doctypes))

;;;###autoload
(defun prj-project-filepaths ()
  (plist-get prj-config :filepaths))

;;;###autoload
(defun prj-project-recent-files ()
  (plist-get prj-config :recent-files))

;;;###autoload
(defun prj-project-search-history ()
  (plist-get prj-config :search-history))

;;;###autoload
(defun prj-project-files (&optional doctypes)
  "Return a list containing files in current project."
  (prj-call-backends :files doctypes))

;;;###autoload
(defun prj-workspace-projects ()
  "Return a list containing projects' name in current workspace."
  (let (projects)
    (dolist (name (directory-files prj-workspace-path))
      (unless (member name '("." ".."))
        (let ((config-file (prj-config-path name)))
          (when (and (file-exists-p config-file)
                     (not (string= name (prj-project-name))))
            (setq projects (append projects `(,name)))))))
    projects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-preference ()
  "Customize document types."
  (interactive)
  (prj-destroy-frontends)
  (customize-group 'prj-group))

;;;###autoload
(defun prj-count-files ()
  (interactive)
  (message "[%s] %s total files." (prj-project-name) (length (prj-project-files))))

;;;###autoload
(defun prj-create-project ()
  "Show configuration for creating new project."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-destroy-frontends)
  (prj-call-frontends :create-project
                      'prj-create-project-internal))

;;;###autoload
(defun prj-delete-project ()
  "Show configuration for deleting projects."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-destroy-frontends)
  (prj-call-frontends :delete-project
                      'prj-delete-project-internal))

;;;###autoload
(defun prj-edit-project ()
  "Show configuration for editing project's setting."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-destroy-frontends)
  ;; Load project if wasn't loaded.
  (unless (prj-project-p)
    (prj-load-project))
  (prj-call-frontends :edit-project
                      'prj-edit-project-internal))

;;;###autoload
(defun prj-load-project (&optional name)
  "List available prjects in current workspace and let user to choose which 
project to be loaded."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-destroy-frontends)
  (prj-destroy-backends)
  (if name
      (prj-load-project-internal name)
    (prj-call-frontends :load-project
                        'prj-load-project-internal
                        (prj-workspace-projects))))

;;;###autoload
(defun prj-load-recent-project ()
  "Load the project which user exits emacs last time."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  (prj-destroy-frontends)
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
  (let ((name (prj-project-name)))
    (prj-destroy-all)
    (message "Unload [%s] ...done" name)))

;;;###autoload
(defun prj-build-database (&optional is-rebuil)
  "Index file and tags."
  (interactive
   (when (string= "yes" (ido-completing-read
                         (format "[%s] Rebuild database? " (prj-project-name))
                         '("yes" "no")
                         nil
                         t))
     '(t)))
  (prj-destroy-frontends)
  (unless (prj-project-p)
    (prj-load-project))
  ;; Create file list which is the data base of the project's files.
  (when (prj-project-p)
    (message "Building database might take a while, please wait ...")
    (prj-call-backends :index-files is-rebuil)
    (run-hook-with-args prj-after-build-database-hook)
    (message "Database is updated!")))

;;;###autoload
(defun prj-find-file ()
  "Open file by the given file name."
  (interactive)
  (prj-destroy-frontends)
  ;; Load project if wasn't loaded.
  (unless (prj-project-p)
    (prj-load-project))
  (prj-call-frontends :find-file
                      'prj-find-file-internal
                      (prj-project-files)))

;;;###autoload
(defun prj-search-project ()
  "Search string in the project. Append new search result to the old caches."
  (interactive)
  (prj-destroy-frontends)
  ;; Load project if no project was loaded.
  (unless (prj-project-p)
    (prj-load-project))
  (prj-call-frontends :search-project
                      'prj-search-project-internal))

;;;###autoload
(defun prj-toggle-search-buffer ()
  (interactive)
  ;; TODO: bug when user is select definition window and try to toggle search buffer off.
  ;; TODO: use front-end???
  (unless (prj-project-p)
    (prj-load-project))
  (let ((buffer (get-buffer prj-searchdb-name)))
    (if buffer
        ;; Back to previous buffer of current window.
        (with-current-buffer buffer
          (and (buffer-modified-p)
               (save-buffer 0))
          (kill-buffer))
      ;; Go to search buffer.
      (his-add-position-type-history)
      (find-file (prj-searchdb-path))
      (his-add-position-type-history))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-kill-emacs-hook ()
  (prj-save-file-names))

(defun prj-save-file-names (&optional close)
  (when (prj-project-p)
    ;; Save file names opened in current session.
    (let ((searchdb (prj-searchdb-path))
          file files)
      (dolist (buffer (buffer-list))
        (when (and (buffer-live-p buffer)
                   (setq file (buffer-file-name buffer))
                   ;; Skip search database.
                   (not (string= file searchdb)))
          (setq files (append files `(,file)))
          ;; Close buffers.
          (when close
            (kill-buffer buffer))))
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
