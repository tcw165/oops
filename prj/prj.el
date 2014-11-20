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
;; - Support project's local variable.
;;
;;; Interactive API (for end user) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  `prj-preference'
;;  `prj-create-project'
;;  `prj-delete-project'
;;  `prj-edit-project'
;;  `prj-load-project'
;;  `prj-load-recent-project'
;;  `prj-unload-project'
;;  `prj-build-database'
;;  `prj-find-file'
;;  `prj-search-project'
;;
;;; Developer API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  `prj-project-p'
;;  `prj-workspace-projects'
;;  `prj-project-config-dir'
;;  `prj-project-name'
;;  `prj-project-doctypes'
;;  `prj-project-filepaths'
;;  `prj-project-recent-files'
;;  `prj-project-files'
;;  `prj-project-set'
;;  `prj-export-config'
;;  `prj-searchdb-buffer'
;;
;;; Implementation for Front-Ends:
;;  `prj-create-project-impl'
;;  `prj-delete-project-impl'
;;  `prj-edit-project-impl'
;;  `prj-load-project-impl'
;;  `prj-find-file-impl'
;;  `prj-search-project-impl'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-08-01 (0.0.1)
;;    Initial release.

(and load-file-name
     (let ((dir (file-name-directory load-file-name)))
       (add-to-list 'load-path (concat dir "/frontends"))
       (add-to-list 'load-path (concat dir "/backends"))))

;; Default frontends.
(require 'prj-default-frontend)
;; Default backends.
(require 'prj-default-backend)
;; Default modes.
(require 'prj-grep-mode)

(defgroup prj-group nil
  "A Project management utility. This utility provides you a workspace and many 
projects concept. It also provide you a way to easily find file without knowing 
its full path; Add different directories with specific document types in a 
project; Powerful selective grep string or regular expression in a project, etc."
  :tag "Prj")

(defcustom prj-workspace-path "~/.emacs.d/.workspace"
  "The place storing all the projects' configurations."
  :type 'directory
  :set 'prj-cus-set-workspace
  :initialize 'custom-initialize-default
  :group 'prj-group)

(defcustom prj-document-types '(("Text" . "*.txt;*.md")
                                ("Emacs Lisp" . ".emacs;*.el;*.el.gz")
                                ("Shell" . ".bashrc;.bash_profile;.zshrc;*.sh")
                                ("Python" . "*.py")
                                ("Octave" . "*.m")
                                ("C/C++ Header" . "*.h;*.hxx;*.hpp")
                                ("C/C++ Source" . "*.c;*.cxx;*.cpp"))
  "Categorize file names refer to specific matches and give them type names. 
It is an alist of (DOC_NAME MATCHES). Each matches in MATCHES should be delimit 
with ';'."
  :type '(repeat (cons (string :tag "Type")
                       (string :tag "File")))
  :group 'prj-group)

(defcustom prj-exclude-types ".git;.svn"
  "Those kinds of file should be excluded in the project. Each matches should 
be delimit with ';'."
  ;; TODO: give GUI a pretty appearance.
  :type 'string
  :group 'prj-group)

(defcustom prj-frontends '(prj-create-project-frontend
                           prj-delete-project-frontend
                           prj-edit-project-frontend
                           prj-load-project-frontend
                           prj-find-file-frontend
                           prj-search-project-frontend)
  "The list of front-ends for the purpose of visualization. Every front-end takes
at least one argument, command. Optional arguement, callback, is the implementation 
provided by engine to complete task in respect of the given command. The engine 
will iterate through these back-ends, if any back-end return non-nil and command 
is neither `:init' nor `:destroy' will make engine discard remaining iteration.

### Commands:
`:init'             - Initialize frontends.
`:destroy'          - Destroy frontends.

`:show'             - Ask frontends to show their GUIs.
`:hide'             - Ask frontends to hide their GUIs.

`:create-project'   - Show GUI for creating project.
                      Default is `prj-create-project-frontend'.
`:delete-project'   - Show GUI for deleting project(s).
                      Default is `prj-delete-project-frontend'.
`:edit-project'     - Show GUI for editing project.
                      Default is `prj-edit-project-frontend'.
`:load-project'     - Show GUI for loading project. 2nd argument is project's file 
                      list.
                      Default is `prj-load-project-frontend'.
`:find-files'       - Show GUI for finding files in the project. 2nd argument is a 
                      files list of current project.
                      Default is `prj-find-file-frontend'.
`:search-project'   - Show GUI for searching project.
                      Default is `prj-search-project-frontend'."
  :type '(list (symbol :tag "Create Project Frontend")
               (symbol :tag "Delete Project Frontend")
               (symbol :tag "  Edit Project Frontend")
               (symbol :tag "  Load Project Frontend")
               (symbol :tag "     Find File Frontend")
               (symbol :tag "Search Project Frontend"))
  :group 'prj-group)

(defcustom prj-backends '(prj-index-files-backend
                          prj-find-files-backend
                          prj-async-grep-backend)
  "The list of back-ends for the purpose of indexing files or tagging files which 
should be included in current project. Every back-end takes at least one argument, 
command.

### Commands:
`:init'             - Initialize back-ends .
`:destroy'          - Ask back-ends to destroy something or free memory.

`:index-files'      - Collect files recursively under directories given by project
                      file-path setting.
`:find-files'       - Return a files list of current project. Optional arguemnt is
                      a list of keys of `prj-document-types' when you want specific 
                      files.

`:search'           - Search string in the current project. Optional argument ..."
  :type '(list (symbol :tag "Index Files Backend")
               (symbol :tag " Find Files Backend")
               (symbol :tag "     Search Backend"))
  :group 'prj-group)

(defcustom prj-search-history-max 8
  "Maximum history amount."
  :type 'integer
  :group 'prj-group)

(defcustom prj-after-build-database-hook nil
  "Hook run after project's database is built."
  :type 'hook
  :group 'prj-group)

(defvar prj-name nil)

(defvar prj-config nil
  "A plist which represent a project's configuration, it will be exported as 
format of JSON file. see `prj-new-config'.

### Plist Format:
`:filepaths'        - A list containing file-path(s) string(s).
`:doctypes'         - A plist containing pairs of document type and document 
                      extensions. see `prj-document-types'.
`:recent-files'     - A list containing recent file-path(s) string(s).")

(defmacro prj-plist-put (plist prop val)
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defun prj-config-path (&optional name)
  (expand-file-name (format "%s/%s/config.db"
                            prj-workspace-path
                            (or name
                                (prj-project-name)))))

(defun prj-searchdb-path (&optional name-only)
  (let ((name "search.grep"))
    (if name-only
        name
      (expand-file-name (format "%s/%s/%s"
                                prj-workspace-path
                                (prj-project-name)
                                name)))))

(defun prj-cus-set-workspace (symbol value)
  "Make sure the directory is present."
  (when (stringp value)
    (let ((project (prj-project-name))
          (old-path prj-workspace-path)
          (new-path (expand-file-name (or (and (string-match "\\(.*\\)/$" value)
                                              (match-string 1 value))
                                         value))))
      (make-directory (file-name-directory new-path) t)
      (rename-file old-path new-path)
      (set symbol new-path)
      (prj-unload-project)
      (prj-load-project project))))

(defun prj-new-config ()
  "Return a config template. Check `prj-config' for detail."
  (let (config)
    (prj-plist-put config :filepaths '())
    (prj-plist-put config :doctypes '())
    (prj-plist-put config :recent-files '())
    config))

(defun prj-import-json (filename)
  "Read data exported by `prj-export-json' from file `filename'."
  (when (and (file-exists-p filename) (require 'json))
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'list))
      (json-read-file filename))))

(defun prj-export-json (filename data)
  "Export `data' to `filename' file.."
  (when (and (file-writable-p filename) (require 'json))
    (with-temp-file filename
      (insert (json-encode-plist data)))))

(defun prj-init-frontends ()
  (dolist (backend prj-frontends)
    (funcall backend :init)))

(defun prj-destroy-frontends ()
  (dolist (backend prj-frontends)
    (funcall backend :destroy)))

(defun prj-hide-frontends ()
  (dolist (backend prj-frontends)
    (funcall backend :hide)))

(defun prj-show-frontends (command &rest args)
  ;; Hide all frontends.
  (prj-hide-frontends)
  ;; Find correct frontend.
  (let ((commands '(:create-project
                    :delete-project
                    :edit-project
                    :load-project
                    :find-files
                    :search-project))
        (funcs prj-frontends))
    (catch 'break
      (while commands
        (when (eq command (car commands))
          (let ((ret (apply (car funcs) command args)))
            (and ret (throw 'break ret))))
        (setq commands (cdr commands)
              funcs (cdr funcs))))))

(defun prj-init-backends ()
  (dolist (backend prj-backends)
    (funcall backend :init)))

(defun prj-destroy-backends ()
  (dolist (backend prj-backends)
    (funcall backend :destroy)))

(defun prj-call-backends (command &rest args)
  (let ((commands '(:index-files
                    :find-files
                    :search))
        (funcs prj-backends))
    (catch 'break
      (while commands
        (when (eq command (car commands))
          (let ((ret (apply (car funcs) command args)))
            (and ret (throw 'break ret))))
        (setq commands (cdr commands)
              funcs (cdr funcs))))))

(defun prj-clean-config&search ()
  "Clean search buffer or widget buffers which belongs to other project when 
user loads a project or unload a project."
  ;; Kill search buffer.
  (let ((buffer (prj-searchdb-buffer)))
    (and buffer (kill-buffer buffer)))
  ;; Reset configuration.
  (setq prj-name nil
        prj-config nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Developer API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-project-p ()
  "Return t if any project was loaded (current project)."
  (and prj-config t))

;;;###autoload
(defun prj-workspace-projects ()
  "Return a list containing projects' name in current workspace."
  (unless (file-exists-p prj-workspace-path)
    (make-directory prj-workspace-path t))
  (let (projects)
    (dolist (name (directory-files prj-workspace-path))
      (unless (member name '("." ".."))
        (let ((config-file (prj-config-path name)))
          (when (and (file-exists-p config-file)
                     (not (string= name (prj-project-name))))
            (setq projects (append projects `(,name)))))))
    projects))

;;;###autoload
(defun prj-project-config-dir ()
  (expand-file-name (format "%s/%s"
                            prj-workspace-path
                            (prj-project-name))))

;;;###autoload
(defun prj-project-name ()
  prj-name)

;;;###autoload
(defun prj-project-doctypes (&optional key)
  (plist-get prj-config :doctypes))

;;;###autoload
(defun prj-project-filepaths ()
  (plist-get prj-config :filepaths))

;;;###autoload
(defun prj-project-recent-files ()
  (plist-get prj-config :recent-files))

;;;###autoload
(defun prj-project-files (&optional doctypes filepaths is-list)
  "Return a list containing files in current project."
  (prj-call-backends :find-files doctypes filepaths is-list))

;;;###autoload
(defun prj-project-set (key value)
  (and key value
       (prj-plist-put prj-config key value)))

;;;###autoload
(defun prj-export-config ()
  (prj-export-json (prj-config-path) prj-config))

;;;###autoload
(defun prj-searchdb-buffer (&optional reload)
  (if reload
      (find-file-noselect (prj-searchdb-path))
    (get-buffer (prj-searchdb-path t))))

;;;###autoload
(defun prj-create-project-impl (name doctypes filepaths)
  "Internal function to create project. It is called by functions in the 
`prj-create-project-frontend'."
  (let* ((path (prj-config-path name))
         (dir (file-name-directory (expand-file-name path)))
         (config (prj-new-config)))
    ;; Prepare project directory.
    (unless (file-directory-p dir)
      (make-directory dir t))
    ;; Export configuration.
    (prj-plist-put config :doctypes doctypes)
    (prj-plist-put config :filepaths filepaths)
    (prj-export-json path config)
    ;; Load project.
    (prj-load-project name)
    ;; Update database
    (prj-build-database)))

;;;###autoload
(defun prj-delete-project-impl (projects)
  "Internal function to delete project. It is called by functions in the 
`prj-delete-project-frontend'."
  (dolist (project projects)
    ;; Unload current project if it is selected.
    (when (and (prj-project-p)
               (string= project (prj-project-name)))
      (prj-unload-project))
    ;; Delete directory
    (delete-directory (format "%s/%s" prj-workspace-path project) t t))
  (message "Delet project ...done"))

;;;###autoload
(defun prj-load-project-impl (name)
  "Internal function to edit project. It is called by functions in the 
`prj-load-project-frontend'."
  ;; Save files in the last session.
  (prj-save-recent-files t)
  (prj-clean-config&search)
  ;; Read configuration.
  (setq prj-name name
        prj-config (prj-import-json (prj-config-path name)))
  ;; Open files.
  (ignore-errors
    (mapc 'find-file-existing (prj-project-recent-files)))
  (if prj-project-mode
      (progn
        (prj-destroy-backends)
        (prj-init-backends))
    (prj-project-mode 1))
  (message "Load [%s] ...done" (prj-project-name)))

;;;###autoload
(defun prj-edit-project-impl (doctypes filepaths)
  "Internal function to edit project. It is called by functions in the 
`prj-edit-project-frontend'."
  (let ((old-doctypes (prj-project-doctypes))
        (old-filepaths (prj-project-filepaths)))
    (prj-plist-put prj-config :doctypes doctypes)
    (prj-plist-put prj-config :filepaths filepaths)
    (prj-export-config)
    ;; Update database.
    (unless (and (equal doctypes old-doctypes)
                 (equal filepaths old-filepaths))
      (prj-build-database))))

;;;###autoload
(defun prj-find-file-impl (file)
  (when (file-exists-p file)
    (his-add-position-type-history)
    (find-file file)
    (his-add-position-type-history)))

;;;###autoload
(defun prj-search-project-impl (match &optional doctypes filepaths case-sensitive word-only)
  "Internal function to edit project. It is called by functions in the 
`prj-search-project-frontend'."
  ;; Filter document types for acceleration.
  (and (equal doctypes (prj-project-doctypes))
       (setq doctypes nil))
  ;; Filter file paths for acceleration.
  (and (equal filepaths '(""))
       (setq filepaths nil))
  ;; Start to search.
  (prj-call-backends :search
                     match
                     (prj-project-files doctypes filepaths)
                     (prj-searchdb-path)
                     case-sensitive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-preference ()
  "Customize document types."
  (interactive)
  (prj-hide-frontends)
  (customize-group 'prj-group))

;;;###autoload
(defun prj-count-files ()
  (interactive)
  (message "[%s] %s total files."
           (prj-project-name)
           (with-temp-buffer
             (insert-file-contents-literally (prj-project-files))
             (1- (count-lines 1 (point-max))))))

;;;###autoload
(defun prj-create-project ()
  "Show configuration for creating new project."
  (interactive)
  (prj-show-frontends :create-project))

;;;###autoload
(defun prj-delete-project ()
  "Show configuration for deleting projects."
  (interactive)
  (prj-show-frontends :delete-project))

;;;###autoload
(defun prj-edit-project ()
  "Show configuration for editing project's setting."
  (interactive)
  (if (prj-project-p)
      (prj-show-frontends :edit-project)
    (message "No project was loaded!")))

;;;###autoload
(defun prj-load-project (&optional name)
  "List available prjects in current workspace and let user to choose which 
project to be loaded."
  (interactive)
  (if name
      (prj-load-project-impl name)
    (prj-show-frontends :load-project (prj-workspace-projects))))

;;;###autoload
(defun prj-load-recent-project ()
  "Load the project which user exits emacs last time."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  ;; TODO:
  nil)

;;;###autoload
(defun prj-unload-project ()
  "Unload current project."
  (interactive)
  (unless prj-project-mode
    (prj-project-mode 1))
  ;; Save files in the last session.
  (prj-save-recent-files t)
  ;; Stop saveplaces.
  (setq-default save-place nil)
  (let ((name (prj-project-name)))
    (prj-clean-config&search)
    (message "Unload [%s] ...done" name)))

;;;###autoload
(defun prj-build-database ()
  (interactive)
  ;; Create file list which is the data base of the project's files.
  (when (prj-project-p)
    (message "[%s] Building database might take a while, please wait ..."
             (prj-project-name))
    (prj-call-backends :index-files)
    (run-hooks 'prj-after-build-database-hook)
    (message "[%s] Database is updated!" (prj-project-name))))

;;;###autoload
(defun prj-find-file ()
  "Open file by the given file name."
  (interactive)
  (and (prj-project-p)
       (prj-show-frontends :find-files (prj-project-files nil nil t))))

;;;###autoload
(defun prj-search-project ()
  "Search string in the project. Append new search result to the old caches."
  (interactive)
  ;; Load project if no project was loaded.
  (and (prj-project-p)
       (prj-show-frontends :search-project)))

;;;###autoload
(defun prj-toggle-search-buffer ()
  (interactive)
  (when (prj-project-p)
    (let ((buffer (prj-searchdb-buffer)))
      (if (eq (window-buffer) buffer)
          (kill-buffer)
        (and buffer (kill-buffer buffer))
        ;; Go to search buffer.
        (his-add-position-type-history)
        (switch-to-buffer (prj-searchdb-buffer t) t t)
        (his-add-position-type-history)))))

;;;###autoload
(defun prj-list-files ()
  (interactive)
  (switch-to-buffer (list-buffers-noselect) t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-save-recent-files (&optional close)
  (when (prj-project-p)
    ;; Save file names opened in current session.
    (let (file
          files)
      (dolist (buffer (buffer-list))
        (when (and (buffer-live-p buffer)
                   ;; Skip search database.
                   (not (eq buffer (prj-searchdb-buffer)))
                   (setq file (buffer-file-name buffer)))
          (push file files)
          ;; Close buffers.
          (when close
            (kill-buffer buffer))))
      ;; Export configuration.
      (prj-plist-put prj-config :recent-files files)
      (prj-export-config))))

(defun prj-kill-emacs-hook ()
  (prj-save-recent-files))

;;;###autoload
(define-minor-mode prj-project-mode
  "Provide convenient menu items and tool-bar items for project feature."
  :lighter " Project"
  :global t
  (if prj-project-mode
      (progn
        (add-hook 'kill-emacs-hook 'prj-kill-emacs-hook t nil)
        (prj-init-frontends)
        (prj-init-backends))
    (remove-hook 'kill-emacs-hook 'prj-kill-emacs-hook nil)
    (prj-destroy-frontends)
    (prj-destroy-backends)))

;; Force to reload save-place database.
(require 'saveplace)
(setq-default save-place t)
(setq save-place-loaded nil
      save-place-file (format "%s/saveplaces.db"
                              (expand-file-name prj-workspace-path)))

(provide 'prj)
