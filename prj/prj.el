;; Copyright (C) 2014, 2015
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

;; Built-in libraries.
(require 'saveplace)

;; 3rd party librarise
(require 'async)
(require 'helm)

;; Part of this package.
(require 'prj-gui)

(defgroup prj nil
  "A Project management utility. This utility provides you a workspace and many
projects concept. It also provide you a way to easily find file without knowing
its full path; Add different directories with specific document types in a
project; Powerful selective grep string or regular expression in a project, etc.")

(defcustom prj-workspace-path "~/.emacs.d/.workspace"
  "The place storing all the projects' configurations."
  :type 'directory
  :set 'prj-cus-set-workspace
  :initialize 'custom-initialize-default
  :group 'prj)

;; TODO: Use regexp.
(defcustom prj-document-types '(("All" . "*")
                                ("Text" . "*.txt;*.md")
                                ("Json" . "*.json")
                                ("XML Like" . "*.xml;*.yml")
                                ("Emacs Lisp" . ".emacs;*.el;*.el.gz")
                                ("Shell" . ".bashrc;.bash_profile;.zshrc;*.sh")
                                ("Python" . "*.py")
                                ("JavaScript" . "*.js")
                                ("HTML/CSS" . "*.html;*.css;*.styl;*.jade")
                                ("C/C++ Header" . "*.h;*.hxx;*.hpp")
                                ("C/C++ Source" . "*.c;*.cxx;*.cpp"))
  "Categorize file names refer to specific matches and give them type names.
It is an alist of (DOC_NAME MATCHES). Each matches in MATCHES should be delimit
with ';'."
  :type '(repeat (cons (string :tag "Type")
                       (string :tag "File")))
  :group 'prj)

;; TODO: Use regexp.
(defcustom prj-exclude-types ".git;.svn"
  "Those kinds of file should be excluded in the project. Each matches should
be delimit with ';'."
  :type 'string
  :group 'prj)

(defvar prj-project-name nil)

(defvar prj-project-config nil
  "A plist which represent a project's configuration, it will be exported as
format of JSON file. see `prj-new-config'.

### Plist Format:
`:filepaths'        - A list containing file-path(s) string(s).
`:doctypes'         - A plist containing pairs of document type and document
                      extensions. see `prj-document-types'.
`:opened-files'     - A list containing recent file-path(s) string(s).")

(defvar prj-files-number 0)

(defmacro prj-plist-put (plist prop val)
  `(setq ,plist (plist-put ,plist ,prop ,val)))

(defun prj-project-config-path (&optional name)
  "Return a full path for the project's configuration file. If NAME is provided,
 return a new path of configuration file."
  (expand-file-name (format "%s/%s/config.db"
                            prj-workspace-path
                            (or name
                                prj-project-name))))

(defun prj-cus-set-workspace (symbol value)
  "Make sure the directory is present."
  (when (stringp value)
    (let ((project prj-project-name)
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
  "Return a config template. Check `prj-project-config' for detail."
  (let (config)
    (prj-plist-put config :filepaths '())
    (prj-plist-put config :doctypes '())
    (prj-plist-put config :opened-files '())
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

(defun prj-project-p ()
  "Return t if any project was loaded."
  (and prj-project-name prj-project-config))

(defun prj-workspace-projects ()
  "Return a list containing projects' name in current workspace."
  (unless (file-exists-p prj-workspace-path)
    (make-directory prj-workspace-path t))
  (let (projects)
    (dolist (name (directory-files prj-workspace-path))
      (unless (member name '("." ".."))
        (let ((config-file (prj-project-config-path name)))
          (when (and (file-exists-p config-file)
                     (not (string= name prj-project-name)))
            (setq projects (append projects `(,name)))))))
    projects))

(defun prj-project-doctypes ()
  (plist-get prj-project-config :doctypes))

(defun prj-project-filepaths ()
  (plist-get prj-project-config :filepaths))

(defun prj-project-opened-files ()
  (plist-get prj-project-config :opened-files))

(defun prj-export-config ()
  (prj-export-json (prj-project-config-path) prj-project-config))

(defun prj-kill-old-session ()
  "Clean search buffer or widget buffers which belongs to other project when
user loads a project or unload a project."
  ;; Kill search buffer.
  ;; (let ((buffer (prj-searchdb-buffer)))
  ;;   (and buffer (kill-buffer buffer)))
  ;; Reset configuration.
  (setq prj-project-name nil
        prj-project-config nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-save-opened-files (&optional isKill)
  "Save opened files list in the project's configuration. Kill buffer if IS-KILL
 is t."
  (when (prj-project-p)
    ;; Save file names opened in current session.
    (let (file
          files)
      (dolist (buffer (buffer-list))
        (when (and (buffer-live-p buffer)
                   ;; Skip search database.
                   ;; (not (eq buffer (prj-searchdb-buffer)))
                   (setq file (buffer-file-name buffer)))
          (push file files)
          ;; Kill buffers.
          (when isKill
            (kill-buffer buffer))))
      ;; Export configuration.
      (prj-plist-put prj-project-config :opened-files files)
      (prj-export-config))))

(define-minor-mode prj-project-mode
  "Provide convenient menu items and tool-bar items for project feature."
  :lighter " Prj"
  :global t
  :group 'prj
  (if prj-project-mode
      (progn
        (add-hook 'kill-emacs-hook 'prj-save-opened-files t nil)
        ;; Force `saveplace' to reload database.
        (setq save-place-loaded nil))
    (remove-hook 'kill-emacs-hook 'prj-save-opened-files nil)))

;; `saveplace'
(custom-set-variables
 '(save-place t)
 `(save-place-file ,(format "%s/saveplaces.db"
                            (expand-file-name prj-workspace-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old Database Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar prj-total-files-cache nil)

;; (defun prj-export-data (filename data)
;;   "Export `data' to `filename' file. The saved data can be imported with `prj-import-data'."
;;   (when (file-writable-p filename)
;;     (with-temp-file filename
;;       (insert (let (print-length)
;; 		(prin1-to-string data))))))

;; (defun prj-import-data (filename)
;;   "Read data exported by `prj-export-data' from file `filename'."
;;   (when (file-exists-p filename)
;;     (with-temp-buffer
;;       (insert-file-contents filename)
;;       (read (buffer-string)))))

;; (defun prj-filedb-path (&optional name)
;;   (let ((dir (expand-file-name (format "%s/%s/files"
;;                                        prj-workspace-path
;;                                        prj-project-name))))
;;     (if name
;;         (format "%s/%s.files" dir (or (and (string= name "all") name)
;;                                       (and (string-match "^temp.*" name) name)
;;                                       (secure-hash 'sha1 name)))
;;       dir)))

;; (defun prj-total-files-cache-path ()
;;   (prj-filedb-path "temp0"))

;; (defun prj-filepaths-gen (filepaths)
;;   (let ((filepath filepaths)
;;         ret)
;;     (while filepath
;;       (setq ret (concat ret (car filepath))
;;             filepath (cdr filepath))
;;       (and filepath
;;            (setq ret (concat ret " "))))
;;     ret))

;; (defun prj-matches-gen (doctype &optional is-exclude)
;;   "Convert DOCTYPE to string as include-path parameter for FIND.
;; Example:
;;   *.md;*.el;*.txt   =>   -name *.md -o -name *.el -o -name *.txt
;; If `is-exclude' is t:
;;   *.git;*.svn       =>   ! -name .git ! -name .svn"
;;   (if is-exclude
;;       (let ((matches (concat "! -name \"" doctype "\"")))
;;         (replace-regexp-in-string ";" "\" ! -name \"" matches))
;;     (let ((matches (concat "-name \"" doctype "\"")))
;;       (replace-regexp-in-string ";" "\" -o -name \"" matches))))

;; (defun prj-process-find (filepaths output &optional matches excludes)
;;   (let ((dir (file-name-directory output)))
;;     (unless (file-exists-p dir)
;;       (make-directory dir t))
;;     (call-process-shell-command
;;      (format "find %s -type f %s %s 1>\"%s\""
;;              filepaths
;;              (or matches "")
;;              (or excludes "")
;;              output))))

;; (defun prj-process-cat (input-file output-file)
;;   (call-process-shell-command (format "cat \"%s\" 1>>\"%s\""
;;                                       input-file
;;                                       output-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-new-project-impl (name doctypes filepaths)
  "Internal function to create project. It is called by functions in the
`prj-new-project-frontend'."
  (let* ((path (prj-project-config-path name))
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
    ;; (prj-build-database)
    ))

(defun prj-delete-project-impl (projects)
  "Internal function to delete project. It is called by functions in the
`prj-delete-project-frontend'."
  (dolist (project projects)
    ;; Unload current project if it is selected.
    (when (and (prj-project-p)
               (string= project prj-project-name))
      (prj-unload-project))
    ;; Delete directory
    (delete-directory (format "%s/%s" prj-workspace-path project) t t))
  (message "Delet project ...done"))

(defun prj-edit-project-impl (doctypes filepaths)
  "Internal function to edit project. It is called by functions in the
`prj-edit-project-frontend'."
  (let ((old-doctypes (prj-project-doctypes))
        (old-filepaths (prj-project-filepaths)))
    (prj-plist-put prj-project-config :doctypes doctypes)
    (prj-plist-put prj-project-config :filepaths filepaths)
    (prj-export-config)
    ;; Update database.
    ;; (unless (and (equal doctypes old-doctypes)
    ;;              (equal filepaths old-filepaths))
    ;;   (prj-build-database))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-new-project ()
  "Show widget for creating new project."
  (interactive)
  (prj-gui-new-project 'prj-new-project-impl))

;;;###autoload
(defun prj-delete-project ()
  "Show widget for deleting projects."
  (interactive)
  (prj-gui-delete-project 'prj-delete-project-impl))

;;;###autoload
(defun prj-edit-project ()
  "Show widget for editing project's setting."
  (interactive)
  (if (prj-project-p)
      (prj-gui-edit-project 'prj-edit-project-impl)
    (message "No project was loaded!")))

;;;###autoload
(defun prj-load-project (&optional name)
  "List available prjects in current workspace and let user to choose which
project to be loaded."
  (interactive)
  (let* ((pname (prj-workspace-projects))
         (name (unless name
                 (if (> (length pname) 1)
                     (ido-completing-read "Load project: "
                                          pname nil t)
                   (car pname)))))
    (when name
      (prj-save-opened-files t)
      (prj-kill-old-session)
      ;; Read configuration.
      (setq prj-project-name name
            prj-project-config (prj-import-json (prj-project-config-path)))
      (prj-project-mode 1)
      ;; Open files.
      (switch-to-buffer (car (last (mapcar
                                    (lambda (fname)
                                      (message "[%s] Open... %s"
                                               prj-project-name
                                               fname)
                                      (and (file-exists-p fname)
                                           (find-file-noselect fname)))
                                    (prj-project-opened-files)))))
      (message "[%s] Loaded!" prj-project-name))))

;;;###autoload
(defun prj-unload-project ()
  "Unload current project."
  (interactive)
  ;; Save files in the last session.
  (prj-save-opened-files t)
  (let ((name prj-project-name))
    (prj-kill-old-session)
    (prj-project-mode -1)
    (message "[%s] Unloaded!" name)))

;; ;;;###autoload
;; (defun prj-build-database ()
;;   (interactive)
;;   (when (prj-project-p)
;;     ;; TODO: Use `async'.
;;     (message "[%s] Building database might take a while, please wait ..."
;;              prj-project-name)
;;     (let* ((filepaths (prj-filepaths-gen (prj-project-filepaths)))
;;            (excludes (prj-matches-gen prj-exclude-types t))
;;            (db-all (prj-filedb-path "all"))
;;            (db-dir (file-name-directory db-all)))
;;       ;; New database.
;;       (when (file-exists-p db-dir)
;;         (delete-directory db-dir t nil))
;;       (make-directory db-dir)
;;       (write-region 1 1 db-all nil 0)
;;       ;; Collect files in respect of document types.
;;       (dolist (doctype (prj-project-doctypes))
;;         (let ((matches (prj-matches-gen
;;                         (cdr (assoc doctype prj-document-types))))
;;               (db-doctype (prj-filedb-path doctype)))
;;           ;; Create single category database.
;;           (prj-process-find filepaths db-doctype matches excludes)
;;           ;; Concatenate single category database to all categories database.
;;           (prj-process-cat db-doctype db-all))))
;;     (message "[%s] Database is updated!" prj-project-name)))

;;;###autoload
(defun prj-find-file ()
  "Open file by the given file name."
  (interactive)
  (when (prj-project-p)
    ;; TODO: Use `helm', `helm-build-async-source'
    ;; ;; Reference 1.
    ;; (require 'helm-mode)
    ;; (let* ((preselection (or (dired-get-filename nil t)
    ;;                          (buffer-file-name (current-buffer))))
    ;;        (only    (helm-read-file-name
    ;;                  "Search in file(s): "
    ;;                  :marked-candidates t
    ;;                  :preselect (and helm-do-grep-preselect-candidate
    ;;                                  (if helm-ff-transformer-show-only-basename
    ;;                                      (helm-basename preselection)
    ;;                                    preselection))))
    ;;        (prefarg (or current-prefix-arg helm-current-prefix-arg)))
    ;;   (helm-do-grep-1 only prefarg))
    (helm :sources (helm-build-async-source "test2"
                     :candidate-number-limit 9999
                     :candidates-process
                     (lambda ()
                       (start-process
                        "Find" nil "find" (car (prj-project-filepaths)))))
          :buffer (format "*[%s] Find File*" prj-project-name)
          :truncate-lines t)
    ))

;;;###autoload
(defun prj-search-project ()
  "Search string in the project. Append new search result to the old caches."
  (interactive)
  (when (prj-project-p)
    (message "Constructing...")))

(provide 'prj)
