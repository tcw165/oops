;; Copyright (C) 2014
;;
;; Author: BoyW165
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

(require 'cus-edit)
(require 'widget)
(require 'wid-edit)
(require 'tooltip)
(require 'company)
(require 'company-files)

(defvar prj-ok-func nil)
(make-variable-buffer-local 'prj-ok-func)

(defvar prj-widget-textfield nil)
(make-variable-buffer-local 'prj-widget-textfield)

(defvar prj-widget-checkboxes nil)
(make-variable-buffer-local 'prj-widget-checkboxes)

(defvar prj-widget-filepaths nil)
(make-variable-buffer-local 'prj-widget-filepaths)

(defvar prj-widget-doctypes nil)
(make-variable-buffer-local 'prj-widget-doctypes)

(defvar prj-widget-case-sensitive nil)
(make-variable-buffer-local 'prj-widget-case-sensitive)

(defvar prj-widget-word-only nil)
(make-variable-buffer-local 'prj-widget-word-only)

(defvar prj-widget-skip-comment nil)
(make-variable-buffer-local 'prj-widget-skip-comment)

(defface prj-button-face
  '((t (:background "lightgrey" :foreground "black" :weight bold)))
  "Face for button."
  :group 'prj-group)

(defface prj-button-mouse-face
  '((t (:background "grey90" :foreground "black" :weight bold)))
  "Face for mouse button (when mouse is over the button)."
  :group 'prj-group)

(defface prj-button-pressed-face
  '((t (:background "cyan3" :foreground "black" :weight bold)))
  "Face for pressed button."
  :group 'prj-group)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-create-project-widget-frontend (command &optional ok)
  (case command
    (:show
     (prj-with-widget "*Create Project*"
       ;; Ok implementation callback ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ok

       ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (lambda (&rest ignore)
         (let ((name (widget-value prj-widget-textfield))
               (doctypes (prj-widget-doctypes))
               (filepaths (prj-widget-filepaths)))
           (unless (> (length name) 0)
             (error "Project name is empty!"))
           (unless (> (length doctypes) 0)
             (error "No document types is selected!"))
           (unless (> (length filepaths) 0)
             (error "No valid file path!"))
           ;; call `prj-create-project-internal'.
           (and prj-ok-func
                (funcall prj-ok-func
                         `(:name ,name :doctypes ,doctypes
                                 :filepaths ,filepaths)))
           (kill-buffer)))

       ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Widget for project name.
       (setq prj-widget-textfield
             (widget-create 'editable-field
                            :format "Project Name: %v"))
       (widget-insert "\n")
       (widget-insert "Document Types (Edit) ") ;; TODO: add customizable link
       (widget-create 'push-button
                      :notify (prj-widget-checkbox-select-all prj-widget-doctypes)
                      "Select All")
       (widget-insert " ")
       (widget-create 'push-button
                      :notify (prj-widget-checkbox-deselect-all prj-widget-doctypes)
                      "Deselect All")
       (widget-insert " :\n")
       ;; Widget for doctypes.
       (let (wid)
         (dolist (doctype prj-document-types)
           (setq wid (widget-create 'checkbox
                                    :data doctype
                                    :format (concat "%[%v%] "
                                                    (prj-format-doctype doctype)
                                                    "\n"))
                 prj-widget-doctypes (append prj-widget-doctypes
                                              `(,wid)))))
       (widget-insert "\n")
       (widget-insert "Include Path:\n")
       (widget-insert (propertize "- Button INS to add a path; Button DEL to \
remove one.\n"
                                  'face 'font-lock-string-face))
       (widget-insert (propertize "- Use TAB to get a path prompt.\n"
                                  'face 'font-lock-string-face))
       ;; Widget for filepaths.
       (setq prj-widget-filepaths
             (widget-create 'editable-list
                            :entry-format "%i %d path: %v"
                            :value '("")
                            ;; Put :company to make company work for it.
                            '(editable-field :company prj-browse-file-backend)))))
    (:hide
     (and (get-buffer "*Create Project*")
          (kill-buffer "*Create Project*")))))

;;;###autoload
(defun prj-delete-project-widget-frontend (command &optional ok)
  (case command
    (:show
     (prj-with-widget "*Delete Project*"
       ;; Ok implementation callback ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ok

       ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (lambda (&rest ignore)
         (let ((projects (prj-widget-checkboxes)))
           (unless (> (length projects) 0)
             (error "No project is selected!"))
           ;; call `prj-delete-project-internal'.
           (and prj-ok-func
                (funcall prj-ok-func projects))
           (kill-buffer)))

       ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (widget-insert "Delete project ")
       (widget-create 'push-button
                      :notify (prj-widget-checkbox-select-all prj-widget-checkboxes)
                      "Select All")
       (widget-insert " ")
       (widget-create 'push-button
                      :notify (prj-widget-checkbox-deselect-all prj-widget-checkboxes)
                      "Deselect All")
       (widget-insert " :\n")
       ;; Widget for project name.
       (let (projects)
         ;; Find out all the projects.
         (dolist (file (directory-files prj-workspace-path))
           (let ((config (prj-config-path file)))
             (when (file-exists-p config)
               (setq projects (append projects
                                      `(,file))))))
         (unless projects
           (kill-buffer)
           (error "No projects can be deleted."))
         (let (wid)
           (dolist (project projects)
             (setq wid (widget-create 'checkbox
                                      :data project
                                      :format (concat "%[%v%] " project "\n"))
                   prj-widget-checkboxes (append prj-widget-checkboxes
                                                  `(,wid))))))))
    (:hide
     (and (get-buffer "*Delete Project*")
          (kill-buffer "*Delete Project*")))))

;;;###autoload
(defun prj-load-project-widget-frontend (command &optional ok)
  (case command
    (:show
     (let (choices)
       ;; Find available directories which represent a project.
       (dolist (name (directory-files prj-workspace-path))
         (unless (member name '("." ".."))
           (let ((config-file (prj-config-path name)))
             (when (and (file-exists-p config-file)
                        (not (string= name (prj-project-name))))
               (setq choices (append choices `(,name)))))))
       ;; Prompt user to create project if no projects is in workspace.
       (if choices
           ;; Prompt user to load project and call
           ;; `prj-load-project-internal'.
           (funcall ok
                    (ido-completing-read "Load project: " choices nil t))
         (message "No project in the workspace. Please create new project!"))))))

;;;###autoload
(defun prj-edit-project-widget-frontend (command &optional ok)
  (case command
    (:show
     (prj-with-widget "*Edit Project*"
       ;; Ok implementation callback ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ok

       ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (lambda (&rest ignore)
         (let ((doctypes (prj-widget-doctypes))
               (filepaths (prj-widget-filepaths)))
           (unless (> (length doctypes) 0)
             (error "No document types is selected!"))
           (unless (> (length filepaths) 0)
             (error "No valid file path!"))
           ;; call `prj-edit-project-internal'.
           (and prj-ok-func
                (funcall prj-ok-func
                         `(:doctypes ,doctypes :filepaths ,filepaths)))
           (kill-buffer)))

       ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (widget-insert (format "Project Name: %s\n\n"
                              (propertize (prj-project-name)
                                          'face 'widget-field)))
       (widget-insert "Document Types (Edit) ") ;; TODO: add customizable link
       (widget-create 'push-button
                      :notify (prj-widget-checkbox-select-all prj-widget-doctypes)
                      "Select All")
       (widget-insert " ")
       (widget-create 'push-button
                      :notify (prj-widget-checkbox-deselect-all prj-widget-doctypes)
                      "Deselect All")
       (widget-insert " :\n")
       ;; Widget for doctypes.
       (let (wid)
         (dolist (doctype prj-document-types)
           (setq wid (widget-create 'checkbox
                                    :data doctype
                                    :format (concat "%[%v%] "
                                                    (prj-format-doctype doctype)
                                                    "\n")
                                    :value (and (lax-plist-get (prj-project-doctypes)
                                                               (car doctype))
                                                t))
                 prj-widget-doctypes (append prj-widget-doctypes
                                             `(,wid)))))
       (widget-insert "\n")
       (widget-insert "Include Path:\n")
       ;; Widget for filepaths.
       (setq prj-widget-filepaths
             (widget-create 'editable-list
                            :entry-format "%i %d path: %v"
                            :value (copy-list (prj-project-filepaths))
                            '(editable-field :company prj-browse-file-backend)))))
    (:hide
     (and (get-buffer "*Edit Project*")
          (kill-buffer "*Edit Project*")))))

;;;###autoload
(defun prj-find-file-frontend (command &optional ok)
  (case command
    (:show
     (let* ((filedb (prj-import-data (prj-filedb-path)))
            filelist)
       (while filedb
         (setq filelist (append filelist (cadr filedb))
               filedb (cddr filedb)))
       ;; TOOD: use `sos-source-buffer' and new implementation.
       ;; call `prj-find-file-internal'.
       (funcall ok (ido-completing-read (format "[%s] Find file: "
                                                (prj-project-name))
                                        filelist))))))

;;;###autoload
(defun prj-search-project-widget-frontend (command &optional ok)
  (case command
    (:show
     (let ((thing (prj-thingatpt))
           (history (cadr (prj-project-search-history))))
       (prj-with-widget "*Search Project*"
         ;; Ok implementation callback ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ok

         ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (lambda (&rest ignore)
           (let ((match (widget-value prj-widget-textfield))
                 (doctypes (prj-widget-doctypes))
                 (filepaths nil)
                 (casefold (widget-value prj-widget-case-sensitive))
                 (word-only (widget-value prj-widget-word-only))
                 (skip-comment (widget-value prj-widget-skip-comment)))
             (unless (> (length match) 0)
               (error "Empty search!"))
             (unless (> (length doctypes) 0)
               (error "No document types is selected!"))
             ;; call `prj-search-project-internal-1'.
             (and prj-ok-func
                  (funcall prj-ok-func
                           `(:match ,match :doctypes ,doctypes
                                    :filepaths ,filepaths
                                    :casefold ,casefold
                                    :word-only ,word-only
                                    :skip-comment ,skip-comment)))
             (kill-buffer)))

         ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; Widget for search.
         (setq prj-widget-textfield
               (widget-create 'editable-field
                              :format "Search: %v"
                              :company 'prj-search-backend
                              ;; Get thing on the point, latest history or empty.
                              :value (or thing
                                         (and (stringp history)
                                              history)
                                         "")))
         (setq prj-widget-case-sensitive
               (widget-create 'checkbox
                              :format "%[%v%] Case Sensitive  "
                              :value t))
         (setq prj-widget-word-only
               (widget-create 'checkbox
                              :format "%[%v%] Match Whole Word  "))
         (setq prj-widget-skip-comment
               (widget-create 'checkbox
                              :format "%[%v%] Skip Comments\n"
                              :value t))
         (widget-insert "\n")
         (widget-insert "Document Types ")
         (widget-create 'push-button
                        :notify (prj-widget-checkbox-select-all prj-widget-doctypes)
                        "Select All")
         (widget-insert " ")
         (widget-create 'push-button
                        :notify (prj-widget-checkbox-deselect-all prj-widget-doctypes)
                        "Deselect All")
         (widget-insert " :\n")
         ;; Widget for doctypes.
         (let ((doctype (prj-project-doctypes))
               wid)
           (while doctype
             (setq wid (widget-create 'checkbox
                                      :data doctype
                                      :format (concat "%[%v%] "
                                                      (prj-format-doctype doctype)
                                                      "\n")
                                      :value t)
                   prj-widget-doctypes (append prj-widget-doctypes
                                               `(,wid))
                   doctype (cddr doctype))))
         (widget-insert "\n")
         (widget-insert "Search Path:\n")
         ;; Widget for filepaths.
         (dolist (path-root (prj-project-filepaths))
           (let ((path-root (propertize path-root
                                        'face 'font-lock-string-face))
                 wid)
             (when (file-directory-p path-root)
               (setq wid (widget-create 'editable-list
                                        :entry-format (concat "%i %d "
                                                              path-root
                                                              "%v")
                                        :value '("")
                                        '(editable-field
                                          :company prj-browse-file-backend))
                     prj-widget-filepaths (append prj-widget-filepaths
                                                  `(,wid)))))))))
    (:hide
     (and (get-buffer "*Search Project*")
          (kill-buffer "*Search Project*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro prj-with-widget (name ok-callback ok-notify &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (switch-to-buffer (get-buffer-create ,name))
     ;; TODO: fix compatibility with `company'.
     ;; Face
     (setq-local widget-image-enable nil)
     (setq-local widget-button-face 'prj-button-face)
     (setq-local widget-button-pressed-face 'prj-button-pressed-face)
     (setq-local widget-mouse-face 'prj-button-mouse-face)
     (setq-local widget-push-button-prefix " ")
     (setq-local widget-push-button-suffix " ")
     (setq prj-ok-func ,ok-callback
           header-line-format '((:eval (format "  %s"
                                               (propertize ,name
                                                           'face 'bold)))
                                " | "
                                (:eval (format "%s or %s to jump among widgets."
                                               (propertize " TAB "
                                                           'face 'tooltip)
                                               (propertize " Shift-TAB "
                                                           'face 'tooltip)))))
     (widget-insert "\n")
     ;; ==> body
     ,@body
     ;; <=======
     (widget-insert "\n")
     (widget-create 'push-button
                    :notify ,ok-notify
                    "ok")
     (widget-insert " ")
     (widget-create 'push-button
                    :notify (lambda (&rest ignore)
                              (kill-buffer))
                    "cancel")
     ;; Make some TAB do something before its original task.
     (add-hook 'widget-forward-hook 'prj-widget-forward-or-company t t)
     (use-local-map widget-keymap)
     (widget-setup)
     ;; Move point to 1st editable-field.
     (let ((field (car widget-field-list)))
       (when field
         (goto-char (widget-field-end field))))
     ;; Enable specific feature.
     (when (featurep 'company)
       (company-mode)
       (make-local-variable 'company-backends)
       (add-to-list 'company-backends 'prj-browse-file-backend)
       (add-to-list 'company-backends 'prj-search-backend))))

(defmacro prj-widget-checkbox-select-all (checkboxes)
  `(lambda (&rest ignore)
     (dolist (box ,checkboxes)
       (widget-value-set box t))))

(defmacro prj-widget-checkbox-deselect-all (checkboxes)
  `(lambda (&rest ignore)
     (dolist (box ,checkboxes)
       (widget-value-set box nil))))

(defun prj-format-doctype (doctype)
  (cond
   ;; alist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((stringp (cdr doctype))
    (format "%s (%s)" (car doctype) (cdr doctype)))
   ;; plist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (t
    (format "%s (%s)" (car doctype) (cadr doctype)))))

(defun prj-widget-doctypes ()
  "Return a plist of selected document types."
  (let (doctypes)
    (dolist (checkbox prj-widget-doctypes)
      (let ((doctype (and (widget-value checkbox)
                          (widget-get checkbox :data))))
        (when doctype
          (setq doctypes (append doctypes
                                 `(,(car doctype) ,(cdr doctype)))))))
    doctypes))

(defun prj-widget-filepaths ()
  "Return a list of file paths."
  (let (filepaths)
    (dolist (file (widget-value prj-widget-filepaths))
      (when (and (> (length file) 2)
                 (file-exists-p file))
        (setq file (expand-file-name file)
              ;; Add '/' postfix if it is a directory.
              file (if (and (file-directory-p file)
                            (not (string-match "\/$" file)))
                       (concat file "/")
                     file)
              filepaths (append filepaths `(,file)))))
    filepaths))

(defun prj-widget-checkboxes ()
  (let (ret)
    (dolist (checkbox prj-widget-checkboxes)
      (let ((data (and (widget-value checkbox)
                       (widget-get checkbox :data))))
        (when data
          (setq ret (append ret `(,data))))))
    ret))

(defun prj-thingatpt ()
  "Return a list, (REGEXP_STRING BEG END), on which the point is or just string of selection."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((bound (bounds-of-thing-at-point 'symbol)))
      (and bound
           (buffer-substring-no-properties (car bound) (cdr bound))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; back-ends for `company' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-browse-file-backend (command &optional arg &rest ign)
  "The backend based on `company' to provide convenience when browsing files."
  (case command
    (prefix
     (prj-common-prefix 'prj-browse-file-backend))
    (candidates
     (let* ((prefix arg)
            (dir (or (and (file-directory-p prefix)
                          prefix)
                     (file-name-directory prefix)))
            path candidates directories)
       (and dir
            (unless (equal dir (car prj-browse-file-cache))
              (dolist (file (prj-directory-files dir (prj-to-regexp prj-exclude-types)))
                (setq path (prj-concat-filepath dir file))
                (push path candidates)
                ;; Add one level of children.
                (when (file-directory-p path)
                  (push path directories)))
              (dolist (directory (reverse directories))
                (ignore-errors
                  (dolist (child (prj-directory-files directory (prj-to-regexp prj-exclude-types)))
                    (setq path (prj-concat-filepath directory child))
                    (push path candidates))))
              (setq prj-browse-file-cache (cons dir (nreverse candidates)))))
       (all-completions prefix
                        (cdr prj-browse-file-cache))))
    (ignore-case t)))

;;;###autoload
(defun prj-search-backend (command &optional arg &rest ign)
  "Following are for `company' when searching project."
  (case command
    (prefix
     (prj-common-prefix 'prj-search-backend))
    (candidates
     (all-completions arg (prj-search-cache)))
    (ignore-case t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar prj-browse-file-cache nil
  "Storing (DIR . (CANDIDATES...)) for completion.")
(make-variable-buffer-local 'prj-browse-file-cache)

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

(defun prj-to-regexp (wildcardexp)
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

(defun prj-widget-forward-or-company ()
  "It is for `widget-forward-hook' to continue forward to next widget or show company prompt."
  (interactive)
  (and (or (prj-common-prefix 'prj-browse-file-backend)
           (prj-common-prefix 'prj-search-backend))
       (company-complete)
       (top-level)))

(defun prj-common-prefix (backend)
  "The function responds 'prefix for `prj-browse-file-backend'. Return nil means skip this backend function; Any string means there're candidates should be prompt. Only the editable-field with :file-browse property is allowed."
  (let* ((field (widget-field-at (point)))
         (field-backend (and field
                             (widget-get field :company))))
    (if (and field field-backend
             (eq field-backend backend))
        (let* ((start (widget-field-start field))
               (end (widget-field-end field))
               (prefix (buffer-substring-no-properties start end)))
          prefix)
      nil)))

(provide 'prj-widget-frontend)
