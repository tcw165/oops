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

;; 3rd party library.
(require 'grizzl)

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

(defvar prj-widget-keymap
  (let ((map (copy-keymap (default-value 'widget-keymap))))
    (define-key map "\t" '(lambda () (interactive) (prj-widget-forward-or-company)))
    (define-key map [tab] '(lambda () (interactive) (prj-widget-forward-or-company)))
    (define-key map [escape] '(lambda () (interactive) (kill-buffer)))
    map))

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

(defmacro prj-with-widget (name ok-notify &rest body)
  (declare (indent 1) (debug t))
  ;; (require 'widget)
  (require 'wid-edit)
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
     (setq header-line-format `(,(format "  %s | TAB or Shift-TAB to jump among widgets."
                                         (propertize ,name 'face 'bold))))
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
     (use-local-map prj-widget-keymap)
     (widget-setup)
     ;; Move point to 1st editable-field.
     (goto-char (or (and (car widget-field-list)
                         (widget-field-end (car widget-field-list)))
                    (point)))
     ;; Enable specific feature.
     (when (require 'company)
       (setq-local company-frontends '(company-preview-frontend))
       (setq-local company-backends '(company-browse-file-backend))
       (company-mode 1))))

(defmacro prj-widget-checkbox-select-all (checkboxes)
  `(lambda (&rest ignore)
     (dolist (box ,checkboxes)
       (widget-value-set box t))))

(defmacro prj-widget-checkbox-deselect-all (checkboxes)
  `(lambda (&rest ignore)
     (dolist (box ,checkboxes)
       (widget-value-set box nil))))

(defun prj-thingatpt ()
  "Return a list, (REGEXP_STRING BEG END), on which the point is or just string of selection."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((bound (bounds-of-thing-at-point 'symbol)))
      (and bound
           (buffer-substring-no-properties (car bound) (cdr bound))))))

(defun prj-format-doctype (doctype)
  (format "%s (%s)"
          doctype
          (cdr (assoc doctype prj-document-types))))

(defun prj-widget-doctypes ()
  "Return a plist of selected document types."
  (let (doctypes)
    (dolist (checkbox prj-widget-doctypes)
      (let ((doctype (and (widget-value checkbox)
                          (widget-get checkbox :data))))
        (when doctype
          (setq doctypes (append doctypes `(,doctype))))))
    doctypes))

(defun prj-widget-filepaths ()
  "Return a list of file paths."
  (let (filepaths)
    (dolist (file (widget-value prj-widget-filepaths))
      (when (and (not (string= file ""))
                 (file-exists-p file))
        (setq file (expand-file-name file)
              ;; Remove '/' postfix if any.
              file (or (and (string-match "\\(.*\\)\/$" file)
                            (match-string 1 file))
                       file)
              ;; Add '/' postfix if it is a directory.
              ;; file (if (and (file-directory-p file)
              ;;               (not (string-match "\/$" file)))
              ;;          (concat file "/")
              ;;        file)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Front-Ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-create-project-frontend (command &rest args)
  (case command
    (:init)
    (:destroy)
    (:create-project
     (prj-with-widget "*Create Project*"
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
           (kill-buffer)
           (funcall 'prj-create-project-impl name doctypes filepaths)))

       ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Widget for project name.
       (setq prj-widget-textfield (widget-create 'editable-field
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
           (let ((doctype (car doctype)))
             (setq wid (widget-create 'checkbox
                                      :data doctype
                                      :format (concat "%[%v%] "
                                                      (prj-format-doctype doctype)
                                                      "\n"))
                   prj-widget-doctypes (append prj-widget-doctypes `(,wid))))))
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
                            '(editable-field :company company-browse-file-backend)))))
    (:hide
     (and (get-buffer "*Create Project*")
          (kill-buffer "*Create Project*")))))

;;;###autoload
(defun prj-delete-project-frontend (command &rest args)
  (case command
    (:init)
    (:destroy)
    (:delete-project
     (prj-with-widget "*Delete Project*"
       ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (lambda (&rest ignore)
         (let ((projects (prj-widget-checkboxes)))
           (unless (> (length projects) 0)
             (error "No project is selected!"))
           (kill-buffer)
           (funcall 'prj-delete-project-impl projects)))

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
(defun prj-load-project-frontend (command &optional projects)
  (case command
    (:init)
    (:destroy)
    (:load-project
     (if projects
         ;; Prompt user to load project and return project name.
         (funcall 'prj-load-project-impl
                  (ido-completing-read "Load project: " projects nil t))
       (if (prj-project-p)
           (message "[%s] No other project in the workspace!" (prj-project-name))
         (message "No project in the workspace. Please create new project!"))))))

;;;###autoload
(defun prj-edit-project-frontend (command &rest args)
  (case command
    (:init)
    (:destroy)
    (:edit-project
     (prj-with-widget "*Edit Project*"
       ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (lambda (&rest ignore)
         (let ((doctypes (prj-widget-doctypes))
               (filepaths (prj-widget-filepaths)))
           (unless (> (length doctypes) 0)
             (error "No document types is selected!"))
           (unless (> (length filepaths) 0)
             (error "No valid file path!"))
           (kill-buffer)
           (funcall 'prj-edit-project-impl doctypes filepaths)))

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
           (let ((doctype (car doctype)))
             (setq wid (widget-create 'checkbox
                                      :data doctype
                                      :format (concat "%[%v%] "
                                                      (prj-format-doctype doctype)
                                                      "\n")
                                      :value (member doctype (prj-project-doctypes)))
                   prj-widget-doctypes (append prj-widget-doctypes `(,wid))))))
       (widget-insert "\n")
       (widget-insert "Include Path:\n")
       ;; Widget for filepaths.
       (setq prj-widget-filepaths
             (widget-create 'editable-list
                            :entry-format "%i %d path: %v"
                            :value (copy-list (prj-project-filepaths))
                            '(editable-field :company company-browse-file-backend)))))
    (:hide
     (and (get-buffer "*Edit Project*")
          (kill-buffer "*Edit Project*")))))

;;;###autoload
(defun prj-find-file-frontend (command &optional files)
  (case command
    (:init)
    (:destroy)
    (:find-files
     (let ((file (grizzl-completing-read-database
                  (format "[%s] find file" (prj-project-name))
                  files)))
       (when file
         (funcall 'prj-find-file-impl file))))))

;;;###autoload
(defun prj-search-project-frontend (command &rest args)
  (case command
    (:init)
    (:destroy)
    (:search-project
     (let ((thing (prj-thingatpt))
           ;; (history (car (prj-project-search-history)))
           history)
       (prj-with-widget "*Search Project*"
         ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (lambda (&rest ignore)
           (let ((match (widget-value prj-widget-textfield))
                 (doctypes (prj-widget-doctypes))
                 (filepaths (prj-widget-filepaths))
                 (case-sensitive (widget-value prj-widget-case-sensitive))
                 (word-only (widget-value prj-widget-word-only)))
             (unless (> (length match) 0)
               (error "Empty search!"))
             (unless (> (length doctypes) 0)
               (error "No document types is selected!"))
             ;; Update search history.
             ;; (let ((history (prj-project-search-history)))
             ;;   (if (member match history)
             ;;       ;; Reorder the match.
             ;;       (setq history (delete match history)
             ;;             history (push match history))
             ;;     (setq history (push match history)))
             ;;   ;; Keep maximum history.
             ;;   (and (> (length history) prj-search-history-max)
             ;;        (setcdr (nthcdr (1- prj-search-history-max) history) nil))
             ;;   (prj-plist-put prj-config :search-history history)
             ;;   (prj-export-config))
             (kill-buffer)
             (funcall 'prj-search-project-impl
                      match doctypes filepaths case-sensitive word-only)))

         ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; Widget for search.
         (setq prj-widget-textfield
               (widget-create 'editable-field
                              :format "Search: %v"
                              ;; Get thing on the point, latest history or empty.
                              :value (or thing
                                         (and (stringp history)
                                              history)
                                         "")))
         (setq prj-widget-case-sensitive
               (widget-create 'checkbox
                              :format "%[%v%] Case Sensitive  "
                              :value nil))
         (setq prj-widget-word-only
               (widget-create 'checkbox
                              :format "%[%v%] Match Whole Word  "))
         (widget-insert "\n\n")
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
         (let (wid)
           (dolist (doctype (prj-project-doctypes))
             (setq wid (widget-create 'checkbox
                                      :data doctype
                                      :format (concat "%[%v%] "
                                                      (prj-format-doctype doctype)
                                                      "\n")
                                      :value t)
                   prj-widget-doctypes (append prj-widget-doctypes `(,wid)))))
         ;; Widget for filepaths.
         (widget-insert "\n")
         (widget-insert "Search Path:\n")
         (dolist (path-root (prj-project-filepaths))
           (let ((path-root (abbreviate-file-name path-root)))
             (when (file-exists-p path-root)
               (setq company-file-prefix (append company-file-prefix
                                                 `(,path-root))))))
         (setq prj-widget-filepaths
               (widget-create 'editable-list
                              :entry-format "%i %d path: %v"
                              :value '("")
                              '(editable-field :company company-browse-file-backend))))))
    (:hide
     (and (get-buffer "*Search Project*")
          (kill-buffer "*Search Project*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; back-ends for `company' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-file-prefix nil
  "Storing (DIR . (CANDIDATES...)) for completion.")
(make-variable-buffer-local 'company-file-prefix)

(defun company-directory-files (dir)
  "Return a list containing file names under `dir' but exlcudes files that match `exclude'."
  (let (files)
    (ignore-errors
      (dolist (file (directory-files dir))
        (and (not (member file '("." "..")))
             (not (string-match "\\(\.git\\|\.svn\\)" file))
             (setq file (concat dir
                                (unless (eq (aref dir (1- (length dir))) ?/) "/")
                                file)
                   files (append files `(,file))))))
    files))

(defun company-widget-p ()
  (let* ((field (widget-field-at (point)))
         (field-backend (and field (widget-get field :company))))
    (and field field-backend
         (eq field-backend 'company-browse-file-backend))))

(defun prj-widget-forward-or-company ()
  "It is for `widget-forward-hook' to continue forward to next widget or show company prompt."
  (if (company-widget-p)
      (company-complete)
    (widget-forward 1)))

;;;###autoload
(defun company-browse-file-backend (command &rest args)
  "The backend based on `company' to provide convenience when browsing files."
  (case command
    (prefix
     (when (company-widget-p)
       (let* ((field (widget-field-at (point)))
              (start (widget-field-start field))
              (end (widget-field-end field)))
         (buffer-substring-no-properties start end))))
    (candidates
     (let* ((prefix (car args))
            (prefixs (or company-file-prefix
                         (and (string= prefix "") '("~/"))
                         `(,prefix)))
            candidates-all)
       (dolist (prefix prefixs)
         (let ((dir (or (and (file-directory-p prefix)
                             prefix)
                        (file-name-directory prefix)))
               candidates)
           (when dir
             ;; (company-directory-files "~/")
             (dolist (file (company-directory-files dir))
               (setq candidates (append candidates `(,file)))
               ;; Add 2nd level of children.
               (when (file-directory-p file)
                 (dolist (file2 (company-directory-files file))
                   (setq candidates (append candidates `(,file2))))))
             (setq candidates (all-completions prefix candidates)
                   candidates-all (append candidates-all candidates)))))
       candidates-all))
    (ignore-case t)))

(provide 'prj-default-frontend)
