;; Copyright (C) 2014, 2015
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
;; (require 'grizzl)

(require 'widget)
(require 'wid-edit)

(defface prj-gui-button-face
  '((t (:background "lightgrey" :foreground "black" :weight bold)))
  "Face for button."
  :group 'prj)

(defface prj-gui-button-mouse-face
  '((t (:background "grey90" :foreground "black" :weight bold)))
  "Face for mouse button (when mouse is over the button)."
  :group 'prj)

(defface prj-gui-button-pressed-face
  '((t (:background "cyan3" :foreground "black" :weight bold)))
  "Face for pressed button."
  :group 'prj)

(defvar prj-gui-widget-keymap
  (let ((map (copy-keymap (default-value 'widget-keymap))))
    (define-key map "\t" '(lambda () (interactive) (prj-widget-forward-or-company)))
    (define-key map [tab] '(lambda () (interactive) (prj-widget-forward-or-company)))
    (define-key map [escape] '(lambda () (interactive) (kill-buffer)))
    map))

(defvar prj-gui-widget-textfield nil
  "Storage for textfield widgets.")

(defvar prj-gui-widget-checkboxes nil
  "Storage for checkbox widgets.")

(defvar prj-gui-widget-filepaths nil
  "Storage for filepath widgets.")

(defvar prj-gui-widget-doctypes nil
  "Storage for doctypes widgets.")

(defmacro prj-gui-with-widget (name ok-impl &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (prj-gui-kill-old-session)
     (switch-to-buffer (get-buffer-create ,name))
     ;; TODO: fix compatibility with `company'.
     ;; Face
     (setq-local widget-image-enable nil)
     (setq-local widget-button-face 'prj-gui-button-face)
     (setq-local widget-button-pressed-face 'prj-gui-button-pressed-face)
     (setq-local widget-mouse-face 'prj-gui-button-mouse-face)
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
                    :notify ,ok-impl
                    "ok")
     (widget-insert " ")
     (widget-create 'push-button
                    :notify (lambda (&rest ignore)
                              (kill-buffer))
                    "cancel")
     (use-local-map prj-gui-widget-keymap)
     (widget-setup)
     ;; Move point to 1st editable-field or checkbox.
     (goto-char (or (and (car widget-field-list)
                         (widget-field-end (car widget-field-list)))
                    (point)))
     ;; Enable specific feature.
     (when (require 'company)
       (setq-local company-frontends '(company-preview-frontend))
       (setq-local company-backends '(company-browse-file-backend))
       (company-mode 1))))

(defmacro prj-gui-widget-checkbox-select-all (checkboxes)
  `(lambda (&rest ignore)
     (dolist (box ,checkboxes)
       (widget-value-set box t))))

(defmacro prj-gui-widget-checkbox-deselect-all (checkboxes)
  `(lambda (&rest ignore)
     (dolist (box ,checkboxes)
       (widget-value-set box nil))))

(defun prj-gui-format-doctype (doctype)
  (format "%s (%s)"
          doctype
          (cdr (assoc doctype prj-document-types))))

(defun prj-gui-widget-doctypes ()
  "Return a plist of selected document types."
  (let (doctypes)
    (dolist (checkbox prj-gui-widget-doctypes)
      (let ((doctype (and (widget-value checkbox)
                          (widget-get checkbox :data))))
        (when doctype
          (setq doctypes (append doctypes `(,doctype))))))
    doctypes))

(defun prj-gui-widget-filepaths ()
  "Return a list of file paths."
  (let (filepaths)
    (dolist (file (widget-value prj-gui-widget-filepaths))
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

(defun prj-gui-widget-checkboxes ()
  (let (ret)
    (dolist (checkbox prj-gui-widget-checkboxes)
      (let ((data (and (widget-value checkbox)
                       (widget-get checkbox :data))))
        (when data
          (setq ret (append ret `(,data))))))
    ret))

(defun prj-gui-kill-old-session ()
  ;; Kill old GUIs.
  (mapc
   (lambda (bname)
     (let ((buf (get-buffer bname)))
       (and buf (kill-buffer buf))))
   '("*Create Project*"
     "*Delete Project*"
     "*Edit Project*"
     "*Search Project*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-gui-new-project (impl)
  (prj-gui-with-widget "*Create Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (eval `(lambda (&rest ignore)
             (let ((name (widget-value prj-gui-widget-textfield))
                   (doctypes (prj-gui-widget-doctypes))
                   (filepaths (prj-gui-widget-filepaths)))
               (unless (> (length name) 0)
                 (error "Project name is empty!"))
               (unless (> (length doctypes) 0)
                 (error "No document types is selected!"))
               (unless (> (length filepaths) 0)
                 (error "No valid file path!"))
               (kill-buffer)
               (funcall ',impl name doctypes filepaths))))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Widget for project name.
    (setq prj-gui-widget-textfield (widget-create 'editable-field
                                                  :format "Project Name: %v"))
    (widget-insert "\n")
    (widget-insert "Document Types (Edit) ") ;; TODO: add customizable link
    (widget-create 'push-button
                   :notify (prj-gui-widget-checkbox-select-all prj-gui-widget-doctypes)
                   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (prj-gui-widget-checkbox-deselect-all prj-gui-widget-doctypes)
                   "Deselect All")
    (widget-insert " :\n")
    ;; Widget for doctypes.
    (let (wid)
      (dolist (doctype prj-document-types)
        (let ((doctype (car doctype)))
          (setq wid (widget-create 'checkbox
                                   :data doctype
                                   :format (concat "%[%v%] "
                                                   (prj-gui-format-doctype doctype)
                                                   "\n"))
                prj-gui-widget-doctypes (append prj-gui-widget-doctypes `(,wid))))))
    (widget-insert "\n")
    (widget-insert "Include Path:\n")
    (widget-insert (propertize "- Button INS to add a path; Button DEL to \
remove one.\n"
                               'face 'font-lock-string-face))
    (widget-insert (propertize "- Use TAB to get a path prompt.\n"
                               'face 'font-lock-string-face))
    ;; Widget for filepaths.
    (setq prj-gui-widget-filepaths
          (widget-create 'editable-list
                         :entry-format "%i %d path: %v"
                         :value '("")
                         '(editable-field :company company-browse-file-backend)))))

(defun prj-gui-delete-project (impl)
  (prj-gui-with-widget "*Delete Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (eval `(lambda (&rest ignore)
             (let ((names (prj-gui-widget-checkboxes)))
               (unless (> (length names) 0)
                 (error "No project is selected!"))
               (kill-buffer)
               (funcall ',impl names))))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (widget-insert "Delete project ")
    (widget-create 'push-button
                   :notify (prj-gui-widget-checkbox-select-all prj-gui-widget-checkboxes)
                   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (prj-gui-widget-checkbox-deselect-all prj-gui-widget-checkboxes)
                   "Deselect All")
    (widget-insert " :\n")
    ;; Widget for project name.
    (let (projects)
      ;; Find out all the projects.
      (dolist (file (directory-files prj-workspace-path))
        (let ((config (prj-project-config-path file)))
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
                prj-gui-widget-checkboxes (append prj-gui-widget-checkboxes
                                                  `(,wid))))))))

(defun prj-gui-edit-project (impl)
  (prj-gui-with-widget "*Edit Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (eval `(lambda (&rest ignore)
             (let ((doctypes (prj-gui-widget-doctypes))
                   (filepaths (prj-gui-widget-filepaths)))
               (unless (> (length doctypes) 0)
                 (error "No document types is selected!"))
               (unless (> (length filepaths) 0)
                 (error "No valid file path!"))
               (kill-buffer)
               (funcall ',impl doctypes filepaths))))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (widget-insert (format "Project Name: %s (total %s files)\n\n"
                           prj-project-name
                           prj-files-number))
    ;; TODO: add customizable link
    (widget-insert "Document Types")
    (widget-create 'push-button
                   :notify (prj-gui-widget-checkbox-select-all prj-gui-widget-doctypes)
                   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify (prj-gui-widget-checkbox-deselect-all prj-gui-widget-doctypes)
                   "Deselect All")
    (widget-insert " :\n")
    ;; Widget for doctypes.
    (let (wid)
      (dolist (doctype prj-document-types)
        (let ((doctype (car doctype)))
          (setq wid (widget-create 'checkbox
                                   :data doctype
                                   :format (concat "%[%v%] "
                                                   (prj-gui-format-doctype doctype)
                                                   "\n")
                                   :value (member doctype (prj-project-doctypes)))
                prj-gui-widget-doctypes (append prj-gui-widget-doctypes `(,wid))))))
    (widget-insert "\n")
    (widget-insert "Include Path:\n")
    ;; Widget for filepaths.
    (setq prj-gui-widget-filepaths
          (widget-create 'editable-list
                         :entry-format "%i %d path: %v"
                         :value (copy-list (prj-project-filepaths))
                         '(editable-field :company company-browse-file-backend)))))

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

(provide 'prj-gui)
