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

(defvar prj2-ok-func nil)
(make-variable-buffer-local 'prj2-ok-func)

(defvar prj2-widget-project-name nil)
(make-variable-buffer-local 'prj2-widget-project-name)

(defvar prj2-widget-filepaths nil)
(make-variable-buffer-local 'prj2-widget-filepaths)

(defvar prj2-widget-doctypes nil)
(make-variable-buffer-local 'prj2-widget-doctypes)

;; (defface prj2-title-face
;;   '((t (:background "yellow" :foreground "black" :weight bold :height 2.0)))
;;   "Default face for highlighting keyword in definition window."
;;   :group 'prj2-group)

;;;###autoload
(defun prj2-create-project-widget-frontend (command &optional ok)
  (case command
    (:show
     (prj2-with-widget "*Create Project*"
       ;; Ok callback ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ok

       ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (lambda (&rest ignore)
         ;; Validate file paths.
         (let ((name (widget-value prj2-widget-project-name))
               (doctypes (prj2-widget-doctypes))
               (filepaths (prj2-widget-filepaths)))
           (unless (> (length name) 0)
             (error "Project name is empty!"))
           (unless (> (length doctypes) 0)
             (error "No document types is selected!"))
           (unless (> (length filepaths) 0)
             (error "No file path!"))
           (and prj2-ok-func
                (funcall prj2-ok-func name doctypes filepaths))
           (kill-buffer)))

       ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Widget for project name.
       (setq prj2-widget-project-name
             (widget-create 'editable-field
                            :format "Project Name: %v"))
       (widget-insert "\n")
       (widget-insert "Document Types (Edit) ") ;; TODO: add customizable link
       (widget-create 'push-button
                      :notify (prj2-widget-checkbox-select-all prj2-widget-doctypes)
                      "Select All")
       (widget-insert " ")
       (widget-create 'push-button
                      :notify (prj2-widget-checkbox-deselect-all prj2-widget-doctypes)
                      "Deselect All")
       (widget-insert " :\n")
       (let (wid)
         (dolist (doctype prj2-document-types)
           (setq wid (widget-create 'checkbox
                                    :format (concat "%[%v%] "
                                                    (prj2-format-doctype doctype)
                                                    "\n")))
           (widget-put wid :data doctype)
           ;; Widget for doctypes.
           (setq prj2-widget-doctypes (append prj2-widget-doctypes
                                              `(,wid)))))
       (widget-insert "\n")
       (widget-insert "Include Path:\n")
       (widget-insert (propertize "- Button INS to add a path; Button DEL to \
remove one.\n"
                                  'face 'font-lock-string-face))
       (widget-insert (propertize "- Use TAB to get a path prompt.\n"
                                  'face 'font-lock-string-face))
       ;; Widget for filepaths.
       (setq prj2-widget-filepaths
             (widget-create 'editable-list
                            :entry-format "%i %d %v"
                            :value '("")
                            ;; Put :company to make company work for it.
                            '(editable-field :value "" :company prj2-browse-file-backend)))))
    (:hide
     (and (get-buffer "*Create Project*")
          (kill-buffer "*Create Project*")))))

;;;###autoload
(defun prj2-delete-project-widget-frontend (command &optional ok)
  (case command
    (:show
     )
    (:hide
     (and (get-buffer "*Delete Project*")
          (kill-buffer "*Delete Project*")))))

;;;###autoload
(defun prj2-edit-project-widget-frontend (command &optional ok)
  (case command
    (:show
     )
    (:hide
     (and (get-buffer "*Edit Project*")
          (kill-buffer "*Edit Project*")))))

;;;###autoload
(defun prj2-search-project-widget-frontend (command &optional ok)
  (case command
    (:show
     )
    (:hide
     (and (get-buffer "*Search Project*")
          (kill-buffer "*Search Project*")))))

;;;###autoload
(defun prj2-find-file-frontend (command &optional ok)
  (case command
    (:show
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; back-ends for `company' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj2-browse-file-backend (command &optional arg &rest ign)
  "The backend based on `company' to provide convenience when browsing files."
  (case command
    (prefix (prj2-common-prefix 'prj2-browse-file-backend))
    (candidates (prj2-browse-file-complete arg))
    (ignore-case t)))

;;;###autoload
(defun prj2-search-backend (command &optional arg &rest ign)
  "Following are for `company' when searching project."
  (case command
    (prefix (prj2-common-prefix 'prj2-search-backend))
    (candidates (prj2-search-complete arg))
    (ignore-case t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro prj2-widget-checkbox-select-all (checkboxes)
  `(lambda (&rest ignore)
     (dolist (box ,checkboxes)
       (widget-value-set box t))))

(defmacro prj2-widget-checkbox-deselect-all (checkboxes)
  `(lambda (&rest ignore)
     (dolist (box ,checkboxes)
       (widget-value-set box nil))))

(defmacro prj2-with-widget (name ok-callback ok-notify &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (switch-to-buffer (get-buffer-create ,name))
     ;; TODO: fix compatibility with `company'.
     ;; Face
     ;; (setq-local widget-button-face custom-button)
     ;; (setq-local widget-button-pressed-face custom-button-pressed)
     ;; (setq-local widget-mouse-face custom-button-mouse)
     ;; When possible, use relief for buttons, not bracketing.
     ;; (when custom-raised-buttons
     ;;   (setq-local widget-push-button-prefix " ")
     ;;   (setq-local widget-push-button-suffix " ")
     ;;   (setq-local widget-link-prefix "")
     ;;   (setq-local widget-link-suffix ""))
     (setq prj2-ok-func ,ok-callback
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
     (add-hook 'widget-forward-hook 'prj2-widget-forward-or-company t t)
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
       (add-to-list 'company-backends 'prj2-browse-file-backend)
       (add-to-list 'company-backends 'prj2-search-backend))))

(defun prj2-widget-doctypes ()
  "Return a plist of selected document types."
  (let (doctypes)
    (dolist (checkbox prj2-widget-doctypes)
      (let ((doctype (and (widget-value checkbox)
                          (widget-get checkbox :data))))
        (when doctype
          (setq doctypes (append doctypes
                                 `(,(car doctype) ,(cdr doctype)))))))
    doctypes))

(defun prj2-widget-filepaths ()
  "Return a list of file paths."
  (let (filepaths)
    (dolist (file (widget-value prj2-widget-filepaths))
      (when (and (> (length file) 2)
                 (file-exists-p file))
        (setq filepaths (append filepaths
                                `(,(expand-file-name file))))))
    filepaths))

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

(defun prj2-to-regexp (wildcardexp)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar prj2-browse-file-cache nil
  "Storing (DIR . (CANDIDATES...)) for completion.")
(make-variable-buffer-local 'prj2-browse-file-cache)

(defun prj2-widget-forward-or-company ()
  "It is for `widget-forward-hook' to continue forward to next widget or show company prompt."
  (interactive)
  (and (or (prj2-common-prefix 'prj2-browse-file-backend)
           (prj2-common-prefix 'prj2-search-backend))
       (company-complete)
       (top-level)))

(defun prj2-common-prefix (backend)
  "The function responds 'prefix for `prj2-browse-file-backend'. Return nil means skip this backend function; Any string means there're candidates should be prompt. Only the editable-field with :file-browse property is allowed."
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

(defun prj2-browse-file-complete (prefix)
  "The function responds 'candiates for `prj2-browse-file-backend'."
  (let* ((dir (or (and (file-directory-p prefix)
                       prefix)
                  (file-name-directory prefix)))
         path
         candidates
         directories)
    (and dir
         (unless (equal dir (car prj2-browse-file-cache))
           (dolist (file (prj2-directory-files dir (prj2-to-regexp prj2-exclude-types)))
             (setq path (prj2-concat-filepath dir file))
             (push path candidates)
             ;; Add one level of children.
             (when (file-directory-p path)
               (push path directories)))
           (dolist (directory (reverse directories))
             (ignore-errors
               (dolist (child (prj2-directory-files directory (prj2-to-regexp prj2-exclude-types)))
                 (setq path (prj2-concat-filepath directory child))
                 (push path candidates))))
           (setq prj2-browse-file-cache (cons dir (nreverse candidates)))))
    (all-completions prefix
                     (cdr prj2-browse-file-cache))))

(defun prj2-search-complete (prefix)
  "The function responds 'candiates for `prj2-search-backend'."
  ;; TODO: use `prj2-search-cache'.
  (all-completions prefix (prj2-search-cache)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj2-format-doctype (doctype)
  (format "%s (%s)" (car doctype) (cdr doctype)))

(defun prj2-widget-clean ()
  (dolist (name '("*Create Project*"
                  "*Edit Project*"
                  "*Delete Project*"
                  "*Search Project*"))
    (let ((search (get-buffer name)))
      (and search
           (kill-buffer search)))))

(defun prj2-setup-delete-project-widget ()
  (prj2-with-widget "*Delete Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (lambda (&rest ignore)
      (prj2-delete-project-internal prj2-tmp-list1)
      (kill-buffer))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (widget-insert "=== Delete Project ===\n")
    (widget-insert "Use TAB to jump to forward widget; Shift-Tab to jump to backward widget.\n\n")
    (widget-insert "Select projects to be deleted:\n")
    (let (choices)
      (dolist (f (directory-files prj2-workspace-path))
	(let ((config-file (format "%s/%s/%s" prj2-workspace-path f prj2-config-name)))
	  (when (file-exists-p config-file)
	    (push f choices))))
      (unless choices
	(kill-buffer)
	(error "[Prj] No projects can be deleted."))
      (dolist (c choices)
	(widget-put (widget-create 'checkbox
				   :format (concat "%[%v%] " c "\n")
				   :notify (prj2-widget-checkbox-notify :filepaths prj2-tmp-list1))
		    :filepaths c)))))

(defun prj2-setup-edit-project-widget ()
  (prj2-with-widget "*Edit Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (lambda (&rest ignore)
      ;; Sort the order of doctypes.
      (prj2-sort-doctypes prj2-tmp-list1 prj2-document-types)
      ;; Validate file paths.
      (prj2-validate-filepaths prj2-widget-filepaths)
      (let (updatefiledb)
	;; Check if update is necessary.
	(catch 'loop
	  (unless (and (= (length prj2-tmp-list1) (length (prj2-project-doctypes)))
		       (= (length prj2-widget-filepaths) (length (prj2-project-filepaths))))
	    (setq updatefiledb t)
	    (throw 'loop t))
	  (dolist (elm prj2-tmp-list1)
	    (unless (member elm (prj2-project-doctypes))
	      (setq updatefiledb t)
	      (throw 'loop t)))
	  (dolist (elm prj2-widget-filepaths)
	    (unless (member elm (prj2-project-filepaths))
	      (setq updatefiledb t)
	      (throw 'loop t))))
	;; Export configuration.
	(prj2-edit-project-internal prj2-tmp-list1 prj2-widget-filepaths updatefiledb)
	(kill-buffer)))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq prj2-tmp-list1 (copy-list (prj2-project-doctypes)))
    (setq prj2-widget-filepaths (copy-list (prj2-project-filepaths)))

    (widget-insert "=== Edit Project ===\n")
    (widget-insert "Use TAB to jump to forward widget; Shift-Tab to jump to backward widget.\n\n")
    (widget-insert (format "Project Name: %s\n\n" (prj2-widget-project-name)))
    (widget-insert "Document Types (Edit) ") ;; TODO: add customizable link
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-widget-doctypes)
			       (widget-value-set box t))
			     (dolist (type prj2-document-types)
			       (push type prj2-tmp-list1)))
		   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-widget-doctypes)
			       (widget-value-set box nil)))
		   "Deselect All")
    (widget-insert " :\n")
    (dolist (type prj2-document-types)
      (let (wid)
	(setq wid (widget-create 'checkbox
				 :format (concat "%[%v%] " (prj2-format-doctype type) "\n")
				 :value  (catch 'loop
					   (dolist (currtype (prj2-project-doctypes))
					     (and (equal (car currtype) (car type))
						  (throw 'loop t))))
				 :notify (prj2-widget-checkbox-notify :doctype prj2-tmp-list1)))
	(widget-put wid :doctype type)
	(push wid prj2-widget-doctypes)))
    (widget-insert "\n")
    (widget-insert "Include Path:\n")
    (widget-create 'editable-list
		   :entry-format "%i %d %v"
		   :value (prj2-project-filepaths)
		   :notify (prj2-widget-common-notify prj2-widget-filepaths)
		   '(editable-field :company prj2-browse-file-backend))))

(defun prj2-setup-search-project-widget (search)
  (prj2-with-widget "*Search Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (lambda (&rest ignore)
      (unless prj2-widget-project-name
	(error (format "[%s] Please enter something for searching!" (prj2-widget-project-name))))
      (unless (> (length prj2-tmp-list1) 0)
	(error (format "[%s] Please select document types for searching!" (prj2-widget-project-name))))
      (kill-buffer)
      (prj2-search-project-internal prj2-widget-project-name prj2-tmp-list1))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq prj2-tmp-list1 (copy-list (prj2-project-doctypes)))
    (and search
         (setq prj2-widget-project-name search))

    (widget-insert "=== Search Project ===\n\n")
    (widget-insert (format "Project Name: %s\n\n" (prj2-widget-project-name)))
    (widget-create 'editable-field
		   :format "Search: %v"
                   :value (or search "")
		   :notify (prj2-widget-common-notify prj2-widget-project-name)
                   :company 'prj2-search-backend)
    (widget-insert "\n")
    (widget-insert "Document Types ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-widget-doctypes)
			       (widget-value-set box t))
			     (dolist (type (prj2-project-doctypes))
			       (push type prj2-tmp-list1)))
		   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-widget-doctypes)
			       (widget-value-set box nil)))
		   "Deselect All")
    (widget-insert " :\n")
    (dolist (type (prj2-project-doctypes))
      (let (wid)
	(setq wid (widget-create 'checkbox
				 :format (concat "%[%v%] " (prj2-format-doctype type) "\n")
				 :value t
				 :notify (prj2-widget-checkbox-notify :doctypes prj2-tmp-list1)))
	(widget-put wid :doctypes type)
	(push wid prj2-widget-doctypes)))))

(provide 'prj2-widget-frontend)
