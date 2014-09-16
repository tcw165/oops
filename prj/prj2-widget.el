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
(require 'company)
(require 'company-files)

(defvar prj2-tmp-string nil)
(defvar prj2-tmp-list1 nil)
(defvar prj2-tmp-list2 nil)
(defvar prj2-tmp-list3 nil)

;;;###autoload
(defun prj2-create-project-widget-frontend ()
  )

;;;###autoload
(defun prj2-delete-project-widget-frontend ()
  )

;;;###autoload
(defun prj2-edit-project-widget-frontend ()
  )

;;;###autoload
(defun prj2-search-project-widget-frontend ()
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro prj2-widget-common-notify (place)
  "(Widget) Notify function for 'editable-field and 'editable-list."
  `(lambda (wid &rest ignore)
     (setq ,place (widget-value wid))))

(defmacro prj2-widget-checkbox-notify (prop place)
  "(Widget) Notify function for 'checkbox."
  `(lambda (wid &rest ignore)
     (if (widget-value wid)
	 (push (widget-get wid ,prop) ,place)
       (setq ,place (delq (widget-get wid ,prop) ,place)))))

(defmacro prj2-with-widget (name ok &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (let ((buffer (get-buffer ,name)))
       (if buffer
           ;; Kill it if exist.
           (kill-buffer buffer)
         ;; Create one if doesn't exist.
         (switch-to-buffer ,name)
         (kill-all-local-variables)
         (let ((inhibit-read-only t))
           (erase-buffer))
         (remove-overlays)
         ;; TODO: fix compatibility with `company'.
         ;; Face
         (setq-local widget-button-face custom-button)
         (setq-local widget-button-pressed-face custom-button-pressed)
         (setq-local widget-mouse-face custom-button-mouse)
         ;; When possible, use relief for buttons, not bracketing.
         (when custom-raised-buttons
           (setq-local widget-push-button-prefix " ")
           (setq-local widget-push-button-suffix " ")
           (setq-local widget-link-prefix "")
           (setq-local widget-link-suffix ""))
         (setq show-trailing-whitespace nil)
         (setq prj2-tmp-string nil)
         (setq prj2-tmp-list1 nil)
         (setq prj2-tmp-list2 nil)
         (setq prj2-tmp-list3 nil)
         ,@body ;; <== body
         (widget-insert "\n")
         (widget-create 'push-button
                        :notify ,ok
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
         (company-mode)
         (make-local-variable 'company-backends)
         (add-to-list 'company-backends 'prj2-browse-file-backend)
         (add-to-list 'company-backends 'prj2-search-backend)))))

(defmacro prj2-validate-filepaths (paths)
  "Iterate the file paths in the configuration in order to discard invalid paths."
  `(let (valid-fp)
     (dolist (f ,paths)
       (let ((fp (and (file-exists-p f)
		      (expand-file-name f))))
	 (and fp
	      (push fp valid-fp))))
     (and valid-fp
	  (setq ,paths valid-fp))))

(defmacro prj2-sort-doctypes (choices references)
  `(let (orderlist)
     (dolist (i ,references)
       (and (member i ,choices)
	    (push i orderlist)))
     (setq orderlist (reverse orderlist)
	   ,choices orderlist)))

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
         (field-backend (widget-get field :company)))
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
           (dolist (file (prj2-directory-files dir (prj2-wildcardexp-to-regexp prj2-exclude-types)))
             (setq path (prj2-concat-filepath dir file))
             (push path candidates)
             ;; Add one level of children.
             (when (file-directory-p path)
               (push path directories)))
           (dolist (directory (reverse directories))
             (ignore-errors
               (dolist (child (prj2-directory-files directory (prj2-wildcardexp-to-regexp prj2-exclude-types)))
                 (setq path (prj2-concat-filepath directory child))
                 (push path candidates))))
           (setq prj2-browse-file-cache (cons dir (nreverse candidates)))))
    (all-completions prefix
                     (cdr prj2-browse-file-cache))))

(defun prj2-browse-file-backend (command &optional arg &rest ign)
  "The backend based on `company' to provide convenience when browsing files."
  (case command
    (prefix (prj2-common-prefix 'prj2-browse-file-backend))
    (candidates (prj2-browse-file-complete arg))
    (no-cache t)
    (require-match t)
    (ignore-case t)))

(defun prj2-search-complete (prefix)
  "The function responds 'candiates for `prj2-search-backend'."
  ;; TODO: use `prj2-search-cache'.
  (all-completions prefix (prj2-search-cache)))

(defun prj2-search-backend (command &optional arg &rest ign)
  "Following are for `company' when searching project."
  (case command
    (prefix (prj2-common-prefix 'prj2-search-backend))
    (candidates (prj2-search-complete arg))
    (ignore-case t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj2-serialize-doctype (doctype)
  (format "%s (%s)" (car doctype) (cdr doctype)))

(defun prj2-widget-clean ()
  (dolist (name '("*Create Project*"
                  "*Edit Project*"
                  "*Delete Project*"
                  "*Search Project*"))
    (let ((search (get-buffer name)))
      (and search
           (kill-buffer search)))))

(defun prj2-setup-create-project-widget ()
  (prj2-with-widget "*Create Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (lambda (&rest ignore)
      ;; Validate file paths.
      (prj2-validate-filepaths prj2-tmp-list2)
      ;; Return if there is an invalid info.
      (unless (and prj2-tmp-string
		   (> (length prj2-tmp-list1) 0)
		   (> (length prj2-tmp-list2) 0))
	(error "[Prj] Can't create new project due to invalid information."))
      ;; Sort the order of doctypes.
      (prj2-sort-doctypes prj2-tmp-list1 prj2-document-types)
      ;; Export configuration.
      (prj2-create-project-internal prj2-tmp-string prj2-tmp-list1 prj2-tmp-list2)
      (kill-buffer))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (widget-insert "=== Create New Project ===\n")
    (widget-insert "Use TAB to jump to forward widget; Shift-Tab to jump to backward widget.\n\n")
    (widget-create 'editable-field
		   :format "Project Name: %v"
		   :notify (prj2-widget-common-notify prj2-tmp-string))
    (widget-insert "\n")
    (widget-insert "Document Types (Edit) ") ;; TODO: add customizable link
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-tmp-list3)
			       (widget-value-set box t))
			     (dolist (type prj2-document-types)
			       (push type prj2-tmp-list1)))
		   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-tmp-list3)
			       (widget-value-set box nil)))
		   "Deselect All")
    (widget-insert " :\n")
    (dolist (type prj2-document-types)
      (let (wid)
	(setq wid (widget-create 'checkbox
				 :format (concat "%[%v%] " (prj2-serialize-doctype type) "\n")
				 :notify (prj2-widget-checkbox-notify :doctype prj2-tmp-list1)))
	(widget-put wid :doctype type)
	(push wid prj2-tmp-list3)))
    (widget-insert "\n")
    (widget-insert "Include Path:\n")
    (widget-insert "Button INS to insert a new path item; Button DEL to delete one.\n")
    (widget-insert "Use TAB to jump to forward path item or popup a path prompt.\n")
    (widget-create 'editable-list
		   :entry-format "%i %d %v"
		   :value '("")
		   :notify (prj2-widget-common-notify prj2-tmp-list2)
                   ;; Put :company to make company work for it.
		   '(editable-field :value "" :company prj2-browse-file-backend))))

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
      (prj2-validate-filepaths prj2-tmp-list2)
      (let (updatefiledb)
	;; Check if update is necessary.
	(catch 'loop
	  (unless (and (= (length prj2-tmp-list1) (length (prj2-project-doctypes)))
		       (= (length prj2-tmp-list2) (length (prj2-project-filepaths))))
	    (setq updatefiledb t)
	    (throw 'loop t))
	  (dolist (elm prj2-tmp-list1)
	    (unless (member elm (prj2-project-doctypes))
	      (setq updatefiledb t)
	      (throw 'loop t)))
	  (dolist (elm prj2-tmp-list2)
	    (unless (member elm (prj2-project-filepaths))
	      (setq updatefiledb t)
	      (throw 'loop t))))
	;; Export configuration.
	(prj2-edit-project-internal prj2-tmp-list1 prj2-tmp-list2 updatefiledb)
	(kill-buffer)))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq prj2-tmp-list1 (copy-list (prj2-project-doctypes)))
    (setq prj2-tmp-list2 (copy-list (prj2-project-filepaths)))

    (widget-insert "=== Edit Project ===\n")
    (widget-insert "Use TAB to jump to forward widget; Shift-Tab to jump to backward widget.\n\n")
    (widget-insert (format "Project Name: %s\n\n" (prj2-project-name)))
    (widget-insert "Document Types (Edit) ") ;; TODO: add customizable link
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-tmp-list3)
			       (widget-value-set box t))
			     (dolist (type prj2-document-types)
			       (push type prj2-tmp-list1)))
		   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-tmp-list3)
			       (widget-value-set box nil)))
		   "Deselect All")
    (widget-insert " :\n")
    (dolist (type prj2-document-types)
      (let (wid)
	(setq wid (widget-create 'checkbox
				 :format (concat "%[%v%] " (prj2-serialize-doctype type) "\n")
				 :value  (catch 'loop
					   (dolist (currtype (prj2-project-doctypes))
					     (and (equal (car currtype) (car type))
						  (throw 'loop t))))
				 :notify (prj2-widget-checkbox-notify :doctype prj2-tmp-list1)))
	(widget-put wid :doctype type)
	(push wid prj2-tmp-list3)))
    (widget-insert "\n")
    (widget-insert "Include Path:\n")
    (widget-create 'editable-list
		   :entry-format "%i %d %v"
		   :value (prj2-project-filepaths)
		   :notify (prj2-widget-common-notify prj2-tmp-list2)
		   '(editable-field :company prj2-browse-file-backend))))

(defun prj2-setup-search-project-widget (search)
  (prj2-with-widget "*Search Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (lambda (&rest ignore)
      (unless prj2-tmp-string
	(error (format "[%s] Please enter something for searching!" (prj2-project-name))))
      (unless (> (length prj2-tmp-list1) 0)
	(error (format "[%s] Please select document types for searching!" (prj2-project-name))))
      (kill-buffer)
      (prj2-search-project-internal prj2-tmp-string prj2-tmp-list1))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq prj2-tmp-list1 (copy-list (prj2-project-doctypes)))
    (and search
         (setq prj2-tmp-string search))

    (widget-insert "=== Search Project ===\n\n")
    (widget-insert (format "Project Name: %s\n\n" (prj2-project-name)))
    (widget-create 'editable-field
		   :format "Search: %v"
                   :value (or search "")
		   :notify (prj2-widget-common-notify prj2-tmp-string)
                   :company 'prj2-search-backend)
    (widget-insert "\n")
    (widget-insert "Document Types ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-tmp-list3)
			       (widget-value-set box t))
			     (dolist (type (prj2-project-doctypes))
			       (push type prj2-tmp-list1)))
		   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj2-tmp-list1 nil)
			     (dolist (box prj2-tmp-list3)
			       (widget-value-set box nil)))
		   "Deselect All")
    (widget-insert " :\n")
    (dolist (type (prj2-project-doctypes))
      (let (wid)
	(setq wid (widget-create 'checkbox
				 :format (concat "%[%v%] " (prj2-serialize-doctype type) "\n")
				 :value t
				 :notify (prj2-widget-checkbox-notify :doctypes prj2-tmp-list1)))
	(widget-put wid :doctypes type)
	(push wid prj2-tmp-list3)))))

(provide 'prj2-widget)
