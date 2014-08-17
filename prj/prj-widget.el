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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar prj-tmp-string nil)
(defvar prj-tmp-list1 nil)
(defvar prj-tmp-list2 nil)
(defvar prj-tmp-list3 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro prj-widget-common-notify (place)
  "(Widget) Notify function for 'editable-field and 'editable-list."
  `(lambda (wid &rest ignore)
     (setq ,place (widget-value wid))))

(defmacro prj-widget-checkbox-notify (prop place)
  "(Widget) Notify function for 'checkbox."
  `(lambda (wid &rest ignore)
     (if (widget-value wid)
	 (push (widget-get wid ,prop) ,place)
       (setq ,place (delq (widget-get wid ,prop) ,place)))))

(defmacro prj-with-widget (name ok &rest body)
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
         (setq prj-tmp-string nil)
         (setq prj-tmp-list1 nil)
         (setq prj-tmp-list2 nil)
         (setq prj-tmp-list3 nil)
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
         (use-local-map widget-keymap)
         (widget-setup)
         ;; Enable specific mode.
         (company-mode)
         (make-local-variable 'company-backends)
         (add-hook 'widget-forward-hook 'prj-widget-forward-or-company t t)
         (add-to-list 'company-backends 'prj-browse-file-backend)))))

(defmacro prj-validate-filepaths (paths)
  "Iterate the file paths in the configuration in order to discard invalid paths."
  `(let (valid-fp)
     (dolist (f ,paths)
       (let ((fp (and (file-exists-p f)
		      (expand-file-name f))))
	 (and fp
	      (push fp valid-fp))))
     (and valid-fp
	  (setq ,paths valid-fp))))

(defmacro prj-sort-doctypes (choices references)
  `(let (orderlist)
     (dolist (i ,references)
       (and (member i ,choices)
	    (push i orderlist)))
     (setq orderlist (reverse orderlist)
	   ,choices orderlist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-widget-forward-or-company ()
  "It is for `widget-forward-hook' to continue forward to next widget or show company prompt."
  (interactive)
  (and (prj-browse-file-prefix)
       (company-complete)
       (top-level)))

(defun prj-browse-file-prefix ()
  "The function respond 'prefix for `prj-browse-file-backend'. Return nil means skip this backend function; Any string means there're candidates should be prompt. Only the editable-field with :file-browse property is allowed."
  (let* ((field (widget-field-at (point)))
         (supported (widget-get field :file-browse)))
    (if (and field supported)
        (let ((start (widget-field-start field))
              (end (widget-field-end field)))
          (buffer-substring-no-properties start end))
      nil)))

(defun prj-browse-file-backend (command &optional arg &rest ign)
  "The backend based on `company' to provide convenience when browsing files."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'prj-browse-file-backend))
    (prefix (prj-browse-file-prefix))
    ;; (no-cache t)
    (require-match t)
    (ignore-case t)
    (candidates (company-files command arg ign))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-serialize-doctype (doctype)
  (format "%s (%s)" (car doctype) (cdr doctype)))

(defun prj-setup-create-project-widget ()
  (prj-with-widget "*Create Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (lambda (&rest ignore)
      ;; Validate file paths.
      (prj-validate-filepaths prj-tmp-list2)
      ;; Return if there is an invalid info.
      (unless (and prj-tmp-string
		   (> (length prj-tmp-list1) 0)
		   (> (length prj-tmp-list2) 0))
	(error "[Prj] Can't create new project due to invalid information."))
      ;; Sort the order of doctypes.
      (prj-sort-doctypes prj-tmp-list1 prj-document-types)
      ;; Export configuration.
      (prj-create-project-internal prj-tmp-string prj-tmp-list1 prj-tmp-list2)
      (kill-buffer))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (widget-insert "=== Create New Project ===\n\n")
    (widget-create 'editable-field
		   :format "Project Name: %v"
		   :notify (prj-widget-common-notify prj-tmp-string))
    (widget-insert "\n")
    (widget-insert "Document Types (Edit) ") ;; TODO: add customizable link
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj-tmp-list1 nil)
			     (dolist (box prj-tmp-list3)
			       (widget-value-set box t))
			     (dolist (type prj-document-types)
			       (push type prj-tmp-list1)))
		   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj-tmp-list1 nil)
			     (dolist (box prj-tmp-list3)
			       (widget-value-set box nil)))
		   "Deselect All")
    (widget-insert " :\n")
    (dolist (type prj-document-types)
      (let (wid)
	(setq wid (widget-create 'checkbox
				 :format (concat "%[%v%] " (prj-serialize-doctype type) "\n")
				 :notify (prj-widget-checkbox-notify :doctype prj-tmp-list1)))
	(widget-put wid :doctype type)
	(push wid prj-tmp-list3)))
    (widget-insert "\n")
    (widget-insert "Include Path:\n")
    (widget-create 'editable-list
		   :entry-format "%i %d %v"
		   :value '("")
		   :notify (prj-widget-common-notify prj-tmp-list2)
                   ;; Put :file-browse to make company work for it.
		   '(editable-field :value "" :file-browse t))))

(defun prj-setup-delete-project-widget ()
  (prj-with-widget "*Delete Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (lambda (&rest ignore)
      (prj-delete-project-internal prj-tmp-list1)
      (kill-buffer))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (widget-insert "=== Delete Project ===\n\n")
    (widget-insert "Select projects to be deleted:\n")
    (let (choices)
      (dolist (f (directory-files prj-workspace-path))
	(let ((config-file (format "%s/%s/%s" prj-workspace-path f prj-config-name)))
	  (when (file-exists-p config-file)
	    (push f choices))))
      (unless choices
	(kill-buffer)
	(error "[Prj] No projects can be deleted."))
      (dolist (c choices)
	(widget-put (widget-create 'checkbox
				   :format (concat "%[%v%] " c "\n")
				   :notify (prj-widget-checkbox-notify :filepaths prj-tmp-list1))
		    :filepaths c)))))

(defun prj-setup-edit-project-widget ()
  (prj-with-widget "*Edit Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (lambda (&rest ignore)
      ;; Sort the order of doctypes.
      (prj-sort-doctypes prj-tmp-list1 prj-document-types)
      ;; Validate file paths.
      (prj-validate-filepaths prj-tmp-list2)
      (let (updatefiledb)
	;; Check if update is necessary.
	(catch 'loop
	  (unless (and (= (length prj-tmp-list1) (length (prj-project-doctypes)))
		       (= (length prj-tmp-list2) (length (prj-project-filepaths))))
	    (setq updatefiledb t)
	    (throw 'loop t))
	  (dolist (elm prj-tmp-list1)
	    (unless (member elm (prj-project-doctypes))
	      (setq updatefiledb t)
	      (throw 'loop t)))
	  (dolist (elm prj-tmp-list2)
	    (unless (member elm (prj-project-filepaths))
	      (setq updatefiledb t)
	      (throw 'loop t))))
	;; Export configuration.
	(prj-edit-project-internal prj-tmp-list1 prj-tmp-list2 updatefiledb)
	(kill-buffer)))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq prj-tmp-list1 (copy-list (prj-project-doctypes)))
    (setq prj-tmp-list2 (copy-list (prj-project-filepaths)))

    (widget-insert "=== Edit Project ===\n\n")
    (widget-insert (format "Project Name: %s\n\n" (prj-project-name)))
    (widget-insert "Document Types (Edit) ") ;; TODO: add customizable link
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj-tmp-list1 nil)
			     (dolist (box prj-tmp-list3)
			       (widget-value-set box t))
			     (dolist (type prj-document-types)
			       (push type prj-tmp-list1)))
		   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj-tmp-list1 nil)
			     (dolist (box prj-tmp-list3)
			       (widget-value-set box nil)))
		   "Deselect All")
    (widget-insert " :\n")
    (dolist (type prj-document-types)
      (let (wid)
	(setq wid (widget-create 'checkbox
				 :format (concat "%[%v%] " (prj-serialize-doctype type) "\n")
				 :value  (catch 'loop
					   (dolist (currtype (prj-project-doctypes))
					     (and (equal (car currtype) (car type))
						  (throw 'loop t))))
				 :notify (prj-widget-checkbox-notify :doctype prj-tmp-list1)))
	(widget-put wid :doctype type)
	(push wid prj-tmp-list3)))
    (widget-insert "\n")
    (widget-insert "Include Path:\n")
    (widget-create 'editable-list
		   :entry-format "%i %d %v"
		   :value (prj-project-filepaths)
		   :notify (prj-widget-common-notify prj-tmp-list2)
		   '(editable-field))))

(defun prj-setup-search-project-widget ()
  (prj-with-widget "*Search Project*"
    ;; Ok notify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (lambda (&rest ignore)
      (unless prj-tmp-string
	(error (format "[%s] Please enter something for searching!" (prj-project-name))))
      (unless (> (length prj-tmp-list1) 0)
	(error (format "[%s] Please select document types for searching!" (prj-project-name))))
      (kill-buffer)
      (prj-search-project-internal prj-tmp-string prj-tmp-list1))

    ;; Body ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq prj-tmp-list1 (copy-list (prj-project-doctypes)))

    (widget-insert "=== Search Project ===\n\n")
    (widget-insert (format "Project Name: %s\n\n" (prj-project-name)))
    (widget-create 'editable-field
		   :format "Search: %v"
		   :notify (prj-widget-common-notify prj-tmp-string))
    (widget-insert "\n")
    (widget-insert "Document Types ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj-tmp-list1 nil)
			     (dolist (box prj-tmp-list3)
			       (widget-value-set box t))
			     (dolist (type (prj-project-doctypes))
			       (push type prj-tmp-list1)))
		   "Select All")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (setq prj-tmp-list1 nil)
			     (dolist (box prj-tmp-list3)
			       (widget-value-set box nil)))
		   "Deselect All")
    (widget-insert " :\n")
    (dolist (type (prj-project-doctypes))
      (let (wid)
	(setq wid (widget-create 'checkbox
				 :format (concat "%[%v%] " (prj-serialize-doctype type) "\n")
				 :value t
				 :notify (prj-widget-checkbox-notify :doctypes prj-tmp-list1)))
	(widget-put wid :doctypes type)
	(push wid prj-tmp-list3)))))

(provide 'prj-widget)
