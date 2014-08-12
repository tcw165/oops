(defmacro prj-with-widget-common-notify (type config)
  "(Widget) Notify function for 'editable-field and 'editable-list."
  `(lambda (wid &rest ignore)
     (puthash ,type (widget-value wid) ,config)))

(defmacro prj-with-widget-checkbox-notify (type config)
  "(Widget) Notify function for 'checkbox."
  `(lambda (wid &rest ignore)
     (if (widget-value wid)
	 (puthash
	  ,type
	  (push (widget-get wid ,type) (gethash ,type ,config))
	  ,config)
       (puthash
	,type
	(delq (widget-get wid ,type) (gethash ,type ,config))
	,config))))

(defmacro prj-with-widget-cancel-notify ()
  "(Widget) Notify function for cancel 'push-button."
  `(lambda (&rest ignore)
     (kill-buffer)))

(defmacro prj-with-widget (name &rest body)
  `(progn
     (switch-to-buffer ,name)
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
       (erase-buffer))
     (remove-overlays)
     ;; Create a new temporary configuration.
     (setq prj-tmp-config (prj-new-config))
     ;; Face
     (set (make-local-variable 'widget-documentation-face) 'custom-documentation)
     (set (make-local-variable 'widget-button-face) custom-button)
     (set (make-local-variable 'widget-button-pressed-face) custom-button-pressed)
     (set (make-local-variable 'widget-mouse-face) custom-button-mouse)
     ;; When possible, use relief for buttons, not bracketing.
     (when custom-raised-buttons
       (set (make-local-variable 'widget-push-button-prefix) " ")
       (set (make-local-variable 'widget-push-button-suffix) " ")
       (set (make-local-variable 'widget-link-prefix) "")
       (set (make-local-variable 'widget-link-suffix) ""))
     (setq show-trailing-whitespace nil)
     ,@body ;; <== body
     (use-local-map widget-keymap)
     (widget-setup)))

(defun prj-setup-create-project-widget ()
  (prj-with-widget
   "*Create Project*"
   (widget-insert "=== Create New Project ===\n\n")
   (widget-create 'editable-field
		  :format "Project Name: %v"
		  :notify (prj-with-widget-common-notify :name prj-tmp-config))
   (widget-insert "\n")
   (widget-insert "Document Types (Edit):\n") ;; TODO: add customizable link
   (dolist (type prj-document-types)
     (widget-put (widget-create 'checkbox
				:format (concat "%[%v%] " (car type) " (" (cdr type) ")\n")
				:notify (prj-with-widget-checkbox-notify :doctypes prj-tmp-config))
		 :doctypes type))
   (widget-insert "\n")
   (widget-insert "Include Path:\n")
   (widget-create 'editable-list
		  :entry-format "%i %d %v"
		  :value '("")
		  :notify (prj-with-widget-common-notify :filepaths prj-tmp-config)
		  '(editable-field :value ""))
   (widget-insert "\n")
   ;; ok and cancel buttons.
   (widget-create 'push-button
		  :notify (lambda (&rest ignore)
			    (message "[%s] Creating new project ..." (gethash :name prj-tmp-config))
			    (let* ((config-file-path (expand-file-name (format "%s/%s/%s"
									       prj-workspace-path
									       (gethash :name prj-tmp-config)
									       prj-config-name)))
				   (dir (file-name-directory config-file-path)))
			      ;; Valid file paths.
			      (prj-validate-filepaths prj-tmp-config)
			      ;; Return if there is an invalid info.
			      (unless (and (gethash :name prj-tmp-config)
					   (> (length (gethash :doctypes prj-tmp-config)) 0)
					   (> (length (gethash :filepaths prj-tmp-config)) 0))
				(error "[Prj] Can't create new project due to invalid information."))
			      ;; Prepare directory. Directory name is also the project name.
			      (unless (file-directory-p dir)
				(make-directory dir))
			      ;; Export configuration.
			      (prj-export-data config-file-path prj-tmp-config)
			      ;; Kill the "Create Project" form.
			      (kill-buffer)
			      (prj-load-project)))
		  "ok")
   (widget-insert " ")
   (widget-create 'push-button
		  :notify (prj-with-widget-cancel-notify)
		  "cancel")
   (goto-char 43)))

(defun prj-setup-delete-project-widget ()
  (prj-with-widget
   "*Delete Project*"
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
				  :notify (prj-with-widget-checkbox-notify :filepaths prj-tmp-config))
		   :filepaths c)))
   (widget-insert "\n")
   ;; ok and cancel buttons.
   (widget-create 'push-button
		  :notify (lambda (&rest ignore)
			    (let ((cs (gethash :filepaths prj-tmp-config)))
			      (dolist (c cs)
				;; Unload current project if it is selected.
				(when (and (prj-project-p)
					   (string-equal (prj-project-name) c))
				  (prj-unload-project))
				;; Delete directory
				(delete-directory (format "%s/%s" prj-workspace-path c) t t)))
			    (kill-buffer)
			    (message "[Prj] Delet project ...done"))
		  "ok")
   (widget-insert " ")
   (widget-create 'push-button
		  :notify (prj-with-widget-cancel-notify)
		  "cancel")
   (goto-char 56)))

(defun prj-setup-edit-project-widget ()
  (prj-with-widget
   "*Edit Project*"
   (widget-insert "=== Edit Project ===\n\n")
   (widget-insert (format "Project Name: %s\n\n" (prj-project-name)))
   (widget-insert "Document Types (Edit):\n") ;; TODO: add customizable link
   (dolist (type prj-document-types)
     (widget-put (widget-create 'checkbox
				:format (concat "%[%v%] " (car type) " (" (cdr type) ")\n")
				:value (let (res)
					 (dolist (currtype (gethash :doctypes prj-config))
					   (if (string-equal (car currtype) (car type))
					       (setq res t)))
					 res)
				:notify (prj-with-widget-checkbox-notify :doctypes prj-config))
		 :doctypes type))
   (widget-insert "\n")
   (widget-insert "Include Path:\n")
   (widget-create 'editable-list
		  :entry-format "%i %d %v"
		  :value (gethash :filepaths prj-config)
		  :notify (prj-with-widget-common-notify :filepaths prj-config)
		  '(editable-field))
   (widget-insert "\n")
   ;; ok and cancel buttons.
   (widget-create 'push-button
		  :notify (lambda (&rest ignore)
			    ;; Validate file paths.
			    (prj-validate-filepaths prj-config)
			    ;; Export configuration.
			    (prj-export-data (prj-config-path) prj-config)
			    ;; Kill the "Edit Project" form.
			    (kill-buffer))
		  "ok")
   (widget-insert " ") 
   (widget-create 'push-button
		  :notify (prj-with-widget-cancel-notify)
		  "cancel")
   (goto-char 37)))

(defun prj-setup-search-project-widget ()
  (prj-with-widget
   "*Search*"
   (widget-insert "=== Search Project ===\n\n")
   (widget-create 'editable-field
		  :format "Search: %v")
   (widget-insert "\n")
   (widget-insert "Document Types ")
   (widget-create 'push-button
		  "Select All")
   (widget-insert " ")
   (widget-create 'push-button
		  "Deselect All")
   (widget-insert " :\n")
   (dolist (type (prj-project-doctypes))
     (widget-put (widget-create 'checkbox
				:format (concat "%[%v%] " (car type) " (" (cdr type) ")\n")
				:notify (prj-with-widget-checkbox-notify :doctypes prj-config))
		 :doctypes type))
   (widget-insert "\n")
   ;; ok and cancel buttons.
   (widget-create 'push-button
		  :notify (lambda (&rest ignore)
			    (let ((b (get-buffer-create "*Search*")))
			      (switch-to-buffer b)
			      (dolist (f (prj-import-data (prj-filedb-path)))
				(message "[%s] Search ...%s" (prj-project-name) f)
				(call-process "grep" nil (list (current-buffer) nil) t "-nH"  f)))
			    ;; Kill the "Create Project" form.
			    (kill-buffer))
		  "ok")
   (widget-insert " ")
   (widget-create 'push-button
		  :notify (prj-with-widget-cancel-notify)
		  "cancel")))

(provide 'prj-widget)
