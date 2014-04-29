;; Following are concept description and the diagram:
;; ('*' means there might be more than one of that kind of windows exist in a same time in a frame.)
;;
;; 1. Editing Window (E*):
;;    It binds to source code you're editing or a temporary search result.
;;    There might be more than one E* window at the same time in a frame.
;;
;; 2. Help Window (H):
;;    To show the definition of the symbol that is currently under cursor if there is only one matched result.
;;    The amount of D* windows increase or decrease proportionally to the amount of E* windows.
;;
;; 3. Help Number Window (HN):
;;    Only appears and provide a options list when there're multiple matched results.
;;    The amount of DO* windows increase or decrease proportionally to the amount of E* windows.
;;
;; 4. Outline Window (O):
;;    To show the outline of the selected E* window.
;;    There'll be only one outline window at the same time in a frame, even there're more than one E* windows.
;;
;; .---.------------------.
;; |   |                  |
;; |   |                  |
;; |   |        E         |
;; | O |                  |
;; |   |                  |
;; |   |------------------|
;; |   |        H         |
;; '---'------------------'
;;
;;             and
;;
;; .---.------------------.
;; |   |                  |
;; |   |                  |
;; |   |         E        |
;; | O |                  |
;; |   |                  |
;; |   |--.---------------|
;; |   |HN|      H        |
;; '---'--'---------------'
;;       ^
;;       |
;;       +---- a very narrow window with width just fits with digit number.
;;

(defvar oops-dbufs nil
  "A list containing buffer that dedicated to show definition.")

(make-variable-frame-local
 (defvar oops--outline-win nil
   "The outline window."))

(make-variable-frame-local
 (defvar oops--help-win nil
   "The help window."))

(make-variable-frame-local
 (defvar oops--help-num-win nil
   "The help number window."))

(defconst oops-win-hook-alist '(
                                ;; (window-configuration-change-hook . oops-window-configuration-change-hook)
                                )
  "")

;; = Windows ===================================================================

(defun oops--winmove-editing-window ()
  (interactive)
  (selected-window)
  )

(defun oops-toggle-browsing-windows (&optional toggle)
  (interactive)
  (let ((enable (if toggle
                    ;; toggle == t or numeric number.
                    (if (booleanp toggle)
                        t
                      (> toggle 0))
                  ;; toggle == nil.
                  ;; TODO/FIXME: exam D* is alive
                  (and (null oops--outline-win)
                       (null oops--help-win))))
        (win (oops--winmove-editing-window))
        (w -45)
        (h -15))
    (if enable
        ;; New windows.
        (progn
          (setq oops--outline-win (split-window win w 'left))
          (oops--winmove-editing-window)
          (setq oops--help-win (split-window win h 'below))
          )
      ;; Delete windows.
      (unless (null oops--outline-win)
        (delete-window oops--outline-win)
        (setq oops--outline-win nil)
        )
      (unless (null oops--help-win)
        (delete-window oops--help-win)
        (setq oops--help-win nil)
        )
      )
    )
  )

;; =============================================================================

(defun oops-new-dbuf ()
  "Generate a new definition buffer and push into `oops-dbufs'. Return the new buffer."
  (let ((buf (generate-new-buffer (format "*definition (%s)*" (length oops-dbufs)))))
    (if (null oops-dbufs)
        (setq oops-dbufs '(buf))
      (push buf oops-dbufs)
      )
    buf
    )
  )

(defun oops-del-dbuf (buf)
  "Delete a definition buff from `oops-dbufs'."
  (if (not (null buf))
      (setq oops-dbufs
            (delq buf oops-dbufs))
    )
  ;; TODO/FIXME: rename oops-dbufs
  )

(defun oops-kill-dbufs ()
  "Kill all the definition windows."
  (dolist (win (window-list))
    (let* ((win-buf (window-buffer win))
           (buf-name (buffer-name win-buf)))
      (when (string-match "^[*]definition.*[*]$" buf-name)
        (delete-window win)
        )
      )
    )
  )

(defun oops-update-help (data)
  ""
  )

(defun oops-update-outline (data)
  ""
  )

(defun oops-window-configuration-change-hook ()
  ""
  ;; Check definition window status.
  (message "window is changed.")
  )

;; = Minor Mode ===============================================================>

;;;###autoload
(define-minor-mode oops-win-mode
  "Window manager mode for ..."
  :global t

  (if oops-win-mode
      ;; Enable.
      (progn
        (dolist (alist oops-win-hook-alist)
          (add-hook (car alist) (cdr alist)))
        )
    ;; Disable.
    (progn
      (dolist (alist oops-win-hook-alist)
        (remove-hook (car alist) (cdr alist)))
      ;; TODO/FIXME: kill oops-dbufs.
      )
    )
  )

;; <============================================================================

(provide 'oops-win-mode)
