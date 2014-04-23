;;
;; .---.------------------.
;; |   |                  |
;; |   |        S         | S* -> Source/Option window (might be mutiple, depends on user)
;; | O |                  | D* -> Definition window (might be multiple, depends on amount of source windows)
;; |   |------------------| O  -> Outline window (only 1 for each frame)
;; |   |        D         |
;; '---'------------------'

;;             or

;; .---.--------.---------.
;; |   |        |         |
;; |   |   S1   |    S2   |
;; | O |        |         | or more complicated...
;; |   |--------:---------|
;; |   |   D1   |    D2   |
;; '---'--------'---------'

(defvar oops-dbufs nil
  "A list containing buffer that dedicated to show definition.")

(defconst oops-win-hook-alist '((window-configuration-change-hook . oops-window-configuration-change-hook)
                                )
  "")

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

;; (defun oops-update-def-windows ()
;;   "A convenient way to get all the window of `selected-frame' binding with source codes. It filters out all the windows with the name starting with \"*\"."

;;   ;; Iterate all live windows.
;;   (dolist (win (window-list))
;;     (let* ((win-buf (window-buffer win))
;;            (buf-name (buffer-name win-buf))
;;            ;; definition buffer's amount.
;;            (i (length oops-dbufs))
;;            ;; new window's height.
;;            (h (- (/ (window-total-height win) 4))))
;;       (when (not (string-match "^[*][a-zA-Z -_(0-9)]+[*]$" buf-name))
;;         (if (null oops-dbufs)
;;             ;; 1st time to create buffer.
;;             (setq oops-dbufs
;;                   (list (oops-gen-new-def-buf i)))
;;           ;; Push new buffer.
;;           (push (oops-gen-new-def-buf i)
;;                 oops-dbufs)
;;           )
;;         ;; Bind new window with new buffer.
;;         (split-window win h 'below)
;;         (set-window-buffer (window-next-sibling win) (car oops-dbufs))
;;         )
;;       )
;;     )
;;   )

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

(defun oops-win-mgr-show-definition (data)
  "Toggle the definition window linked to `oops-dbufs' under the source code window. The height of definition window will be 1/3 height of `frame-root-window'."
  (when oops-win-mgr-mode
    (cond
     ;; nil
     ((null data)
      ;; kill window
      (oops-kill-dbufs)
      )
     ;; (FILE_NAME . POINT)
     ((consp data)
      ;; kill old window
      ;; show window
      )
     ;; special form for mutiple results.
     )
    )
  )

(defun oops-window-configuration-change-hook ()
  ""
  ;; Check definition window status.
  (message "window is changed.")
  )

;; (oops-init-window-core t)
;; (oops-init-window-core nil)

;; (split-window)
;; (window-list)
;; (delete-window (or (window-next-sibling) (window-prev-sibling)))

;; (message "%s"(buffer-list))

;; (window-body-height)
;; (window-text-height)
;; (window-total-height (frame-root-window))
;; (frame-height)

;; ;;;###autoload
;; (defun oops-win-mgr-mode (&optional toggle)
;;   "A convenient way to let `oops-win-mgr-mode' automatically enabled or disabled.
;;  (oops-win-mgr-mode 1)  => enable `oops-win-mgr-mode'.
;;  (oops-win-mgr-mode -1) => disable `oops-win-mgr-mode'."
;;   (interactive)
;;   (let ((enable (if toggle
;;                     ;; toggle == t or numeric number.
;;                     (if (booleanp toggle)
;;                         t
;;                       (> toggle 0))
;;                   ;; toggle == nil.
;;                   (not oops-win-mgr-mode))))
;;     ;; togggle mode.
;;     (oops-win-mgr-mode (if enable 1 -1))
;;     )
;;   )

;; = Minor Mode ===============================================================>

;;;###autoload
(define-minor-mode oops-win-mgr-mode
  "Window manager mode for ..."
  :global t

  (if oops-win-mgr-mode
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

(provide 'oops-win-mgr-mode)
