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
 (defvar oops--is-hsplit-perspective nil))

(make-variable-frame-local
 (defvar oops--is-basic-perspective nil))

(make-variable-frame-local
 (defvar oops--edit-win nil
   "The editing window."))

(make-variable-frame-local
 (defvar oops--outline-win nil
   "The outline window."))

(make-variable-frame-local
 (defvar oops--help-win nil
   "The help window."))

(make-variable-frame-local
 (defvar oops--help-num-win nil
   "The help number window."))

(defconst oops--win-hook-alist '(
                                ;; (window-configuration-change-hook . oops-window-configuration-change-hook)
                                )
  "")

;; = Windows ===================================================================

(defun oops--kill-gnu-windows ()
  (dolist (win (window-list))
    (let* ((buf (window-buffer win))
           (name (buffer-name buf)))
      (if (string-match "^[*]\\(Help\\|Completions\\)[*]$" name)
          (delete-window win))
      )
    )
  )

(defun oops--update-edit-win ()
  "Set new editing window if the test for selected window is passed."
  (let* ((win (selected-window))
         (buf (window-buffer win))
         (name (buffer-name buf)))
    (unless (or (string-match "^[*]+[a-zA-Z ]+[*]+$" name)
                (memq win (list
                           oops--outline-win
                           oops--help-win
                           oops--help-num-win))
                (window-minibuffer-p win)
                )
      (setq oops--edit-win win)
      (message "%s" oops--edit-win)
      )
    )
  )

(defun oops--get-edit-win ()
  "Return editing window or the internal window containing editing windows."
  (interactive)
  (cond
   ;; Only one window.
   ((window-live-p (frame-root-window))
    (selected-window)
    )
   ;; Clean v-split windows.
   ((and oops--is-hsplit-perspective
         (= (length (window-list)) 2))
    (window-parent oops--edit-win)
    )
   ;; Default
   (t nil)
   )
  )

(defun oops-windmove-left ()
  "Select the window to the left of the current one."
  (interactive)
  (windmove-left)
  (oops--update-edit-win)
  )

(defun oops-windmove-right ()
  "Select the window to the right of the current one."
  (interactive)
  (windmove-right)
  (oops--update-edit-win)
  )

(defun oops-windmove-up ()
  "Select the window to the above of the current one."
  (interactive)
  (windmove-up)
  (oops--update-edit-win)
  )

(defun oops-windmove-down ()
  "Select the window to the below of the current one."
  (interactive)
  (windmove-down)
  (oops--update-edit-win)
  )

;; =============================================================================

(defun oops-toggle-hsplit-perspective (&optional toggle)
  (interactive)
  (when oops-win-mode
    ;; Kill all the GNU windows.
    (oops--kill-gnu-windows)
    (let ((enable (if toggle
                      ;; toggle == t or numeric number.
                      (if (booleanp toggle)
                          t
                        (> toggle 0))
                    ;; toggle == nil.
                    (not oops--is-hsplit-perspective)
                    ))
          (win (oops--get-edit-win)))
      (if enable
          (split-window win nil 'right)
        ;; else:
        (delete-window (or (window-next-sibling)
                           (window-prev-sibling)))
        )
      ;; Update flag.
      (setq oops--is-hsplit-perspective enable)
      )
    ;; Try to get new editing window.
    (oops--update-edit-win)
    )
  )

(defun oops-toggle-basic-perspective (&optional toggle)
  "
.---.------------------.
|   |                  |
|   |                  |
|   |        E         |
| O |                  |
|   |                  |
|   |------------------|
|   |        H         |
'---'------------------'
"
  (interactive)
  (when oops-win-mode
    ;; Kill all the GNU windows.
    (oops--kill-gnu-windows)
    (let ((enable (if toggle
                      ;; toggle == t or numeric number.
                      (if (booleanp toggle)
                          t
                        (> toggle 0))
                    ;; toggle == nil.
                    (not oops--is-basic-perspective)
                    ))
          (win (oops--get-edit-win))
          (w (/ (window-total-width) -5))
          (h (/ (window-total-height) -4)))
      (if enable
          ;; New windows.
          (progn
            (if (null oops--outline-win)
              (setq oops--outline-win (split-window win w 'left))
              )
            (if (null oops--help-win)
              (setq oops--help-win (split-window win h 'below))
              )
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
      ;; Update flag.
      (setq oops--is-basic-perspective enable)
      )
    ;; Try to get new editing window.
    (oops--update-edit-win)
    )
  )

;; =============================================================================

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
  ;; TODO: add key map, oops-windmove-left, oops-windmove-right, ...

  (if oops-win-mode
      ;; Enable.
      (progn
        (dolist (alist oops--win-hook-alist)
          (add-hook (car alist) (cdr alist)))
        ;; Try to get new editing window.
        (oops--update-edit-win)
        )
    ;; Disable.
    (dolist (alist oops--win-hook-alist)
      (remove-hook (car alist) (cdr alist)))
    ;; Clean all.
    (oops-toggle-basic-perspective -1)
    (oops-toggle-hsplit-perspective -1)
    )
  )

;; <============================================================================

(provide 'oops-win-mode)
