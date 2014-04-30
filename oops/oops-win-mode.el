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

(defvar oops--win-idtimer nil
  "The idle timer.")

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
                                ;; (window-configuration-change-hook . oops--update-edit-win)
                                )
  "")

;; = Internal ==================================================================

(defun oops--kill-gnu-windows ()
  (dolist (win (window-list))
    (let* ((buf (window-buffer win))
           (name (buffer-name buf)))
      (if (string-match "^[*]\\(Help\\|Completions\\)[*]$" name)
          (delete-window win))
      )
    )
  )

(defun oops--update-edit-win (&optional param)
  "Set new editing window if the test for selected window is passed."
  (let* ((win (selected-window))
         (buf (window-buffer win))
         (name (buffer-name buf))
         (candidate (unless (or (string-match "^[*]+[a-zA-Z ]+[*]+$" name)
                                (memq win (list
                                           oops--outline-win
                                           oops--help-win
                                           oops--help-num-win))
                                (minibuffer-window-active-p win))
                      win)))
    (if candidate
        (setq oops--edit-win candidate)
      )
    ;; Activate basic perspective.
;;    (oops-toggle-basic-perspective 1)
    )
  )
;; (oops-toggle-basic-perspective   1)
;; (oops-toggle-basic-perspective  -1)
;; (oops-toggle-hsplit-perspective  1)
;; (oops-toggle-hsplit-perspective -1)

(defun oops--win-idle-timer (enable)
  "Create the `oops--win-idtimer' if `enable' is t. Destory it if `enable' is nil.

!DO NOT use this function in your lisp, or there would be side effects!"
  (if enable
      ;; Enable idle timer.
      (if (null oops--win-idtimer)
          (setq oops--win-idtimer
                (run-with-idle-timer 0.1 t 'oops--update-edit-win))
          )
    ;; Disable idle timer.
    (unless (null oops--win-idtimer)
      (cancel-timer oops--win-idtimer)
      (setq oops--win-idtimer nil)
      )
    )
  )

;; = For User ==================================================================

(defun oops-windmove-left ()
  "Select the window to the left of the current one."
  (interactive)
  (windmove-left)
  )

(defun oops-windmove-right ()
  "Select the window to the right of the current one."
  (interactive)
  (windmove-right)
  )

(defun oops-windmove-up ()
  "Select the window to the above of the current one."
  (interactive)
  (windmove-up)
  )

(defun oops-windmove-down ()
  "Select the window to the below of the current one."
  (interactive)
  (windmove-down)
  ;; (oops--update-edit-win)
  )

(defun oops-toggle-hsplit-perspective (&optional toggle)
  (interactive)
  (when (and oops-win-mode
             oops--edit-win)
    ;; Kill all the GNU windows.
    (oops--kill-gnu-windows)
    (let ((enable (if toggle
                      ;; toggle == t or numeric number.
                      (if (booleanp toggle)
                          t
                        (> toggle 0))
                    ;; toggle == nil.
                    (not oops--is-hsplit-perspective)
                    )))
      (if enable
          (split-window oops--edit-win nil 'right)
        ;; else:
        (if oops--is-basic-perspective
            ;; 1
            ;; 2
            )
        (delete-window (or (window-next-sibling oops--edit-win)
                           (window-prev-sibling oops--edit-win)))
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
  (when (and oops-win-mode
             oops--edit-win)
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
          (win (cond
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
                (t oops--edit-win)
                ))
          (w (/ (window-total-width) -5))
          (h (/ (window-total-height) -4)))
      (if enable
          ;; Enable basic perspective.
          (progn
            (if (null oops--outline-win)
              (setq oops--outline-win (split-window win w 'left))
              )
            (if (null oops--help-win)
              (setq oops--help-win (split-window win h 'below))
              )
            )
        ;; Disable basic perspective.
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

;; = For Other Lisp ============================================================

(defun oops-update-help (data)
  ""
  )

(defun oops-update-outline (data)
  ""
  )

;; = Minor Mode ===============================================================>

;;;###autoload
(define-minor-mode oops-win-mode
  "Window manager mode for ..."
  :global t
  ;; TODO: add key map, oops-windmove-left, oops-windmove-right, ...

  (if oops-win-mode
      ;; Enable:
      (progn
        ;; Init idle timer.
        (oops--win-idle-timer 1)
        ;; Init hook
        (dolist (alist oops--win-hook-alist)
          (add-hook (car alist) (cdr alist)))
        )
    ;; Disable:
    ;; Destory idle timer.
    (oops--win-idle-timer -1)
    ;; Destory hook.
    (dolist (alist oops--win-hook-alist)
      (remove-hook (car alist) (cdr alist)))
    ;; Clean all.
    (oops-toggle-hsplit-perspective -1)
    (oops-toggle-basic-perspective -1)
    )
  )

;; <============================================================================

(provide 'oops-win-mode)
