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

(defvar oops--win-idtimer nil
  "The idle timer.")

(defconst oops--win-major-modes '(emacs-lisp-mode
                                  lisp-interaction-mode
                                  )
  "A list containing the supported major mode.")

(defconst oops--win-hooks '(;; (window-configuration-change-hook . oops--update-edit-win)
                            )
  "An association list containing the hook descriptions.")

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

(defun oops--update-edit-win ()
  "Find and set new editing window if the test for selected window is passed."
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
    ;; Update `oops--edit-win'.
    (when candidate
      (setq oops--edit-win candidate)
      )
    ;; If `selected-window' eauals to `oops--edit-win''s current value (no matter the value is updated or not),
    ;; enable basic perspective. Or disable it.
    ;; (when (eq oops--edit-win win)
    ;;   ;; Activate basic perspective.
    ;;   (if (memq major-mode oops--win-major-modes)
    ;;       (oops-toggle-basic-perspective 1)
    ;;     (oops-toggle-hsplit-perspective -1)
    ;;     (oops-toggle-basic-perspective -1)
    ;;     )
    ;;   )
    )
  )

(defun oops--win-init-idtimer (enable)
  "Create the `oops--win-idtimer' if `enable' is 1. Destory it if `enable' is -1."
  (if (> enable 0)
      ;; Enable idle timer.
      (if (null oops--win-idtimer)
          (setq oops--win-idtimer
                (run-with-idle-timer 0.5 t 'oops--update-edit-win))
          )
    ;; Disable idle timer.
    (unless (null oops--win-idtimer)
      (cancel-timer oops--win-idtimer)
      (setq oops--win-idtimer nil)
      )
    )
  )

;; = For User ==================================================================

;;;###autoload
(defun oops-windmove-left ()
  "Select the window to the left of the current one."
  (interactive)
  (windmove-left)
  )

;;;###autoload
(defun oops-windmove-right ()
  "Select the window to the right of the current one."
  (interactive)
  (windmove-right)
  )

;;;###autoload
(defun oops-windmove-up ()
  "Select the window to the above of the current one."
  (interactive)
  (windmove-up)
  )

;;;###autoload
(defun oops-windmove-down ()
  "Select the window to the below of the current one."
  (interactive)
  (windmove-down)
  )

;;;###autoload
(defun oops-toggle-hsplit-perspective (&optional toggle)
  (interactive)
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
    ;; TODO:
    ;; What if somebody kills the split window?
    (if enable
        ;; Enable hsplit perspective:
        (split-window oops--edit-win nil 'right)
      ;; Disable hsplit perspective:
      (delete-window (or (window-next-sibling oops--edit-win)
                         (window-prev-sibling oops--edit-win)))
      )
    ;; Update flag.
    (setq oops--is-hsplit-perspective enable)

    (message "[OOPS] hsplit perspective is %s." (if oops--is-hsplit-perspective "on" "off"))
    )
  )

;;;###autoload
(defun oops-toggle-basic-perspective (&optional toggle)
  (interactive)
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
        ;; Enable basic perspective:
        (progn
          ;; (if (null oops--outline-win)
          ;;     (setq oops--outline-win (split-window win w 'left))
          ;;   )
          (if (null oops--help-win)
              (setq oops--help-win (split-window win h 'below))
            )
          )
      ;; Disable basic perspective:
      ;; (when (window-valid-p oops--outline-win)
      ;;   (delete-window oops--outline-win)
      ;;   )
      ;; (setq oops--outline-win nil)

      (when (window-valid-p oops--help-win)
        (delete-window oops--help-win)
        )
      (setq oops--help-win nil)
      )
    ;; Update flag.
    (setq oops--is-basic-perspective enable)

    (message "[OOPS] basic perspective is %s." (if oops--is-basic-perspective "on" "off"))
    )
  )

;; = For Other Lisp ============================================================

(defun oops-update-help (data)
  "The `data' must be one of the following format:
1. (BUFFER . POINT)"
  ;; Only work on the situation that the basic perspective is enabled and
  ;; the edit win is ready.
  (when (and oops--is-basic-perspective
             (eq (selected-window) oops--edit-win))
    (cond
     ;; Format of (BUFFER . POINT):
     ((consp data)
      (let ((buf (car data))
            (pt (cdr data)))
        (with-selected-window oops--help-win
          (set-window-buffer nil buf)
          (goto-char pt)
          (recenter find-function-recenter-line)
          ;; TODO:
          ;; Seperate the buffers that are opened by user and opened by help function.
          ;; Discard the old buffers that are opened by help function.
          )
        )
      )
     ;; Single BUFFER
     ((bufferp data)
      (set-window-buffer oops--help-win data)
      )
     )
    )
  )

(defun oops-update-outline (data)
  ""
  (unless (null oops--outline-win)
    )
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
        ;; Enable idle timer.
        (oops--win-init-idtimer 1)
        ;; Enable hook
        (dolist (alist oops--win-hooks)
          (add-hook (car alist) (cdr alist)))
        )
    ;; Disable:
    ;; Destory idle timer.
    (oops--win-init-idtimer -1)
    ;; Destory hook.
    (dolist (alist oops--win-hooks)
      (remove-hook (car alist) (cdr alist)))
    ;; Clean all.
    (oops-toggle-hsplit-perspective -1)
    (oops-toggle-basic-perspective -1)
    )
  )

;; <============================================================================

(provide 'oops-win-mode)
