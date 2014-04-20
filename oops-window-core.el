(make-variable-buffer-local
 (defvar oops-dbuf nil
  "A buffer exist locally and dedicate to show definition."))

(defvar oops-def-bufs nil
  "A buffer dedicated to show definition.")

(defvar oops-def-window-enabled nil
  "A boolean tha indicate to show or hide the definition windows.")

;; =============================================================================

(defun oops-gen-new-def-buf (index)
  "Generate a new definition buffer with default name and index."
  (generate-new-buffer (format "*definition (%s)*" index))
  )

(defun oops-update-def-windows ()
  "A convenient way to get all the window of `selected-frame' binding with source codes. It filters out all the windows with the name starting with \"*\"."

  ;; Iterate all live windows.
  (dolist (win (window-list))
    (let* ((win-buf (window-buffer win))
           (buf-name (buffer-name win-buf))
           ;; definition buffer's amount.
           (i (length oops-def-bufs))
           ;; new window's height.
           (h (- (/ (window-total-height win) 4))))
      (when (not (string-match "^[*][a-zA-Z -_(0-9)]+[*]$" buf-name))
        (if (null oops-def-bufs)
            ;; 1st time to create buffer.
            (setq oops-def-bufs
                  (list (oops-gen-new-def-buf i)))
          ;; Push new buffer.
          (push (oops-gen-new-def-buf i)
                oops-def-bufs)
          )
        ;; Bind new window with new buffer.
        (split-window win h 'below)
        (set-window-buffer (window-next-sibling win) (car oops-def-bufs))
        )
      )
    )
  )
;; (split-window nil nil 'right)
;; (string-match "^[*]definition.*[*]" "*definition (0)*")
;; (oops-update-def-windows)

(defun oops-kill-def-windows ()
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

(defun oops-toggle-definition-window ()
  "Toggle the definition window linked to `oops-def-bufs' under the source code window. The height of definition window will be 1/3 height of `frame-root-window'.

.---.------------------.
|   |                  |
|   |        S         | S* -> Source/Option window (might be mutiple, depends on user)
| O |                  | D* -> Definition window (might be multiple, depends on amount of source windows)
|   |------------------| O  -> Outline window (only 1 for each frame)
|   |        D         |
'---'------------------'

            or

.---.--------.---------.
|   |        |         |
|   |   S1   |    S2   |
| O |        |         | or more complicated...
|   |--------:---------|
|   |   D1   |    D2   |
'---'--------'---------'
"
  (interactive)
  (if (null oops-def-window-enabled)
      ;; Enable.
      (progn (oops-update-def-windows)
             (setq oops-def-window-enabled t))
    ;; Disable.
    (progn (oops-kill-def-windows)
           (setq oops-def-window-enabled nil))
    )
  )

(defun oops-window-configuration-change-hook ()
  ""
  ;; Check definition window status.
  )

(defconst oops-window-core-hook-alist '((window-configuration-change-hook . oops-window-configuration-change-hook)
                                        )
  "")

(defun oops-init-window-core (enable)
  ""
  (if enable
      ;; Enable.
      (progn
        ;; Add hooks.
        (dolist (alist oops-window-core-hook-alist)
          (add-hook (car alist) (cdr alist))
          )
      )
    ;; Disable.
    (progn
      ;; Remove hooks.
      (dolist (alist  oops-window-core-hook-alist)
        (remove-hook (car alist) (cdr alist))
        )
      )
    )
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

(provide 'oops-window-core)
