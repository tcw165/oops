(make-variable-buffer-local
 (defvar oops-definition-buffer nil
  "A buffer dedicated to show definition."))

(make-variable-buffer-local
 (defvar oops-definition-window-enabled t
   "A buffer-local variable indicate to show or hide definition window."))

(defun oops-init-def-buffer (flag)
  "Initialize definition buffer or destory it. Depends on `flag', t indicate to initialize buffer and nil to destory the buffer.
e.g.
(oops-init-def-buffer t)   => Create buffer.
(oops-init-def-buffer nil) => Destory buffer."
  (if flag
      ;; Create the buffer.
      (if (null oops-definition-buffer)
          (setq oops-definition-buffer
                (generate-new-buffer "#def"))
        )
    ;; Destory the buffer.
    (if (not (null oops-definition-buffer))
        (kill-buffer oops-definition-buffer)
      )
    )
  )

(defun oops-toggle-definition-window (&optional enable)
  "Toggle the definition window linked to `oops-definition-buffer' under the `selected-window'. The height of definition window will be 1/3 height of `frame-root-window'.

.----------------------.
|   |                  |
|   |        A         | A -> Source/Option window
| C |                  | B -> Definition window
|   |------------------| C -> Outline window
|   |        B         |
'----------------------'
"
  (interactive)
  (when (and oops-mode
             enable)
    ;; Force source window to be selected.

    ;; Init buffer.
    (oops-init-def-buffer t)
    ;; Split or merge window.
    (if (or (null (window-parent)) ;; only 1 window total.
            ) ;; parent has zero vertical
        (let* ((h (- (/ (window-total-height) 4)))
               (win (split-window (selected-window) h 'below)))
          (set-window-buffer win oops-definition-buffer)
          )
      )
    )
  )

;; (split-window)
;; (split-window (selected-window) -10 'below)
;; (oops-init-def-buffer nil)
;; (window-list)
;; (frame-root-window)
;; (length (window-list (selected-frame) nil (selected-window)))
;; (window-next-sibling (selected-window))
;; (window-top-child (frame-root-window))
;; (window-tree)

;; (window-body-height)
;; (window-text-height)
;; (window-total-height (frame-root-window))
;; (frame-height)

(provide 'oops-definition-core)
