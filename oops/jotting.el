(defun split-window (&optional window size side)
  (interactive "i")
  (setq window (window-normalize-window window))
  (let* ((side (cond
                ((not side) 'below)
                ((memq side '(below above right left)) side)
                (t 'right)))
         (horizontal (not (memq side '(below above))))
         (frame (window-frame window))
         (parent (window-parent window))
         (function (window-parameter window 'split-window))
         (window-side (window-parameter window 'window-side))
         ;; Rebind the following two variables since in some cases we
         ;; have to override their value.
         (window-combination-limit window-combination-limit)
         (window-combination-resize window-combination-resize)
         atom-root)

    (window--check frame)
    (catch 'done
      (cond
       ;; Ignore window parameters if either `ignore-window-parameters'
       ;; is t or the `split-window' parameter equals t.
       ((or ignore-window-parameters (eq function t)))
       ((functionp function)
        ;; The `split-window' parameter specifies the function to call.
        ;; If that function is `ignore', do nothing.
        (throw 'done (funcall function window size side))
        )
       ;; If WINDOW is part of an atomic window, split the root window
       ;; of that atomic window instead.
       ((and (window-parameter window 'window-atom)
             (setq atom-root (window-atom-root window))
             (not (eq atom-root window)))
        (throw 'done (split-window atom-root size side))
        )
       ;; If WINDOW is a side window or its first or last child is a
       ;; side window, throw an error unless `window-combination-resize'
       ;; equals 'side.
       ((and (not (eq window-combination-resize 'side))
             (or (window-parameter window 'window-side)
                 (and (window-child window)
                      (or (window-parameter
                           (window-child window) 'window-side)
                          (window-parameter
                           (window-last-child window) 'window-side)))))
        (error "Cannot split side window or parent of side window")
        )
       ;; If `window-combination-resize' is 'side and window has a side
       ;; window sibling, bind `window-combination-limit' to t.
       ((and (not (eq window-combination-resize 'side))
             (or (and (window-prev-sibling window)
                      (window-parameter
                       (window-prev-sibling window) 'window-side))
                 (and (window-next-sibling window)
                      (window-parameter
                       (window-next-sibling window) 'window-side))))
        (setq window-combination-limit t)
        )
       )

      ;; If `window-combination-resize' is t and SIZE is non-negative,
      ;; bind `window-combination-limit' to t.
      (when (and (eq window-combination-resize t) size (> size 0))
        (setq window-combination-limit t))

      (let* ((parent-size
              ;; `parent-size' is the size of WINDOW's parent, provided
              ;; it has one.
              (when parent (window-total-size parent horizontal)))
             ;; `resize' non-nil means we are supposed to resize other
             ;; windows in WINDOW's combination.
             (resize
              (and window-combination-resize
                   (or (window-parameter window 'window-side)
                       (not (eq window-combination-resize 'side)))
                   (not (eq window-combination-limit t))
                   ;; Resize makes sense in iso-combinations only.
                   (window-combined-p window horizontal)))
             ;; `old-size' is the current size of WINDOW.
             (old-size (window-total-size window horizontal))
             ;; `new-size' is the specified or calculated size of the
             ;; new window.
             (new-size
              (cond
               ((not size)
                (max (window-split-min-size horizontal)
                     (if resize
                         ;; When resizing try to give the new window the
                         ;; average size of a window in its combination.
                         (min (- parent-size
                                 (window-min-size parent horizontal))
                              (/ parent-size
                                 (1+ (window-combinations
                                      parent horizontal))))
                       ;; Else try to give the new window half the size
                       ;; of WINDOW (plus an eventual odd line).
                       (+ (/ old-size 2) (% old-size 2)))))
               ((>= size 0)
                ;; SIZE non-negative specifies the new size of WINDOW.

                ;; Note: Specifying a non-negative SIZE is practically
                ;; always done as workaround for making the new window
                ;; appear above or on the left of the new window (the
                ;; ispell window is a typical example of that).  In all
                ;; these cases the SIDE argument should be set to 'above
                ;; or 'left in order to support the 'resize option.
                ;; Here we have to nest the windows instead, see above.
                (- old-size size))
               (t
                ;; SIZE negative specifies the size of the new window.
                (- size))))
             new-parent new-normal)

        ;; Check SIZE.
        (cond
         ((not size)
          (cond
           (resize
            ;; SIZE unspecified, resizing.
            (when (and (not (window-sizable-p parent (- new-size) horizontal))
                       ;; Try again with minimum split size.
                       (setq new-size
                             (max new-size (window-split-min-size horizontal)))
                       (not (window-sizable-p parent (- new-size) horizontal)))
              (error "Window %s too small for splitting" parent)))
           ((> (+ new-size (window-min-size window horizontal)) old-size)
            ;; SIZE unspecified, no resizing.
            (error "Window %s too small for splitting" window))))
         ((and (>= size 0)
               (or (>= size old-size)
                   (< new-size (if horizontal
                                   window-safe-min-width
                                 window-safe-min-width))))
          ;; SIZE specified as new size of old window.  If the new size
          ;; is larger than the old size or the size of the new window
          ;; would be less than the safe minimum, signal an error.
          (error "Window %s too small for splitting" window))
         (resize
          ;; SIZE specified, resizing.
          (unless (window-sizable-p parent (- new-size) horizontal)
            ;; If we cannot resize the parent give up.
            (error "Window %s too small for splitting" parent)))
         ((or (< new-size
                 (if horizontal window-safe-min-width window-safe-min-height))
              (< (- old-size new-size)
                 (if horizontal window-safe-min-width window-safe-min-height)))
          ;; SIZE specification violates minimum size restrictions.
          (error "Window %s too small for splitting" window)))

        (window--resize-reset frame horizontal)

        (setq new-parent
              ;; Make new-parent non-nil if we need a new parent window;
              ;; either because we want to nest or because WINDOW is not
              ;; iso-combined.
              (or (eq window-combination-limit t)
                  (not (window-combined-p window horizontal))))
        (setq new-normal
              ;; Make new-normal the normal size of the new window.
              (cond
               (size (/ (float new-size) (if new-parent old-size parent-size)))
               (new-parent 0.5)
               (resize (/ 1.0 (1+ (window-combinations parent horizontal))))
               (t (/ (window-normal-size window horizontal) 2.0))))

        (if resize
            ;; Try to get space from OLD's siblings.  We could go "up" and
            ;; try getting additional space from surrounding windows but
            ;; we won't be able to return space to those windows when we
            ;; delete the one we create here.  Hence we do not go up.
            (progn
              (window--resize-child-windows parent (- new-size) horizontal)
              (let* ((normal (- 1.0 new-normal))
                     (sub (window-child parent)))
                (while sub
                  (set-window-new-normal
                   sub (* (window-normal-size sub horizontal) normal))
                  (setq sub (window-right sub)))))
          ;; Get entire space from WINDOW.
          (set-window-new-total window (- old-size new-size))
          (window--resize-this-window window (- new-size) horizontal)
          (set-window-new-normal
           window (- (if new-parent 1.0 (window-normal-size window horizontal))
                     new-normal)))

        (let* ((new (split-window-internal window new-size side new-normal)))
          ;; Assign window-side parameters, if any.
          (when (eq window-combination-resize 'side)
            (let ((window-side
                   (cond
                    (window-side window-side)
                    ((eq side 'above) 'top)
                    ((eq side 'below) 'bottom)
                    (t side))))
              ;; We made a new side window.
              (set-window-parameter new 'window-side window-side)
              (when (and new-parent (window-parameter window 'window-side))
                ;; We've been splitting a side root window.  Give the
                ;; new parent the same window-side parameter.
                (set-window-parameter
                 (window-parent new) 'window-side window-side))))

          (run-window-configuration-change-hook frame)
          (window--check frame)
          ;; Always return the new window.
          new
          )
        )
      )
    )
  )
