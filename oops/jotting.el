;; (overlays-in (point-min) (point-max))
;; (#<overlay from 40 to 41 in jotting.el> #<overlay from 4 to 5 in jotting.el> #<overlay from 1 to 1 in jotting.el> #<overlay from 1 to 1 in jotting.el> #<overlay from 1 to 1 in jotting.el> #<overlay from 1 to 1 in jotting.el>)

;; Study: (global-hl-line-mode)

(defun test-idtimer-function ()
  (message "%s" (overlays-in (point-min) (point-max)))
  )
(setq test-idtimer (run-with-idle-timer 0.5 t 'test-idtimer-function))
;; (cancel-timer test-idtimer)

(regexp-quote "http://www.w3.org/2001/XMLSchema-datatypes")
;; "http://www\\.w3\\.org/2001/XMLSchema-datatypes"
(regexp-quote "c:\home")
;; "c:home"
