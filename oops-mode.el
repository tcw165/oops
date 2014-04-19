;; The `oops-mode' provides the following feature:
;; * Find the definition of symbol nearby the `point'. If there's a sole result, open the source file and move `point' to where it is. If there're multiple results, show a option windows under the `current-buffer'.
;; * Show a definition window under the `current-buffer', the definition refers to the symbol nearby the `point'.
;;
;; The architecture concept:
;;
;;                 .-----------.
;;                 | oops-mode |
;;                 '-----------'
;;                 /      |     \
;;      .----------.  .-------.  .-----.
;;      | lisp-lib |  | c-lib |  | ... |
;;      '----------'  '-------'  '-----'
;;                       |
;;                      use
;;                       |
;;    ======================================
;;      .------.   .-----------------.
;;      | core | + | definition-core | + ...
;;      '------'   '-----------------'
;;

;; Core libraries:
(require 'oops-core)
(require 'oops-definition-core)
;; Language libraries:
(require 'oops-c-lib)
(require 'oops-cpp-lib)
(require 'oops-lisp-lib)
(require 'oops-python-lib)
;; TODO/FIXME:
;; * Port to built in customization.
(require 'oops-settings)

;; =============================================================================

(defconst oops-supported-alist
  (list 'emacs-lisp-mode-hook
        'lisp-interaction-mode-hook)
  "An association list that indicates the supported languages. Its format should be (MODE-HOOK . `oops-mode')")

(defvar oops-idle-timer nil
  "An idle timer that ask `oops-idle-timer-function' to do something.")

;; =============================================================================

(defun oops-jump-to-definition-atpt ()
  "Find the symbol's definition. If there's only one result, open the file in the current window. If there're multiple results, show a list under the current window.

For lisp, it supports symbol of `defun', `defadvice', `defvar' and `provide'.
For C/C++, it doesn't support yet.
For Python, it doesn't support yet."
  (interactive)
  (when oops-mode
    (cond
     ;; lisp
     ((or (eq major-mode 'emacs-lisp-mode)
          (eq major-mode 'lisp-interaction-mode))
      (oops-lisp-jump-to-definition-atpt)
      )
     ;; c
     ;; c++
     ;; python
     )
    )
  )

(defun oops-idle-timer-function ()
  "The function for `oops-idle-timer' do the following things:
  * Detect the symbol nearby the `point', and show symbol's definition in another window. Normally, the definition window is under the current window."
  (when oops-mode
    (message "%s" (current-time))
    (cond
     ;; lisp
     ((or (eq major-mode 'emacs-lisp-mode)
          (eq major-mode 'lisp-interaction-mode))oops-idle-timer-for-definition
      (oops-lisp-show-definition-atpt)
      )
     ;; c
     ;; c++
     ;; python
     ;; eshell
     )
    )
  )

;; = Minor Mode ===============================================================>

;;;###autoload
(define-minor-mode oops-mode
  ""
  :lighter " Oops"
  )

;; <============================================================================

(defun oops-init-hook (enable)
  "Add hooks and enable `oops-mode' for all the supported languages if `enable' is t.
Remove hooks and disable `oops-mode' for all the supported languages if `enable' is nil.

!DO NOT use this function in your lisp, or there would be side effects!"
  (if enable
      ;; Add hooks and enable `oops-mode' for all the supported languages.
      (progn (dolist (hook oops-supported-alist)
               (add-hook hook 'oops-mode))
             (dolist (bf (buffer-list))
               (with-current-buffer bf
                 (oops-mode 1)))
             )
    ;; Remove hooks and disable `oops-mode' for all the supported languages.
    (progn (dolist (hook oops-supported-alist)
             (remove-hook hook 'oops-mode))
           (dolist (bf (buffer-list))
               (with-current-buffer bf
                 (oops-mode -1)))
           )
    )
  )
;; (oops-init-hook t)
;; (oops-init-hook nil)

(defun oops-init-idle-timer (enable)
  "Create the `oops-idle-timer' if `enable' is t. Destory it if `enable' is nil.

!DO NOT use this function in your lisp, or there would be side effects!"
  ;; Enable idle timer.
  (when (and enable
             (null oops-idle-timer))
    (setq oops-idle-timer
          (run-with-idle-timer 0.2 t 'oops-idle-timer-function))
    )
  ;; Disable idle timer.
  (when (and (null enable)
             (timerp oops-idle-timer))
    (cancel-timer oops-idle-timer)
    (setq oops-idle-timer nil)
    )
  )
;; (oops-init-idle-timer t)
;; (oops-init-idle-timer nil)

(defun oops-global-init (enable)
  "A convenient way to let `oops-mode' automatically and globally enabled for all the supported languages.
To disable `oops-mode' if `enable' is nil."
  (interactive
   (let* ((in (read-minibuffer (format "oops-global-init (current state is \"%s\", plz enter t or nil): " oops-mode) nil)))
     (list (not (string-equal in "nil"))))
   )
  ;; hooks.
  (oops-init-hook enable)
  ;; idle timer.
  (oops-init-idle-timer enable)
  )
;; (oops-global-init t)
;; (oops-global-init nil)

(provide 'oops-mode)
