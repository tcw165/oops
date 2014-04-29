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
;;     .------.   .-----------------.
;;     | core | + | definition-core | + ...
;;     '------'   '-----------------'
;;

(defun oops-add-to-list-with-subdirs (base exclude)
  "Add sub-directories recursively to `load-path'.
The `base' should be a directory string and the `exclude' should be a list that to be skipped."
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (member f exclude)))
        (oops-add-to-list-with-subdirs name exclude)
        )
      )
    )
  (add-to-list 'load-path base)
  )
(oops-add-to-list-with-subdirs "~/.emacs.d" '("." ".." ".svn" ".git"))

;; 3rd party library ===========================================================
(require 'highlight-symbol)
(require 'highlight-parentheses)

;; Oops library ================================================================

;; Core libraries:
(require 'oops-core)
(require 'oops-win-mode)
(require 'oops-help)
;; Language libraries:
(require 'oops-c-lib)
(require 'oops-cpp-lib)
(require 'oops-lisp-lib)
(require 'oops-python-lib)
;; Settings:
(require 'oops-settings)

;; =============================================================================

(defconst oops-hook-alist
  '(;; imenu
    (emacs-lisp-mode-hook . imenu-add-menubar-index)
    (lisp-interaction-mode-hook . imenu-add-menubar-index))
  "An association list that indicates the bindings of major mode and minor mode. Its format should be (MAJOR-MODE-HOOK . MINOR-MODE-HOOK)")

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
    (cond
     ;; lisp
     ((or (eq major-mode 'emacs-lisp-mode)
          (eq major-mode 'lisp-interaction-mode))
      (oops-lisp-show-definition-atpt)
      )
     ;; c
     ;; c++-mode
     ;; python-mode
     ;; eshell
     )
    )
  )

(defun oops-init-idle-timer (enable)
  "Create the `oops-idle-timer' if `enable' is t. Destory it if `enable' is nil.

!DO NOT use this function in your lisp, or there would be side effects!"
  (if enable
      ;; Enable idle timer.
      (if (null oops-idle-timer)
          (setq oops-idle-timer
                (run-with-idle-timer 0.2 t 'oops-idle-timer-function))
          )
    ;; Disable idle timer.
    (if (and (not (null oops-idle-timer))
             (timerp oops-idle-timer))
        (progn (cancel-timer oops-idle-timer)
               (setq oops-idle-timer nil))
        )
    )
  )

;; ;; ###autoload
;; (defun oops-mode (&optional toggle)
;;   "A convenient way to let `oops-everywhere-mode' automatically and globally enabled for all the supported languages.
;;  (oops-mode 1)  => enable `oops-everywhere-mode'.
;;  (oops-mode -1) => disable `oops-everywhere-mode'."
;;   (interactive)
;;   (let ((enable (if toggle
;;                     ;; toggle == t or numeric number.
;;                     (if (booleanp toggle)
;;                         t
;;                       (> toggle 0))
;;                   ;; toggle == nil.
;;                   (not oops-everywhere-mode))))
;;     ;; idle timer.
;;     (oops-init-idle-timer enable)

;;     ;; togggle mode.
;;     (oops-everywhere-mode (if enable 1 -1))
;;     (oops-win-mode (if enable 1 -1))
;;     )
;;   )

;; = Minor Mode ===============================================================>

;;;###autoload
(define-minor-mode oops-mode
  ""
  :lighter " Oops"
  :global t

  (if oops-mode
      ;; Enable.
      (progn (dolist (l oops-hook-alist)
               (add-hook (car l) (cdr l)))

             (oops-init-idle-timer 1)
             (oops-win-mode 1)
             )
    ;; Disable.
    (progn (dolist (l oops-hook-alist)
             (remove-hook (car l) (cdr l)))

           (oops-init-idle-timer -1)
           (oops-win-mode -1)
           )
    )
  )

;; <============================================================================

(provide 'oops-mode)
