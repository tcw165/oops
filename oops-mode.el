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

(defvar oops-idle-timer-for-definition nil
  "An idle timer that detect the symbol nearby the `point', and show symbol's definition in another window. Normally, the definition window is under the current window.")

(defun oops-find-definition-at-point ()
  "Find the symbol's definition. If there's only one result, open the file in the current window. If there're multiple results, show a list under the current window.

For lisp, it supports symbol of `defun', `defadvice', `defvar' and `provide'.
For C/C++, it doesn't support yet.
For Python, it doesn't support yet."
  (interactive)
  (cond
   ;; lisp
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'lisp-interaction-mode))
    (oops-lisp-find-definition-at-point)
    )
   ;; c
   ;; c++
   ;; python
   )
  )

(defun oops-show-definition-at-point ()
  ""
  (when oops-mode
    (cond
     ;; lisp
     ((or (eq major-mode 'emacs-lisp-mode)
          (eq major-mode 'lisp-interaction-mode))
      (oops-lisp-show-definition-at-point)
      (message "Yet ready...")
      )
     ;; c
     ;; c++
     ;; python
     ;; eshell
     )
    )
  )

;;;###autoload
(define-minor-mode oops-mode
  ""
  :lighter " Oops"
  (if oops-mode
      ;; Enable idle timer to show definition.
      (setq oops-idle-timer-for-definition
            (run-with-idle-timer 0.2 t 'oops-show-definition-at-point))
    ;; Disable idle timer to show definition.
    (cancel-timer oops-idle-timer-for-definition)
    )
  )

(defun oops-enable-all (&optional flag)
  "Provide a convenient way to let `oops-mode' enabled with supported language by using `add-hook'.
The `flag' indicates whether to enable or disable `oops-mode'."
  (interactive)
  (message "Yet ready...")
  )

(provide 'oops-mode)
