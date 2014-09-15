;; Copyright (C) 2014
;;
;; Author: BoyW165
;; Version: 0.0.1
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is a framework that refers to the point and show useful information.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-10-01 (0.0.1)
;;    Initial release.

;; Front-ends.
(require 'sos-basic-frontend)

;; Back-ends.
(require 'sos-grep-backend)
(require 'sos-elisp-backend)
(require 'sos-semantic-backend)

(require 'history)

(defgroup sos-group nil
  "A utility to show you documentation at button window by finding some 
meaningful information around the point."
  :tag "Sos")

(defcustom sos-frontends '(sos-definition-buffer-frontend
                           sos-tips-frontend)
  "The list of front-ends for the purpose of visualization.

`:init': When the visualization should be initialized.

`:show': When the visualization should be showed.

`:hide': When the visualization should be hidden.

`:destroy': When the visualization should be destroied.

`:update': When the data has been updated."
  :type '(repeat (symbol :tag "Front-end"))
  :group 'sos-group)

(defcustom sos-backends '(sos-grep-backend
                          sos-elisp-backend)
  "The list of back-ends for the purpose of collecting candidates. The sos 
engine will dispatch all the back-ends and pass specific commands in order. 
Every command has its purpose, paremeter rule and return rule (get meaningful 
symbol name around the point, find candidates refer to a symbol name). By 
passing command and get return data from a back-end, the sos engine gets 
information to show the result to another window, minibuffer or popup a GUI 
dialog, etc. Be aware, not every back-ends will be dispatched. If a back-end 
return candidates to sos engine, it inform the sos engine that there's no need 
to dispatch remaining back-ends.

### The sample of a back-end:

  (defun some-backend (command &rest args)
    (case command
      (:init t)
      (:symbol (and (member major-mode MAJOR_MODE_CANDIDATES)
                    (thing-at-point 'symbol))))
      (:candidates (list STRING01 STRING02 STRING03 ...))
      (:tips STRING))

Each back-end is a function that takes a variable number of arguments. The
first argument is the command requested from the sos enine.  It is one of
the following:

### The order of the commands to be called by sos engine, begins from top to down:

`:init': Called once for each buffer. The back-end can check for external
programs and files and load any required libraries.  Raising an error here
will show up in message log once, and the back-end will not be used for
completion.

`:symbol': The back-end should return a symbol, nil or 'stop.
- Return a symbol tells sos engine that the back-end will take charge current task. It 
also tells sos engine don't iterate the following back-ends.
- Return nil tells sos engine to skip the back-end.
- Return `:stop' tells sos engine to stop iterating the following back-ends.
- Return value will be cached to `sos-symbol'.

`:candidates': The back-end should return a $CANDIDATES list or nil.
Return a list tells sos engine where the definition is and it must be a list
even if there's only one candidate. It also tells sos engine don't iterate the
following back-ends.
Return nil tells sos engine it cannot find any definition and stop iterating
the following back-ends.
Return value will be cached to `sos-candidates'.

 $CANDIDATES format (alist):
 ### If candidate is a file...
 ((:file STRING
   :linum INTEGER
   :type STRING
   :hl-word STRING
   :mode-line STRING) (...) ...)

 ### If candidate is a document string...
 ((:doc STRING
   :linum INTEGER
   :type STRING
   :hl-word STRING
   :mode-line STRING) (...) ...)

### Optional commands:
`:tips': ."
  :type '(repeat (symbol :tag "Back-end"))
  :group 'sos-group)

(defcustom sos-idle-delay 0.15
  "The idle delay in seconds until sos starts automatically."
  :type '(number :tag "Seconds"))

(defvar sos-timer nil
  "The idle timer to call `sos-idle-begin'.")

(defvar sos-source-buffer nil
  "The current source code buffer.")

(defvar sos-source-window nil
  "The current window where the source code buffer is at.")

(defvar sos-candidates-stack nil
  "A list containing `sos-candidates'. Engine will push the current candidates 
into the stack when user navigate to deeper definition in the definition window.")

(defvar sos-backend nil
  "The back-end which takes control of current session in the back-ends list.")
(make-variable-buffer-local 'sos-backend)

(defvar sos-symbol nil
  "Cache the return value from back-end with `:symbol' command.")
(make-variable-buffer-local 'sos-symbol)

(defvar sos-candidates nil
  "Cache the return value from back-end with `:candidates' command.")
(make-variable-buffer-local 'sos-candidates)

(defvar sos-index 0
  "The index of current candidate in the list.")
(make-variable-buffer-local 'sos-index)

(defvar sos-tips nil
  "Cache the return value from back-end with `:tips' command.")
(make-variable-buffer-local 'sos-tips)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro sos-get-local (symb)
  "Get buffer-localed variable of source code buffer. e.g. `sos-backend', 
`sos-symbol', `sos-candidates', `sos-index' and `sos-tips'."
  `(progn
     (and sos-source-buffer
          (with-current-buffer sos-source-buffer
            ,symb))))

(defmacro sos-set-local (symb value)
  "Set buffer-localed variable of source code buffer. e.g. `sos-backend', 
`sos-symbol', `sos-candidates', `sos-index' and `sos-tips'."
  `(progn
     (and sos-source-buffer
          (with-current-buffer sos-source-buffer
            (setq ,symb ,value)))))

(defun sos-is-skip-command (&rest commands)
  "Return t if `this-command' should be skipped.
If you want to skip additional commands, try example:

  (sos-is-skip-command 'self-insert-command 'previous-line 'next-line
                       'left-char right-char)"
  (memq this-command `(mwheel-scroll
                       save-buffer
                       eval-buffer
                       eval-last-sexp
                       ;; Additional commands.
                       ,@commands)))

(defun sos-is-multiple-candidates ()
  (> (length (sos-get-local sos-candidates)) 1))

(defun sos-push-candidates-stack ()
  (push (sos-get-local sos-candidates) sos-candidates-stack))

(defun sos-pop-candidates-stack ()
  (pop sos-candidates-stack))

(defun sos-pre-command ()
  (when sos-timer
    (cancel-timer sos-timer)
    (setq sos-timer nil)))

(defun sos-post-command ()
  (when (sos-is-idle-begin)
    (setq sos-source-buffer (current-buffer)
          sos-source-window (selected-window))
    (setq sos-timer (run-with-timer sos-idle-delay nil
                                    'sos-idle-begin))))

(defun sos-is-idle-begin ()
  (not (or (active-minibuffer-window)
           (sos-is-defintion-buffer&window)
           (sos-is-skip-command))))

(defun sos-idle-begin ()
  (condition-case err
      (progn
        (if (null sos-backend)
            (sos-1st-process)
          (sos-normal-process sos-backend)))
    (error err)))

(defun sos-1st-process ()
  (dolist (backend sos-backends)
    (sos-normal-process backend)
    (if sos-backend
        (return t)
      (sos-call-frontends :hide))))

(defun sos-normal-process (backend)
  (let ((symb (sos-call-backend backend :symbol)))
    (cond
     ;; Return `:stop' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq symb :stop)
      ;; (message "(%s) sos-normal-process: stop" (current-time))
      (setq sos-backend backend)
      (sos-call-frontends :hide))

     ;; Return nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((null symb)
      ;; (message "(%s) sos-normal-process: hide" (current-time))
      (sos-call-frontends :hide))

     ;; Something ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (t
      (if (and (eq symb sos-symbol)
               (eq (current-buffer) sos-source-buffer)
               (eq (selected-window) sos-source-window))
          (progn
            ;; If return symbol string is equal to `sos-symbol', ask front-ends
            ;; to do `:update' task.
            ;; (message "(%s) sos-normal-process: update" (current-time))
            (setq sos-tips (sos-call-backend backend :tips symb))
            (sos-call-frontends :update))
        (setq sos-backend backend
              sos-symbol symb
              sos-candidates (sos-call-backend backend :candidates symb)
              sos-tips (sos-call-backend backend :tips symb))
        (if (and sos-candidates (listp sos-candidates))
            (progn
              ;; (message "(%s) sos-normal-process: show" (current-time))
              (sos-call-frontends :show))
          ;; (message "(%s) sos-normal-process: hide" (current-time))
          (sos-call-frontends :hide)))))))

(defun sos-call-frontends (command &rest args)
  "Iterate all the `sos-backends' and pass `command' by order."
  (let ((commands (cons command args)))
    (dolist (frontend sos-frontends)
      (dolist (cmd commands)
        (funcall frontend cmd)))))

(defun sos-call-backend (backend command &optional arg)
  "Call certain backend `backend' and pass `command' to it."
  (funcall backend command arg))

(defun sos-init-backend (backend)
  (funcall backend :init))

;;;###autoload
(defun sos-goto-definition ()
  (interactive)
  ;; TODO:
  (and (featurep 'history)
       (his-add-position-type-history)))

;;;###autoload
(define-minor-mode sos-definition-window-mode
  "This local minor mode gethers symbol returned from backends around the point 
and show the reference visually through frontends. Usually frontends output the 
result to the `sos-def-buf' displayed in the `sos-def-win'."
  :lighter " SOS:def"
  :global t
  :group 'sos-group
  ;; TODO: menu-bar and tool-bar keymap.
  (if sos-definition-window-mode
      (progn
        (mapc 'sos-init-backend sos-backends)
        (sos-call-frontends :init)
        (add-hook 'pre-command-hook 'sos-pre-command)
        (add-hook 'post-command-hook 'sos-post-command))
    (sos-call-frontends :destroy)
    (remove-hook 'pre-command-hook 'sos-pre-command)
    (remove-hook 'post-command-hook 'sos-post-command)))

;;;###autoload
(define-minor-mode sos-outline-window-mode
  "This local minor mode gethers symbol returned from backends around the point 
and show the reference visually through frontends. Usually frontends output the 
result to the `sos-definition-buffer' displayed in the `sos-definition-window'. 
Show or hide these buffer and window are controlled by `sos-watchdog-mode'."
  :lighter " SOS:outline"
  :global t
  :group 'sos-group
  ;; TODO: menu-bar and tool-bar keymap.
  (if sos-outline-window-mode
      (progn
        nil)
    nil) )

(provide 'sos)
