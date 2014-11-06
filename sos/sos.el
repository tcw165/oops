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
;; 2014-11-01 (0.0.1)
;;    Initial release.

(require 'history)

(and load-file-name
     (let ((dir (file-name-directory load-file-name)))
       (add-to-list 'load-path (concat dir "/frontends"))
       (add-to-list 'load-path (concat dir "/backends"))))

;; Default frontends.
(require 'sos-default-frontend)
;; Default backends.
(require 'sos-elisp-backend)
(require 'sos-jedi-backend)
(require 'sos-cc++-backend)
(require 'sos-grep-backend)
(require 'sos-candidates-preview-backend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup sos-group nil
  "A utility to show you documentation at button window by finding some 
meaningful information around the point."
  :tag "Sos")

(defcustom sos-backends '(sos-elisp-backend
                          sos-jedi-backend
                          sos-cc++-backend
                          sos-grep-backend
                          sos-candidates-preview-backend)
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

  (defun some-backend (command &optional arg)
    (case command
      (:init t)
      (:symbol (and (member major-mode MAJOR_MODE_CANDIDATES)
                    (thing-at-point 'symbol))))
      (:candidates (list STRING01 STRING02 STRING03 ...)))

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

`:candidates': The back-end should return a CANDIDATE list or nil.
Return a list tells sos engine where the definition is and it must be a list
even if there's only one candidate. It also tells sos engine don't iterate the
following back-ends.
Return nil tells sos engine it cannot find any definition and stop iterating
the following back-ends.

 CANDIDATE is an alist. There are must-have properties and optional properties:
 ### Must-Have Properties:
 `:doc': Buffer text which is the file content (buffer-string).

 `:file': The absolute file path.

 `:linum': The line number (integer).

 ### Optional Properties:
 `:keywords': A list containing elements to highlight keywords. Its format is 
              `font-lock-keywords'. The 1st matcher must be for the symbol 
              definition.

 `:symbol': The symbol's name.

 `:type': The String which describe the symbol.

 `:show': t indicates front-ends show this CANDIDATE by default.

   ### Example - File Candidate:
   ((:symbol STRING
     :doc BUFFER_STRING
     :type STRING
     :file STRING
     :linum INTEGER
     :match REGEXP) ...)

   ### Example - Document Candidate:
   ((:symbol STRING
     :doc BUFFER_STRING
     :type STRING
     :linum INTEGER) ...)"
  :type '(repeat (symbol :tag "Back-end"))
  :group 'sos-group)

(defvar sos-symbol nil
  "Cache the return value from back-end with `:symbol' command.")
(make-variable-frame-local 'sos-symbol)

(defvar sos-backend nil
  "The back-end which takes control of current session in the back-ends list.")
(make-variable-buffer-local 'sos-backend)

(defun sos-call-backend (backend command &rest args)
  "Call certain backend `backend' and pass `command' to it."
  (apply backend command args))

(defun sos-init-backend (backend)
  (sos-call-backend backend :init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition Window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sos-definition-window-frontends '(sos-definition-buffer-frontend)
  "The list of front-ends for the purpose of visualization.

### Commands:
`:init'     - When the visualization should be initialized.
`:destroy'  - When the visualization should be destroied.
`:show'     - When the visualization should be showed.
`:hide'     - When the visualization should be hidden.
`:update'   - When the data has been updated.

### The sample of a front-end:
  (defun some-frontend (command &rest args)
    (case command
      (:init t)
      (:show (message PROMPTY)))
      (:hide ...)
      (:destroy ...))
"
  :type '(repeat (symbol :tag "Front-end"))
  :group 'sos-group)

(defcustom sos-idle-delay 0.3
  "The idle delay in seconds until sos starts automatically."
  :type '(number :tag "Seconds"))

(defvar sos-timer nil
  "The idle timer to call `sos-idle-begin'.")

(defvar sos-source-buffer nil
  "The current source code buffer.")

(defvar sos-source-window nil
  "The current window where the source code buffer is at.")

(defvar sos-is-skip nil
  "t to skip `sos-idle-begin'.")
(make-variable-buffer-local 'sos-is-skip)

(defun sos-call-def-win-frontends (command &rest args)
  (dolist (frontend sos-definition-window-frontends)
    (apply frontend command args)))

(defun sos-def-win-normal-process (backend)
  (let ((symb (sos-call-backend backend :symbol)))
    (cond
     ;; Return `:stop' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq symb :stop)
      (setq sos-backend backend
            sos-symbol nil)
      (sos-call-def-win-frontends :hide))

     ;; Return nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((null symb)
      (setq sos-symbol nil)
      (sos-call-def-win-frontends :hide))

     ;; Something ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (t
      (if (equal symb sos-symbol)
          (sos-call-def-win-frontends :update)
        (setq sos-backend backend
              sos-symbol symb)
        (let ((candidates (sos-call-backend sos-backend :candidates symb)))
          (if candidates
              (cond
               ((listp candidates)
                (sos-call-def-win-frontends :show candidates))
               ;; Load `deferred' module at runtime.
               ((and (require 'deferred) (deferred-p candidates))
                (deferred:nextc
                  candidates
                  (lambda (real-candidates)
                    (sos-call-def-win-frontends :show real-candidates)))))
            (sos-call-def-win-frontends :hide))))))))

(defun sos-def-win-1st-process ()
  (dolist (backend sos-backends)
    (sos-def-win-normal-process backend)
    (if sos-backend
        (return t)
      (sos-call-def-win-frontends :hide))))

(defun sos-is-skip-command (&rest commands)
  "Return t if `this-command' should be skipped.
If you want to skip additional commands, try example:
  (sos-is-skip-command 'self-insert-command 'previous-line ...)"
  (memq this-command `(mwheel-scroll
                       save-buffer
                       eval-buffer
                       eval-last-sexp
                       ;; Additional commands.
                       ,@commands)))

(defun sos-is-idle-begin ()
  (not (or (active-minibuffer-window)
           (sos-is-skip-command)
           sos-is-skip)))

(defun sos-idle-begin ()
  (when (sos-is-idle-begin)
    (setq sos-source-buffer (current-buffer)
          sos-source-window (selected-window))
    (condition-case err
        (if (null sos-backend)
            (sos-def-win-1st-process)
          (sos-def-win-normal-process sos-backend))
      (error err))))

;;;###autoload
(define-minor-mode sos-definition-window-mode
  "This local minor mode gethers symbol returned from backends around the point 
and show the reference visually through frontends. Usually frontends output the 
result to the `sos-def-buf' displayed in the `sos-def-win'."
  :global t
  :group 'sos-group
  ;; TODO: menu-bar and tool-bar keymap.
  (if sos-definition-window-mode
      (progn
        ;; Initialize front-ends & back-ends.
        (sos-call-def-win-frontends :init)
        (mapc 'sos-init-backend sos-backends)
        (setq sos-timer (run-with-idle-timer sos-idle-delay t 'sos-idle-begin)))
    ;; Destroy front-ends & back-ends.
    (sos-call-def-win-frontends :destroy)
    (when sos-timer
      (cancel-timer sos-timer)
      (setq sos-timer nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outline Window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode sos-outline-window-mode
  ""
  :global t
  :group 'sos-group
  ;; TODO: menu-bar and tool-bar keymap.
  (if sos-outline-window-mode
      (progn
        nil)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to Definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sos-goto-definition-frontend 'sos-goto-definitions-frontend
  "The list of front-ends for the purpose of visualization.

### Commands:
`:show': When the visualization should be showed. The 1st argument is the 
         candidates."
  :type '(symbol :tag "Front-end")
  :group 'sos-group)

(defun sos-call-goto-def-frontends (command &rest arg)
  "Iterate all the `sos-backends' and pass `command' by order."
  (apply sos-goto-definition-frontend command arg))

(defun sos-goto-def-normal-process (backend)
  (let ((symb (sos-call-backend backend :symbol)))
    (cond
     ;; Return `:stop' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq symb :stop)
      (setq sos-backend backend))

     ;; Return nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((null symb))

     ;; Something ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (t
      (setq sos-backend backend)
      (let ((candidates (sos-call-backend sos-backend :candidates symb)))
        (and candidates
             (cond
              ((listp candidates)
               (sos-call-goto-def-frontends :show candidates))
              ;; Load `deferred' module at runtime.
              ((and (require 'deferred) (deferred-p candidates))
               (deferred:nextc
                 candidates
                 (lambda (real-candidates)
                   (sos-call-goto-def-frontends :show real-candidates)))))))))))

(defun sos-goto-def-1st-process ()
  (dolist (backend sos-backends)
    (sos-goto-def-normal-process backend)
    (if sos-backend
        (return t))))

;;;###autoload
(defun sos-goto-definition ()
  (interactive)
  (if (null sos-backend)
      (sos-goto-def-1st-process)
    (sos-goto-def-normal-process sos-backend)))

;;;###autoload
(defun sos-goto-local-symbol ()
  (interactive)
  ;; TODO:
  )

;;;###autoload
(defun sos-goto-global-symbol ()
  (interactive)
  ;; TODO:
  )

(provide 'sos)
