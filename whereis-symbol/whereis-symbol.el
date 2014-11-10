;;; whereis-symbol.el --- Smartly detect symbol at point and show symbol's whence in an isolated window.
;;
;; Copyright (C) 2014
;;
;; Author: boyw165
;; Version: 0.0.1
;; Compatibility: GNU Emacs 24.3+
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
;; 2014-10-31 (0.0.x)
;; * Support asynchronous backends which use `deferred' library.
;;
;; 2014-08-01 (0.0.1)
;; * Initial release which was inspired by `company-mode'.
;;   https://github.com/company-mode/company-mode
;;
;;; Code:

;; GNU library.
(eval-when-compile (require 'cl))

;; whereis-symbol's library.
(and load-file-name
     (let ((dir (file-name-directory load-file-name)))
       (add-to-list 'load-path (concat dir "/frontends"))
       (add-to-list 'load-path (concat dir "/backends"))))
;; Default frontends.
(require 'ws-default-frontend)
;; Default backends.
(require 'ws-elisp-backend)
(require 'ws-jedi-backend)
(require 'ws-cc++-backend)
(require 'ws-grep-backend)
(require 'ws-candidates-preview-backend)

(defgroup whereis-symbol nil
  "A utility to show you documentation at button window by finding some 
meaningful information around the point.")

(defcustom ws-backends '(ws-elisp-backend
                         ws-jedi-backend
                         ws-cc++-backend
                         ws-grep-backend
                         ws-candidates-preview-backend)
  "The list of back-ends for the purpose of collecting candidates. The sos 
engine will dispatch all the back-ends and pass specific commands in order. 
Every command has its purpose, paremeter rule and return rule (get meaningful 
symbol name around the point, find candidates refer to a symbol name). By 
passing command and get return data from a back-end, the sos engine gets 
information to show the result to another window, minibuffer or popup a GUI 
dialog, etc. Be aware, not every back-ends will be dispatched. If a back-end 
return candidates to sos engine, it inform the sos engine that there's no need 
to dispatch remaining back-ends.

Example:
--------
  (defun some-backend (command &optional arg)
    (case command
      (:init t)
      (:symbol (and (member major-mode MAJOR_MODE_CANDIDATES)
                    (thing-at-point 'symbol))))
      (:candidates (list STRING01 STRING02 STRING03 ...)))

Each back-end is a function that takes a variable number of arguments. The
first argument is the command requested from the sos enine.  It is one of
the following:

Commands:
---------
`:init': Called once for each buffer. The back-end can check for external
programs and files and load any required libraries.  Raising an error here
will show up in message log once, and the back-end will not be used for
completion.

`:symbol': The back-end should return a symbol, nil or 'stop.
- Return a symbol tells sos engine that the back-end will take charge current 
task. It also tells sos engine don't iterate the following back-ends.
- Return nil tells sos engine to skip the back-end.
- Return `:stop' tells sos engine to stop iterating the following back-ends.
- Return value will be cached to `ws-symbol'.

`:symbol-completion': ...

`:candidates': The back-end should return a CANDIDATE list or nil.
Return a list tells sos engine where the definition is and it must be a list
even if there's only one candidate. It also tells sos engine don't iterate the
following back-ends.
Return nil tells sos engine it cannot find any definition and stop iterating
the following back-ends.

  CANDIDATE is an alist!

  Keys:
  -----------
  `:doc': Buffer text which is the file content (buffer-string).
  or
  `:file': The absolute file path.
  Note: `:doc' and `:file' are exclusive.

  `:linum': The line number (integer).

  Optional Keys:
  --------------------
  `:keywords': A list containing elements to highlight keywords. Its format is 
               `font-lock-keywords'. The 1st matcher must be for the symbol 
               definition.

  `:symbol': The symbol's name.

  `:type': The String which describe the symbol.

  `:show': t indicates front-ends show this CANDIDATE by default.

  Example - File Candidates:
  --------------------------
  ((:symbol STRING
    :doc BUFFER_STRING
    :type STRING
    :file STRING
    :linum INTEGER
    :match REGEXP) ...)

  Example - Document Candidates:
  ------------------------------
  ((:symbol STRING
    :doc BUFFER_STRING
    :type STRING
    :linum INTEGER) ...)"
  :type '(repeat (symbol :tag "Back-end"))
  :group 'whereis-symbol)

(defvar ws-symbol nil
  "Cache the return value from back-end with `:symbol' command.")
(make-variable-frame-local 'ws-symbol)

(defvar ws-backend nil
  "The back-end which takes control of current session in the back-ends list.")
(make-variable-buffer-local 'ws-backend)

(defun ws-call-backend (backend command &rest args)
  "Call certain backend `backend' and pass `command' to it."
  (apply backend command args))

(defun ws-init-backend (backend)
  (ws-call-backend backend :init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Where Is Symbol Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom ws-frontends '(ws-definition-window-frontend)
  "The list of front-ends for the purpose of visualization.

Commands:
---------
`:init'     - When the visualization should be initialized.
`:destroy'  - When the visualization should be destroied.
`:show'     - When the visualization should be showed.
`:hide'     - When the visualization should be hidden.
`:update'   - When the data has been updated.

Example:
--------
  (defun some-frontend (command &rest args)
    (case command
      (:init t)
      (:show (message PROMPTY)))
      (:hide ...)
      (:destroy ...))"
  :type '(repeat (symbol :tag "Front-end"))
  :group 'whereis-symbol)

(defcustom ws-idle-delay 0.3
  "The idle delay in seconds until sos starts automatically."
  :type '(number :tag "Seconds"))

(defvar ws-timer nil
  "The idle timer to call `ws-idle-begin'.")

(defvar ws-source-buffer nil
  "The current source code buffer.")

(defvar ws-source-window nil
  "The current window where the source code buffer is at.")

(defvar ws-is-skip nil
  "t to skip `ws-idle-begin'.")
(make-variable-buffer-local 'ws-is-skip)

(defun ws-call-frontends (command &rest args)
  (dolist (frontend ws-frontends)
    (apply frontend command args)))

(defun ws-normal-process (backend)
  (let ((symb (ws-call-backend backend :symbol)))
    (cond
     ;; Return `:stop' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq symb :stop)
      (setq ws-backend backend
            ws-symbol nil)
      (ws-call-frontends :hide))

     ;; Return nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((null symb)
      (setq ws-symbol nil)
      (ws-call-frontends :hide))

     ;; Something ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (t
      (if (equal symb ws-symbol)
          (ws-call-frontends :update)
        (setq ws-backend backend
              ws-symbol symb)
        (let ((candidates (ws-call-backend ws-backend :candidates symb)))
          (if candidates
              (cond
               ((listp candidates)
                (ws-call-frontends :show candidates))
               ;; Load `deferred' module at runtime.
               ((and (require 'deferred) (deferred-p candidates))
                (deferred:nextc
                  candidates
                  (lambda (real-candidates)
                    (ws-call-frontends :show real-candidates)))))
            (ws-call-frontends :hide))))))))

(defun ws-1st-process ()
  (dolist (backend ws-backends)
    (ws-normal-process backend)
    (if ws-backend
        (return t)
      (ws-call-frontends :hide))))

(defun ws-is-skip-command (&rest commands)
  "Return t if `this-command' should be skipped.
If you want to skip additional commands, try example:
  (ws-is-skip-command 'self-insert-command 'previous-line ...)"
  (memq this-command `(mwheel-scroll
                       save-buffer
                       eval-buffer
                       eval-last-sexp
                       ;; Additional commands.
                       ,@commands)))

(defun ws-is-idle-begin ()
  (not (or (active-minibuffer-window)
           (ws-is-skip-command)
           ws-is-skip)))

(defun ws-idle-begin ()
  (when (ws-is-idle-begin)
    (setq ws-source-buffer (current-buffer)
          ws-source-window (selected-window))
    (condition-case err
        (if (null ws-backend)
            (ws-1st-process)
          (ws-normal-process ws-backend))
      (error (message "whereis-symbol-mode error: %s" err)))))

;;;###autoload
(define-minor-mode whereis-symbol-mode
  "This local minor mode gethers symbol returned from backends around the point 
and show the reference visually through frontends. Usually frontends output the 
result to the `ws-def-buf' displayed in the `ws-def-win'."
  :global t
  :group 'whereis-symbol
  ;; TODO: menu-bar and tool-bar keymap.
  (if whereis-symbol-mode
      (progn
        ;; Initialize front-ends & back-ends.
        (ws-call-frontends :init)
        (mapc 'ws-init-backend ws-backends)
        (setq ws-timer (run-with-idle-timer ws-idle-delay t 'ws-idle-begin)))
    ;; Destroy front-ends & back-ends.
    (ws-call-frontends :destroy)
    (when ws-timer
      (cancel-timer ws-timer)
      (setq ws-timer nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to Definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom ws-goto-frontend 'ws-goto-definitions-frontend
  "The list of front-ends for the purpose of visualization.

Commands:
---------
`:show': When the visualization should be showed. The 1st argument is the 
         candidates."
  :type '(symbol :tag "Front-end")
  :group 'whereis-symbol)

(defcustom ws-before-goto-hook nil
  ""
  :type '(repeat function)
  :group 'whereis-symbol)

(defcustom ws-after-goto-hook nil
  ""
  :type '(repeat function)
  :group 'whereis-symbol)

(defun ws-call-goto-frontends (command &rest arg)
  "Iterate all the `ws-backends' and pass `command' by order."
  (apply ws-goto-frontend command arg))

(defun ws-goto-normal-process (backend)
  (let ((symb (ws-call-backend backend :symbol)))
    (cond
     ;; Return `:stop' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq symb :stop)
      (setq ws-backend backend))

     ;; Return nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((null symb))

     ;; Something ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (t
      (setq ws-backend backend)
      (let ((candidates (ws-call-backend ws-backend :candidates symb)))
        (when candidates
          (run-hooks ws-before-goto-hook)
          (cond
           ((listp candidates)
            (ws-call-goto-frontends :show candidates))
           ;; Load `deferred' module at runtime.
           ((and (require 'deferred) (deferred-p candidates))
            (deferred:nextc
              candidates
              (lambda (real-candidates)
                (ws-call-goto-frontends :show real-candidates)))))
          (run-hooks ws-after-goto-hook)))))))

(defun ws-goto-1st-process ()
  (dolist (backend ws-backends)
    (ws-goto-normal-process backend)
    (if ws-backend
        (return t))))

;;;###autoload
(defun ws-goto-definition ()
  (interactive)
  (and (ws-is-idle-begin)
       (condition-case err
           (if (null ws-backend)
               (ws-goto-1st-process)
             (ws-goto-normal-process ws-backend))
         (error (message "ws-goto-definition error: %s" err)))))

;;;###autoload
(defun ws-goto-local-symbol ()
  (interactive)
  ;; TODO:
  )

;;;###autoload
(defun ws-goto-global-symbol ()
  (interactive)
  ;; TODO:
  )

(provide 'whereis-symbol)
