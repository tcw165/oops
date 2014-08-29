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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-10-01 (0.0.1)
;;    Initial release.

(require 'sos-elisp)
(require 'sos-semantic)

(defgroup sos-group nil
  "A utility to show you documentation at button window by finding some 
meaningful information around the point."
  :tag "Sos")

(defcustom sos-frontends '()
  "The list of front-ends for the purpose of visualization.

`show': When the visualization should start.

`hide': When the visualization should end.

`update': When the data has been updated.

`pre-command': Before every command that is executed while the
visualization is active.

`post-command': After every command that is executed while the
visualization is active."
  :type '(repeat (symbol :tag "Front-end"))
  :group 'sos-group)

(defcustom sos-backends '(sos-elisp-backend
                          sos-semantic-backend)
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

  (defun some-backend (command &optional arg &rest ign)
    (case command
      (:init t)
      (:symbol (and (member major-mode MAJOR_MODE_CANDIDATES)
                    (thing-at-point 'symbol))))
      (:candidates (list STRING01 STRING02 STRING03 ...))
      (:tips TIPS)
      (:no-cache t))

Each back-end is a function that takes a variable number of arguments. The
first argument is the command requested from the sos enine.  It is one of
the following:

### The order of the commands to be called by sos engine, begins from top to down:

`:init': Called once for each buffer. The back-end can check for external
programs and files and load any required libraries.  Raising an error here
will show up in message log once, and the back-end will not be used for
completion.

`:symbol': The back-end should return a string, nil or 'stop.
Return a string which represents a symbol name tells sos engine that the back
-end will take charge current task. The back-end collect the string around the
point and produce a meaningful symbol name. It also tells sos engine don't
iterate the following back-ends.
Return nil tells sos engine to skip the back-end.
Return `:stop' tells sos engine to stop iterating the following back-ends.

`:candidates': The back-end should return a CANDIDATES list or nil.
Return a list tells sos engine where the definition is and it must be a list
even if there's only one candidate. It also tells sos engine don't iterate the
following back-ends.
Return nil tells sos engine it cannot find any definition and stop iterating
the following back-ends.

 CANDIDATES format (alist):
 ((:file FILE
   :offset OFFSET
   :highlight HIGHLIGHT) ...)

 FILE: A string which indicates the absolute path of the source file.

 OFFSET: A integer which indicates the location of the symbol in the source file.

sample:
  TODO: sample

### Optional commands (no sequent order):

`:tips': The back-end should return a string or nil. The return string represents 
a documentation for a completion candidate. The second argument is a candidate. 
The sos engine will iterate the candidates and ask for each candidate its `tips'.

`:no-cache': Usually sos engine doesn't ask for candidates again as referencing
progresses, unless the back-end returns t for this command.  The second argument 
is the latest `symbol'."
  :type '(repeat (symbol :tag "Back-end"))
  :group 'sos-group)

(defcustom sos-idle-delay 0.5
  "The idle delay in seconds until sos starts automatically."
  :type '(number :tag "Seconds"))

(defconst sos-buffer-name "*Reference*")

(defvar sos-timer nil)

(defvar sos-buffer nil)

(defvar sos-window nil)

(defvar sos-window-height 0)

(defvar sos-backend nil
  "The backend which takes the task in the `sos-backends' in the current buffer.")
(make-variable-buffer-local 'sos-backend)

(defvar sos-symbol nil
  "Cache the return value from back-end with `symbol' command.")
(make-variable-buffer-local 'sos-symbol)

(defvar sos-candidates nil
  "Cache the return value from back-end with `candidates' command.")
(make-variable-buffer-local 'sos-candidates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sos-toggle-buffer-window (toggle)
  "Display or hide the `sos-buffer' and `sos-window'."
  ;; TODO: modify mode line.
  (let ((enabled (or (and (booleanp toggle) toggle)
                     (and (numberp toggle)
                          (> toggle 0)))))
    (if enabled
        (progn
          (setq sos-buffer (get-buffer-create sos-buffer-name))
          (when (or (not sos-window)
                    (not (window-live-p sos-window)))
            (let* ((win (cond
                         ;; Only one window.
                         ((window-live-p (frame-root-window))
                          (selected-window))))
                   (height (or (and (> sos-window-height 0)
                                    (- 0 sos-window-height))
                               (and win
                                    (/ (window-height win) -3)))))
              (and win height
                   (setq sos-window (split-window win height 'below)))))
          ;; Force to apply `sos-buffer' to `sos-window'.
          (set-window-buffer sos-window sos-buffer))
      (when (window-valid-p sos-window)
        (delete-window sos-window))
      (when (buffer-live-p sos-buffer)
        (kill-buffer sos-buffer))
      (setq sos-buffer nil
            sos-window nil))))

(defun sos-watchdog-post-command ()
  (when (and (not (eq (selected-window) sos-window))
             (not (active-minibuffer-window)))
    (if sos-mode
        (progn
          ;; Show them.
          (sos-toggle-buffer-window 1)
          ;; Save `sos-window' height.
          (setq sos-window-height (window-height sos-window)))
      ;; Hide them.
      (sos-toggle-buffer-window -1))))

;;;###autoload
(define-minor-mode sos-watchdog-mode
  "A global minor mode which refers to buffer's `sos-mode' to show the `sos-buffer'
 and `sos-window' or hide them. Show them if `sos-mode' is t; Hide if nil."
  :global t
  (if sos-watchdog-mode
      (add-hook 'post-command-hook 'sos-watchdog-post-command t)
    (remove-hook 'post-command-hook 'sos-watchdog-post-command t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sos-pre-command ()
  (sos-call-frontends 'pre-command)
  (when sos-timer
    (cancel-timer sos-timer)
    (setq sos-timer nil)))

(defun sos-post-command ()
  ;; Call `sos-idle-begin'.
  (and (sos-post-command-check)
       (setq sos-timer (run-with-timer sos-idle-delay nil
                                       'sos-idle-begin
                                       (current-buffer) (selected-window)
                                       (buffer-chars-modified-tick) (point)))))

(defun sos-post-command-check ()
  t)

(defun sos-idle-begin (buf win tick pos)
  (and (eq buf (current-buffer))
       (eq win (selected-window))
       (eq tick (buffer-chars-modified-tick))
       (eq pos (point))
       ;; TODO: what situation is to call `sos-begin-new-process'.
       ;; TODO: what situation is to call `sos-continue-process'.
       (or (and sos-backend (sos-continue-process))
           (sos-begin-new-process))))

(defun sos-continue-process ()
  ;; TODO: check :no-cache
  ;; TODO: check :tips
  nil)

(defun sos-begin-new-process ()
  (dolist (backend sos-backends)
    (let ((symb (sos-call-backend backend :symbol)))
      (cond
       ((and symb (stringp symb))
        ;; Set `sos-backend'.
        (setq sos-backend backend)
        ;; Get candidates.
        (let ((candidates (sos-call-backend backend :candidates symb)))
          (when (and candidates (listp candidates))
            (setq sos-candidates candidates)))
        (return t))
       ((eq symb :stop)
        ;; Set `sos-backend' but stop iterating following backends.
        (setq sos-backend backend)
        (return t))))))

(defun sos-call-frontends (command)
  (dolist (frontend sos-frontends)
    (condition-case err
        (funcall frontend command)
      (error "[sos] Front-end %s error \"%s\" on command %s"
             frontend (error-message-string err) command))))

(defun sos-call-backend (backend command &rest rest)
  (condition-case err
      (funcall backend command)
    (error "[sos] Back-end %s error \"%s\" on command %s"
           backend (error-message-string err) command)))

(defun sos-init-backend (backend)
  (condition-case err
      (progn
        (funcall backend :init)
        (put backend :init t))
    (put backend :init nil)
    (error "[sos] Back-end %s error \"%s\" on command %s"
           backend (error-message-string err) :init)))

;;;###autoload
(define-minor-mode sos-mode
  "This local minor mode gethers symbol returned from backends around the point 
and show the reference visually through frontends. Usually frontends output the 
result to the `sos-buffer' displayed in the `sos-window'. Show or hide these 
buffer and window are controlled by `sos-watchdog-mode'."
  :lighter " SOS"
  (if sos-mode
      (progn
        (unless sos-watchdog-mode
          (sos-watchdog-mode 1))
        (mapc 'sos-init-backend sos-backends)
        (add-hook 'pre-command-hook 'sos-pre-command nil t)
        (add-hook 'post-command-hook 'sos-post-command nil t))
    (remove-hook 'pre-command-hook 'sos-pre-command t)
    (remove-hook 'post-command-hook 'sos-post-command t)))

(provide 'sos)
