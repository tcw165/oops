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

  (defun some-backend (cmd &optional arg &rest ign)
    (if (equal major-mode 'xxx-mode)
        (thing-at-point 'symbol)
      nil))

Each back-end is a function that takes a variable number of arguments. The
first argument is the command requested from the sos enine.  It is one of
the following:

### The order of the commands to be called by sos engine, begins from top to down:

`init': Called once for each buffer. The back-end can check for external
programs and files and load any required libraries.  Raising an error here
will show up in message log once, and the back-end will not be used for
completion.

`thing': The back-end should return a string, nil or 'stop.
Return a string which represents a symbol name tells sos engine that the back
-end will take charge current task. The back-end collect the string around the
point and produce a meaningful symbol name. It also tells sos engine don't
iterate the following back-ends.
Return nil tells sos engine to skip the back-end.
Return 'stop tells sos engine to stop iterating the following back-ends.

`candidates': The back-end should return a list or nil.
Return a list tells sos engine where the definition is and it must be a list
even if there's only one candidate. It also tells sos engine don't iterate the
following back-ends.
Return nil tells sos engine it cannot find any definition and stop iterating
the following back-ends.

`meta': The back-end should return a string or nil. The return string represents 
a documentation for a completion candidate. The second argument is a candidate. 
The sos engine will iterate the candidates and ask for each candidate its `meta'."
  :type '(repeat (symbol :tag "Back-end"))
  :group 'sos-group)

(defcustom sos-idle-delay 0.5
  "The idle delay in seconds until sos starts automatically."
  :type '(number :tag "Seconds"))

(defvar sos-timer nil)

(defvar sos-window nil)

(defvar sos-buffer nil)

(defvar sos-candidates nil)
(make-variable-buffer-local 'sos-candidates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sos-call-frontends (command)
  (dolist (frontend sos-frontends)
    (condition-case err
        (funcall frontend command)
      (error "[sos] Front-end %s error \"%s\" on command %s"
             frontend (error-message-string err) command))))

(defun sos-init-backend (backend)
  (condition-case err
      (progn
        (funcall backend 'init)
        (put backend 'sos-init t))
    (put backend 'sos-init 'failed)
    (error "[sos] Back-end %s error \"%s\" on command %s"
           backend (error-message-string err) 'init)))

(defun sos-pre-command ()
  (sos-call-frontends 'pre-command)
  (when sos-timer
    (cancel-timer sos-timer)
    (setq sos-timer nil)))

(defun sos-post-command ()
  (setq sos-timer (run-with-timer sos-idle-delay nil
                                  'sos-idle-begin
                                  (current-buffer) (selected-window)
                                  (buffer-chars-modified-tick) (point))))

(defun sos-idle-begin (buf win tick pos)
  (and (eq buf (current-buffer))
       (eq win (selected-window))
       (eq tick (buffer-chars-modified-tick))
       (eq pos (point))
       ;; TODO: call `sos-begin' to do the job.
       (message "%s" 'sos-idle-begin)
       ;; (when (company-auto-begin)
       ;;   (company-input-noop)
       ;;   (company-post-command))
       ))

(defun sos-toggle-candidates-window (toggle)
  ;; TODO: create sos buffer and sos candidates buffer.
  (let ((enabled (or (and (booleanp toggle) toggle)
                     (and (numberp toggle)
                          (> toggle 0))))
        (win (cond
              ;; Only one window.
              ((window-live-p (frame-root-window))
               (selected-window))))
        (h (/ (window-total-height) -3)))
    (if enabled
        (unless sos-window
          (setq sos-window (split-window win h 'below)))
      (and (window-valid-p sos-window)
           (delete-window sos-window))
      (setq sos-window nil))))

;;;###autoload
(define-minor-mode sos-mode
  ""
  :lighter " sos"
  (if sos-mode
      (progn
        (add-hook 'pre-command-hook 'sos-pre-command nil t)
        (add-hook 'post-command-hook 'sos-post-command nil t)
        (mapc 'sos-init-backend sos-backends)
        (sos-toggle-candidates-window 1))
    (remove-hook 'pre-command-hook 'sos-pre-command t)
    (remove-hook 'post-command-hook 'sos-post-command t)
    (sos-toggle-candidates-window -1)))

(provide 'sos)
