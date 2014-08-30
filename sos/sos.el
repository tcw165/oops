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

;; Default supported back-ends.
(require 'sos-grep)
(require 'sos-elisp)
(require 'sos-semantic)

(defgroup sos-group nil
  "A utility to show you documentation at button window by finding some 
meaningful information around the point."
  :tag "Sos")

(defcustom sos-frontends '(sos-reference-buffer-frontend
                           sos-tips-frontend)
  "The list of front-ends for the purpose of visualization.

`:show': When the visualization should start.

`:hide': When the visualization should end.

`:update': When the data has been updated."
  :type '(repeat (symbol :tag "Front-end"))
  :group 'sos-group)

(defcustom sos-backends '(sos-grep-backend
                          sos-elisp-backend
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

  (defun some-backend (command &rest args)
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
Return value will be cached to `sos-symbol'.

`:candidates': The back-end should return a $CANDIDATES list or nil.
Return a list tells sos engine where the definition is and it must be a list
even if there's only one candidate. It also tells sos engine don't iterate the
following back-ends.
Return nil tells sos engine it cannot find any definition and stop iterating
the following back-ends.
Return value will be cached to `sos-candidates'.

 $CANDIDATES format (alist):
 ((:file FILE
   :offset OFFSET
   :linum LINUM) ...)

 FILE: A string which indicates the absolute path of the source file.

 OFFSET: A integer which indicates the location of the symbol in the source file.

 HIGHLIGHT: A boolean which indicate to highlight the symbol.

sample:
  TODO: sample

### Optional commands (no sequent order):

`:tips': The back-end should return a string or nil. The return string represents 
a documentation for a completion candidate. The second argument is `sos-symbol' 
which is returned from `:symbol' command.
The sos engine will iterate the candidates and ask for each candidate its `tips'."
  :type '(repeat (symbol :tag "Back-end"))
  :group 'sos-group)

(defcustom sos-idle-delay 0.5
  "The idle delay in seconds until sos starts automatically."
  :type '(number :tag "Seconds"))

(defvar sos-timer nil)

(defvar sos-reference-buffer nil)

(defvar sos-reference-window nil)

(defvar sos-reference-window-height 0)

(defvar sos-symbol nil
  "Cache the return value from back-end with `:symbol' command.")
(make-variable-buffer-local 'sos-symbol)

(defvar sos-candidates nil
  "Cache the return value from back-end with `:candidates' command.")
(make-variable-buffer-local 'sos-candidates)

(defvar sos-candidate nil
  "The currently selected candidate for multiple candidates case.")
(make-variable-buffer-local 'sos-candidate)

(defvar sos-tips nil
  "Cache the return value from back-end with `:tips' command.")
(make-variable-buffer-local 'sos-tips)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sos-reference-buffer-frontend (command &rest args)
  (case command
    (:show
     (let ((file (plist-get sos-candidate :file))
           (offset (plist-get sos-candidate :offset))
           (linum (plist-get sos-candidate :linum)))
       (garbage-collect)
       (when (file-exists-p file)
         (with-current-buffer sos-reference-buffer
           (insert-file-contents file nil nil nil t)
           ;; setup major-mode, auto-mode-alist
           (dolist (mode auto-mode-alist)
             (and (not (null (cdr mode)))
                  (string-match (car mode) file)
                  (funcall (cdr mode)))))
         (with-selected-window sos-reference-window
           (or (and offset (goto-char offset))
               (and linum (goto-char (point-min))
                    (forward-line (- linum 1))))
           (recenter 1)))))
    (:hide nil)
    (:update nil)))

(defun sos-tips-frontend (command &rest args)
  (case command
    (:show
     (when sos-tips
       ;; TODO: draw a overlay.
       ;; (message "%s" sos-tips)
       ))
    (:hide nil)
    (:update nil)))

;; (sos-toggle-buffer-window 1)
;; (sos-toggle-buffer-window -1)
(defun sos-toggle-buffer-window (toggle)
  "Display or hide the `sos-reference-buffer' and `sos-reference-window'."
  ;; TODO: modify mode line.
  (let ((enabled (or (and (booleanp toggle) toggle)
                     (and (numberp toggle)
                          (> toggle 0)))))
    (if enabled
        (progn
          (setq sos-reference-buffer (get-buffer-create "*Reference*"))
          (unless (window-live-p sos-reference-window)
            (let* ((win (cond
                         ;; Only one window.
                         ((window-live-p (frame-root-window))
                          (selected-window))
                         (t (selected-window))))
                   (height (or (and (> sos-reference-window-height 0)
                                    (- 0 sos-reference-window-height))
                               (and win
                                    (/ (window-height win) -3)))))
              (and win height
                   (setq sos-reference-window (split-window win height 'below)))))
          ;; Force to apply `sos-reference-buffer' to `sos-reference-window'.
          (set-window-buffer sos-reference-window sos-reference-buffer))
      (and (windowp sos-reference-window)
           (delete-window sos-reference-window))
      (and (bufferp sos-reference-buffer)
           (kill-buffer sos-reference-buffer))
      (setq sos-reference-buffer nil
            sos-reference-window nil))))

(defun sos-watchdog-post-command ()
  (condition-case err
      (progn
        (unless (or (member this-command '(self-insert-command
                                           previous-line
                                           next-line
                                           left-char
                                           right-char
                                           save-buffer))
                    (active-minibuffer-window))
          (if sos-reference-mode
              ;; Show them.
              (progn
                (sos-toggle-buffer-window 1)
                (setq sos-reference-window-height (window-height sos-reference-window)))
            ;; Hide them or not.
            (cond
             ((and (eq (selected-window) sos-reference-window)
                   (eq (current-buffer) sos-reference-buffer)) t)
             ;; If selected window is `sos-reference-window' but its buffer is not
             ;; `sos-reference-buffer'.
             ((and (eq (selected-window) sos-reference-window)
                   (not (eq (window-buffer) sos-reference-buffer)))
              (sos-toggle-buffer-window 1))
             (t (sos-toggle-buffer-window -1))))))
    (error "[sos] sos-watchdog-post-command error \"%s\""
           (error-message-string err))))

;;;###autoload
(define-minor-mode sos-watchdog-mode
  "A global minor mode which refers to buffer's `sos-reference-mode' to show the 
`sos-reference-buffer' and `sos-reference-window' or hide them. Show them if 
`sos-reference-mode' is t; Hide if nil."
  :global t
  (if sos-watchdog-mode
      (add-hook 'post-command-hook 'sos-watchdog-post-command t)
    (remove-hook 'post-command-hook 'sos-watchdog-post-command t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sos-pre-command ()
  ;; Call front-ends.
  (sos-call-frontends :hide)
  ;; Cancel idle timer.
  (when sos-timer
    (cancel-timer sos-timer)
    (setq sos-timer nil)))

(defun sos-post-command ()
  ;; Setup idle timer to call `sos-idle-begin'.
  (and (sos-idle-begin-check)
       (setq sos-timer (run-with-timer sos-idle-delay nil
                                       'sos-idle-begin
                                       (current-buffer) (selected-window)
                                       (buffer-chars-modified-tick) (point)))))

(defun sos-idle-begin-check ()
  (let ((pass t))
    (and (eq (current-buffer) sos-reference-buffer)
         (setq pass nil))
    pass))

(defun sos-idle-begin (buf win tick pos)
  (and (eq buf (current-buffer))
       (eq win (selected-window))
       (eq tick (buffer-chars-modified-tick))
       (eq pos (point))
       (sos-begin-process)))

(defun sos-begin-process ()
  (dolist (backend sos-backends)
    (let ((symb (sos-call-backend backend :symbol)))
      (cond
       ;; Return a string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((stringp symb)
        (setq sos-symbol symb)
        ;; Call back-end: get `sos-candidates' and `sos-candidate'.
        (let ((candidates (sos-call-backend backend :candidates symb)))
          (when (and candidates (listp candidates))
            (setq sos-candidates candidates
                  sos-candidate (and (sos-is-single-candidate)
                                     (car sos-candidates)))
            ;; Call back-end: get `sos-tips'.
            (setq sos-tips (sos-call-backend backend :tips sos-symbol sos-candidate))
            ;; Call front-ends.
            (sos-call-frontends :show)))
        (return t))

       ;; Return nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((null symb)
        (sos-kill-local-variables))

       ;; Return `:stop' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((eq symb :stop)
        (sos-kill-local-variables)
        (return t))))))

(defun sos-is-single-candidate ()
  (= (length sos-candidates) 1))

(defun sos-kill-local-variables ()
  (mapc 'kill-local-variable '(sos-symbol
                               sos-candidates
                               sos-candidate
                               sos-tips)))

(defun sos-call-frontends (command &rest args)
  "Iterate all the `sos-backends' and pass `command' by order."
  (let ((commands (cons command args)))
    (dolist (frontend sos-frontends)
      (dolist (cmd commands)
        (condition-case err
            (funcall frontend cmd)
          (error "[sos] Front-end %s error \"%s\" on command %s"
                 frontend (error-message-string err) commands))))))

(defmacro sos-call-backend (backend command &rest args)
  "Call certain backend `backend' and pass `command' to it."
  `(condition-case err
       (funcall ,backend ,command ,@args)
     (error "[sos] Back-end %s error \"%s\" on command %s"
            backend (error-message-string err) (cons command args))))

(defun sos-init-backend (backend)
  (condition-case err
      (progn
        (funcall backend :init)
        (put backend :init t))
    (put backend :init nil)
    (error "[sos] Back-end %s error \"%s\" on command %s"
           backend (error-message-string err) :init)))

;;;###autoload
(define-minor-mode sos-reference-mode
  "This local minor mode gethers symbol returned from backends around the point 
and show the reference visually through frontends. Usually frontends output the 
result to the `sos-reference-buffer' displayed in the `sos-reference-window'. 
Show or hide these buffer and window are controlled by `sos-watchdog-mode'."
  :lighter " SOS"
  (if sos-reference-mode
      (progn
        (unless (eq (current-buffer) sos-reference-buffer)
          (unless sos-watchdog-mode
            (sos-watchdog-mode 1))
          (mapc 'sos-init-backend sos-backends)
          (add-hook 'pre-command-hook 'sos-pre-command nil t)
          (add-hook 'post-command-hook 'sos-post-command nil t)))
    (sos-kill-local-variables)
    (remove-hook 'pre-command-hook 'sos-pre-command t)
    (remove-hook 'post-command-hook 'sos-post-command t)))

(provide 'sos)
