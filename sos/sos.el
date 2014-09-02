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

;; Required modules.
(require 'sos-nav)

;; Front-ends.
(require 'sos-basic-frontend)

;; Back-ends.
(require 'sos-grep-backend)
(require 'sos-elisp-backend)
(require 'sos-semantic-backend)

(defgroup sos-group nil
  "A utility to show you documentation at button window by finding some 
meaningful information around the point."
  :tag "Sos")

(defface sos-hl
  '((t (:background "yellow" :foreground "black" :weight bold :height 1.5)))
  "Default face for highlighting the current line in Hl-Line mode."
  :group 'sos-group)

(defcustom sos-frontends '(sos-definition-buffer-frontend
                           sos-tips-frontend)
  "The list of front-ends for the purpose of visualization.

`:show': When the visualization should start.

`:hide': When the visualization should end.

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
 ((:file STRING
   :offset INTEGER
   :linum INTEGER
   :hl-word STRING
   :hl-line BOOLEAN) (...) ...)

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

(defcustom sos-idle-delay 0.25
  "The idle delay in seconds until sos starts automatically."
  :type '(number :tag "Seconds"))

(defvar sos-timer nil)

(defvar sos-definition-buffer nil)

(defvar sos-definition-window nil)

(defvar sos-definition-window-height 0)

(defvar sos-hl-face 'sos-hl)

(defvar sos-hl-overlay nil
  "The overlay for `sos-definition-buffer'.")

(defvar sos-file-name nil
  "Cache file name for `sos-navigation-mode'.")

(defvar sos-backend nil
  "The back-end which takes control of current session in the back-ends list.")
(make-variable-buffer-local 'sos-backend)

(defvar sos-symbol nil
  "Cache the return value from back-end with `:symbol' command.")
(make-variable-buffer-local 'sos-symbol)

(defvar sos-candidates nil
  "Cache the return value from back-end with `:candidates' command.")
(make-variable-buffer-local 'sos-candidates)

(defvar sos-tips nil
  "Cache the return value from back-end with `:tips' command.")
(make-variable-buffer-local 'sos-tips)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro sos-with-definition-buffer (&rest body)
  "Get definition buffer and window ready then interpret the `body'."
  (declare (indent 0) (debug t))
  `(progn
     (unless sos-definition-buffer
       (setq sos-definition-buffer (get-buffer-create "*Definition*")))
     (unless (window-live-p sos-definition-window)
       (let* ((win (cond
                    ;; Only one window ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ((window-live-p (frame-root-window))
                     (selected-window))
                    ;; Default ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    (t (selected-window))))
              (height (or (and (> sos-definition-window-height 0)
                               (- 0 sos-definition-window-height))
                          (and win
                               (/ (window-height win) -3)))))
         (and win height
              (setq sos-definition-window (split-window win height 'below)))))
     ;; Bind definition buffer to definition window.
     (set-window-buffer sos-definition-window sos-definition-buffer t)
     (with-selected-window sos-definition-window
       (with-current-buffer sos-definition-buffer
         ;; Disable minor modes (read-write enabled, ...etc).
         (sos-navigation-mode -1)
         ;; Overlays
         (unless (and sos-hl-overlay
                      (buffer-live-p (overlay-buffer sos-hl-overlay)))
           (setq sos-hl-overlay (make-overlay 1 1)))
         (overlay-put sos-hl-overlay 'face sos-hl-face)
         ;; `body' >>>
         (progn ,@body)
         ;; <<<<<<<<<<
         ;; Enable minor modes (read-only, ...etc).
         (sos-navigation-mode 1)))))

(defun sos-is-skip-command (&rest commands)
  (member this-command `(mwheel-scroll
                         save-buffer
                         eval-buffer
                         eval-last-sexp
                         ;; Additional commands.
                         ,@commands)))

(defun sos-is-single-candidate ()
  (= (length sos-candidates) 1))

(defun sos-pre-command ()
  (when sos-timer
    (cancel-timer sos-timer)
    (setq sos-timer nil)))

(defun sos-post-command ()
  (and (sos-is-idle-begin)
       ;;;;;; Begin instantly.
       (or nil
           (and (= sos-idle-delay 0)
                (sos-idle-begin (current-buffer) (point)))
           ;; Begin with delay `sos-idle-delay'
           (setq sos-timer (run-with-timer sos-idle-delay nil
                                           'sos-idle-begin
                                           (current-buffer) (point))))))

(defun sos-is-idle-begin ()
  (not (or (eq (current-buffer) sos-definition-buffer)
           (eq (selected-window) sos-definition-window)
           (sos-is-skip-command))))

(defun sos-idle-begin (buf pt)
  (and (eq buf (current-buffer))
       (eq pt (point))
       (if (null sos-backend)
           (sos-1st-process-backends)
         (sos-process-backend sos-backend))))

(defun sos-1st-process-backends ()
  (dolist (backend sos-backends)
    (sos-process-backend backend)
    (and sos-backend
         (return t)))
  t)

(defun sos-process-backend (backend)
  (let ((symb (sos-call-backend backend :symbol)))
    (cond
     ;; Return a string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((stringp symb)
      (if (string-equal symb sos-symbol)
          (progn
            ;; If return symbol string is equal to `sos-symbol', ask front-ends
            ;; Renew the `:tips' and to do `:update' task.
            (setq sos-tips (sos-call-backend backend :tips symb))
            (sos-call-frontends :update))
        ;; Call front-ends: `:hide'.
        (sos-call-frontends :hide)
        (setq sos-backend backend
              sos-symbol symb)
        ;; Call back-end: get `sos-candidates' and `sos-candidate'.
        (setq sos-candidates (sos-call-backend backend :candidates symb)
              sos-tips (sos-call-backend backend :tips symb))
        ;; (sos-call-backend backend :tips symb)
        (and sos-candidates (listp sos-candidates)
             ;; Call front-ends: `:show'.
             (sos-call-frontends :show :update))))

     ;; Return nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((null symb)
      (sos-kill-local-variables))

     ;; Return `:stop' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((eq symb :stop)
      (setq sos-backend backend))))
  t)

(defun sos-kill-local-variables ()
  (mapc 'kill-local-variable '(sos-backend
                               sos-symbol
                               sos-candidates
                               sos-candidate
                               sos-tips)))

(defun sos-call-frontends (command &rest args)
  "Iterate all the `sos-backends' and pass `command' by order."
  (let ((commands (cons command args)))
    (dolist (frontend sos-frontends)
      (dolist (cmd commands)
        ;; (condition-case err
        ;;     (funcall frontend cmd)
        ;;   (error "[sos] Front-end %s error \"%s\" on command %s"
        ;;          frontend (error-message-string err) commands))
        (funcall frontend cmd)))))

(defun sos-call-backend (backend command &optional arg)
  "Call certain backend `backend' and pass `command' to it."
  ;; (condition-case err
  ;;     (funcall backend command arg)
  ;;   (error "[sos] Back-end %s error \"%s\" on command %s"
  ;;          backend (error-message-string err) (cons command arg)))
  (funcall backend command arg))

(defun sos-init-backend (backend)
  ;; (condition-case err
  ;;     (progn
  ;;       (funcall backend :init))
  ;;   (error "[sos] Back-end %s error \"%s\" on command %s"
  ;;          backend (error-message-string err) :init))
  (funcall backend :init))

;;;###autoload
(define-minor-mode sos-definition-window-mode
  "This local minor mode gethers symbol returned from backends around the point 
and show the reference visually through frontends. Usually frontends output the 
result to the `sos-definition-buffer' displayed in the `sos-definition-window'. 
Show or hide these buffer and window are controlled by `sos-watchdog-mode'."
  :lighter " SOS:Ref"
  :group 'sos-group
  ;; TODO: menu-bar and tool-bar keymap.
  (if sos-definition-window-mode
      (progn
        (unless (eq (current-buffer) sos-definition-buffer)
          (unless sos-watchdog-mode
            (sos-watchdog-mode 1))
          (mapc 'sos-init-backend sos-backends)
          (add-hook 'pre-command-hook 'sos-pre-command nil t)
          (add-hook 'post-command-hook 'sos-post-command nil t)))
    (sos-kill-local-variables)
    (remove-hook 'pre-command-hook 'sos-pre-command t)
    (remove-hook 'post-command-hook 'sos-post-command t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sos-toggle-buffer-window (toggle)
  "Display or hide the `sos-definition-buffer' and `sos-definition-window'."
  (let ((enabled (or (and (booleanp toggle) toggle)
                     (and (numberp toggle)
                          (> toggle 0)))))
    (if enabled
        (sos-with-definition-buffer)
      (when (windowp sos-definition-window)
        (delete-window sos-definition-window))
      (when (bufferp sos-definition-buffer)
        (kill-buffer sos-definition-buffer))
      (setq sos-definition-buffer nil
            sos-definition-window nil
            sos-file-name nil
            sos-hl-overlay nil))))

(defun sos-watchdog-post-command ()
  (condition-case err
      (progn
        (unless (or (sos-is-skip-command 'self-insert-command
                                         'previous-line
                                         'next-line
                                         'left-char
                                         'right-char)
                    (active-minibuffer-window))
          (if sos-definition-window-mode
              ;; Show them.
              (progn
                (sos-toggle-buffer-window 1)
                (setq sos-definition-window-height (window-height sos-definition-window)))
            ;; Hide them or not.
            (cond
             ;; If selected window is `sos-definition-window' and current buffer is
             ;; `sos-definition-buffer':
             ((and (eq (selected-window) sos-definition-window)
                   (eq (current-buffer) sos-definition-buffer)) t)
             ;; If selected window is `sos-definition-window' but its buffer is not
             ;; `sos-definition-buffer':
             ((and (eq (selected-window) sos-definition-window)
                   (not (eq (window-buffer) sos-definition-buffer)))
              (sos-toggle-buffer-window 1))
             ;; Hide by default:
             (t (sos-toggle-buffer-window -1))))))
    (error "[sos] sos-watchdog-post-command error \"%s\""
           (error-message-string err))))

;;;###autoload
(define-minor-mode sos-watchdog-mode
  "A global minor mode which refers to buffer's `sos-definition-window-mode' to show the 
`sos-definition-buffer' and `sos-definition-window' or hide them. Show them if 
`sos-definition-window-mode' is t; Hide if nil."
  :global t
  :group 'sos-group
  (if sos-watchdog-mode
      (add-hook 'post-command-hook 'sos-watchdog-post-command t)
    (remove-hook 'post-command-hook 'sos-watchdog-post-command t)))

(provide 'sos)
