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

(defgroup sos-group nil
  "A utility to show you documentation by finding some meaningful information around the point."
  :tag "Sos")

(defcustom sos-frontends '()
  "The list of active front-ends (visualizations).
Each front-end is a function that takes one argument.  It is called with
one of the following arguments:

`show': When the visualization should start.

`hide': When the visualization should end.

`update': When the data has been updated.

`pre-command': Before every command that is executed while the
visualization is active.

`post-command': After every command that is executed while the
visualization is active.

The visualized data is stored in `company-prefix', `company-candidates',
`company-common', `company-selection', `company-point' and
`company-search-string'."
  :set 'company-frontends-set
  :type '(repeat (symbol :tag "Front-end"))
  :group 'sos-group)

(defcustom sos-backends '(sos-elisp-backend
                          sos-semantic-backend)
  "The list of active back-ends in sos engines. The sos engine will iterate
the back-ends and pass specific commands in order. Every command has its
purpose, paremeter rule and return rule. By passing command and get return
data from a back-end, the sos engine gets information to show the result 
to another window, minibuffer or popup a GUI dialog, etc. The way of showing
the information from the back-ends is handled by the front-ends.

### The sample of a back-end:

  (defun some-backend (cmd &optional arg &rest ign)
    (if (equal major-mode 'xxx-mode)
        (thing-at-point 'symbol)
      nil))

Each back-end is a function that takes a variable number of arguments. The
first argument is the command requested from the sos enine.  It is one of
the following:

### The order of the commands to be called by sos engine, begins from top to down:

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

Optional commands:

`meta': The second argument is a completion candidate. Return a documentation
string for it.

`init': Called once for each buffer. The back-end can check for external programs
and files and load any required libraries. Raising an error here will show up in
message log once, and the back-end will not be used for completion."
  :type '(repeat (symbol :tag "Back-end"))
  :group 'sos-group)

(defun sos-call-frontends (command)
  (dolist (frontend sos-frontends)
    (condition-case err
        (funcall frontend command)
      (error (error "Company: Front-end %s error \"%s\" on command %s"
                    frontend (error-message-string err) command)))))

(defun sos-call-backends (command)
  (dolist (backtend sos-frontends)
    (condition-case err
        (funcall backtend command)
      (error (error "Company: Back-end %s error \"%s\" on command %s"
                    frontend (error-message-string err) command)))))

;;;###autoload
(define-minor-mode sos-mode
  :lighter "sos"
  :keymap company-mode-map
  (if sos-mode
      (progn
        (when (eq company-idle-delay t)
          (setq company-idle-delay 0)
          (warn "Setting `company-idle-delay' to t is deprecated.  Set it to 0 instead."))
        (add-hook 'pre-command-hook 'company-pre-command nil t)
        (add-hook 'post-command-hook 'company-post-command nil t)
        (mapc 'company-init-backend company-backends))
    (remove-hook 'pre-command-hook 'company-pre-command t)
    (remove-hook 'post-command-hook 'company-post-command t)
    (company-cancel)
    (kill-local-variable 'company-point)))

(provide 'sos)
