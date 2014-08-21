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

(defcustom sos-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                           company-preview-if-just-one-frontend
                           company-echo-metadata-frontend)
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
  :type '(repeat (choice (const :tag "echo" company-echo-frontend)
                         (const :tag "echo, strip common"
                                company-echo-strip-common-frontend)
                         (const :tag "show echo meta-data in echo"
                                company-echo-metadata-frontend)
                         (const :tag "pseudo tooltip"
                                company-pseudo-tooltip-frontend)
                         (const :tag "pseudo tooltip, multiple only"
                                company-pseudo-tooltip-unless-just-one-frontend)
                         (const :tag "preview" company-preview-frontend)
                         (const :tag "preview, unique only"
                                company-preview-if-just-one-frontend)
                         (function :tag "custom function" nil))))

(defcustom sos-backends '(sos-elisp-backend
                          sos-semantic-backend)
  "The list of active back-ends in sos engines. The sos engine will iterate
the back-ends and pass specific commands in order. Every command has its
purpose, paremeter rule and return rule. By passing command and get return
data from a back-end, the sos engine get information to show the result 
to another window, a message in minibuffer or popup a GUI dialog, etc. The
way of showing the information from the back-ends is handled by the front-
ends.

### The sample of a back-end:

  (defun some-backend (cmd &optional arg &rest ign)
    (if (equal major-mode 'xxx-mode)
        (thing-at-point 'symbol)
      nil))

Each back-end is a function that takes a variable number of arguments. The
first argument is the command requested from the sos enine.  It is one of
the following:

### The order of the commands, begins from top to down:

`thing':

`candidates':

Only one back-end is used at a time.  The choice depends on the order of
the items in this list, and on the values they return in response to the
`prefix' command (see below).  But a back-end can also be a \"grouped\"
one (see below).

`company-begin-backend' can be used to start a specific back-end,
`company-other-backend' will skip to the next matching back-end in the list.

Each back-end is a function that takes a variable number of arguments.
The first argument is the command requested from the back-end.  It is one
of the following:

`prefix': The back-end should return the text to be completed.  It must be
text immediately before point.  Returning nil passes control to the next
back-end.  The function should return `stop' if it should complete but
cannot (e.g. if it is in the middle of a string).  Instead of a string,
the back-end may return a cons where car is the prefix and cdr is used in
`company-minimum-prefix-length' test.  It must be either number or t, and
in the latter case the test automatically succeeds.

`candidates': The second argument is the prefix to be completed.  The
return value should be a list of candidates that match the prefix.

Non-prefix matches are also supported (candidates that don't start with the
prefix, but match it in some backend-defined way).  Backends that use this
feature must disable cache (return t to `no-cache') and should also respond
to `match'.

Optional commands:

`sorted': Return t here to indicate that the candidates are sorted and will
not need to be sorted again.

`duplicates': If non-nil, company will take care of removing duplicates
from the list.

`no-cache': Usually company doesn't ask for candidates again as completion
progresses, unless the back-end returns t for this command.  The second
argument is the latest prefix.

`meta': The second argument is a completion candidate.  Return a (short)
documentation string for it.

`doc-buffer': The second argument is a completion candidate.  Return a
buffer with documentation for it.  Preferably use `company-doc-buffer',

`location': The second argument is a completion candidate.  Return the cons
of buffer and buffer location, or of file and line number where the
completion candidate was defined.

`annotation': The second argument is a completion candidate.  Return a
string to be displayed inline with the candidate in the popup.  If
duplicates are removed by company, candidates with equal string values will
be kept if they have different annotations.  For that to work properly,
backends should store the related information on candidates using text
properties.

`match': The second argument is a completion candidate.  Backends that
provide non-prefix completions should return the position of the end of
text in the candidate that matches `prefix'.  It will be used when
rendering the popup.

`require-match': If this returns t, the user is not allowed to enter
anything not offered as a candidate.  Use with care!  The default value nil
gives the user that choice with `company-require-match'.  Return value
`never' overrides that option the other way around.

`init': Called once for each buffer. The back-end can check for external
programs and files and load any required libraries.  Raising an error here
will show up in message log once, and the back-end will not be used for
completion.

`post-completion': Called after a completion candidate has been inserted
into the buffer.  The second argument is the candidate.  Can be used to
modify it, e.g. to expand a snippet.

The back-end should return nil for all commands it does not support or
does not know about.  It should also be callable interactively and use
`company-begin-backend' to start itself in that case.

Grouped back-ends:

An element of `company-backends' can also itself be a list of back-ends,
then it's considered to be a \"grouped\" back-end.

When possible, commands taking a candidate as an argument are dispatched to
the back-end it came from.  In other cases, the first non-nil value among
all the back-ends is returned.

The latter is the case for the `prefix' command.  But if the group contains
the keyword `:with', the back-ends after it are ignored for this command.

The completions from back-ends in a group are merged (but only from those
that return the same `prefix').

Asynchronous back-ends:

The return value of each command can also be a cons (:async . FETCHER)
where FETCHER is a function of one argument, CALLBACK.  When the data
arrives, FETCHER must call CALLBACK and pass it the appropriate return
value, as described above.

True asynchronous operation is only supported for command `candidates', and
only during idle completion.  Other commands will block the user interface,
even if the back-end uses the asynchronous calling convention."
  :type `(repeat
          (choice
           :tag "Back-end"
           ,@(mapcar (lambda (b) `(const :tag ,(cdr b) ,(car b)))
                     company-safe-backends)
           (symbol :tag "User defined"))))

(defun sos-call-frontends (command)
  (dolist (frontend company-frontends)
    (condition-case err
        (funcall frontend command)
      (error (error "Company: Front-end %s error \"%s\" on command %s"
                    frontend (error-message-string err) command)))))

(defun sos-call-backends (command)
  (dolist (frontend company-frontends)
    (condition-case err
        (funcall frontend command)
      (error (error "Company: Back-end %s error \"%s\" on command %s"
                    frontend (error-message-string err) command)))))

(provide 'sos)
