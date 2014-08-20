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

(defcustom sos-backends '(company-pseudo-tooltip-unless-just-one-frontend
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
      (error (error "Company: Front-end %s error \"%s\" on command %s"
                    frontend (error-message-string err) command)))))

(provide 'sos)
