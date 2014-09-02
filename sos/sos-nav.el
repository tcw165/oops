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
;; 2014-08-01 (0.0.1)
;;    Initial release.

(defcustom sos-navigation-mode-hook '()
  "Hook run when entering `prj-grep-mode' mode."
  :type 'hook
  :group 'sos-group)

(defvar sos-navigation-map nil)

;; TODO: keymap.
(defvar sos-navigation-mode-line-highlight-map nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sample:
;; (defvar mode-line-position
;;   `((-3 ,(propertize
;; 	  "%p"
;; 	  'local-map mode-line-column-line-number-mode-map
;; 	  'mouse-face 'mode-line-highlight
;; 	  ;; XXX needs better description
;; 	  'help-echo "Size indication mode"))
;;     (size-indication-mode
;;      (8 ,(propertize
;; 	  " of %I"
;; 	  'local-map mode-line-column-line-number-mode-map
;; 	  'mouse-face 'mode-line-highlight
;; 	  ;; XXX needs better description
;; 	  'help-echo "Size indication mode")))
;;     (line-number-mode
;;      ((column-number-mode
;;        (10 ,(propertize
;; 	     " (%l,%c)"
;; 	     'local-map mode-line-column-line-number-mode-map
;; 	     'mouse-face 'mode-line-highlight
;; 	     'help-echo "Line number and Column number"))
;;        (6 ,(propertize
;; 	    " L%l"
;; 	    'local-map mode-line-column-line-number-mode-map
;; 	    'mouse-face 'mode-line-highlight
;; 	    'help-echo "Line Number"))))
;;      ((column-number-mode
;;        (5 ,(propertize
;; 	    " C%c"
;; 	    'local-map mode-line-column-line-number-mode-map
;; 	    'mouse-face 'mode-line-highlight
;; 	    'help-echo "Column number")))))))

(defun sos-nav-kill-local-variables ()
  (mapc 'kill-local-variable '(mode-line-format)))

;; Sample: ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position  (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
;; (sos-navigation-mode-line)
(defun sos-navigation-mode-line ()
  `((8 ,(propertize " L:%l "))
    ,(propertize "%13b "
                 'face 'mode-line-buffer-id)
    (13 (:eval (if buffer-read-only
                   "(read-only) "
                 (if (buffer-modified-p)
                     "(modified) "
                   "(read-write) "))))
    (:eval (and sos-file-name
                (concat "file: " (propertize (abbreviate-file-name sos-file-name)
                                             'local-map sos-navigation-mode-line-highlight-map
                                             'face 'link
                                             'mouse-face 'mode-line-highlight))))))

;;;###autoload
(define-minor-mode sos-navigation-mode
  "Minor mode for *Definition* buffers."
  :lighter " SOS:Navigation"
  :group 'sos-group
  (if sos-navigation-mode
      (progn
        (setq mode-line-format (sos-navigation-mode-line)
              buffer-read-only t))
    (setq buffer-read-only nil)
    (sos-nav-kill-local-variables)))

(provide 'sos-nav)
