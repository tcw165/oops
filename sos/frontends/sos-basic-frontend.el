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

(defvar sos-file-name nil
  "Cache file name for `sos-navigation-mode'.")

(defvar sos-file-linum nil
  "Cache line number for `sos-navigation-mode'.")

(defvar sos-file-keyword nil
  "Cache keyword string for `sos-navigation-mode'.")

(defun sos-definition-buffer-frontend (command &rest args)
  (case command
    (:show
     ;; TODO: multiple candidates `sos-is-single-candidate'.
     (if (sos-is-single-candidate)
         (let* ((candidate (car sos-candidates))
                (file (plist-get candidate :file))
                (linum (plist-get candidate :linum))
                (hl-word (plist-get candidate :hl-word)))
           (when (file-exists-p file)
             (sos-with-definition-buffer
               ;; Disable minor modes (read-write enabled, ...etc) and update buffer.
               (sos-navigation-mode -1)
               (insert-file-contents file nil nil nil t)
               ;; Set them for `sos-nav-mode'.
               (setq sos-file-name file
                     sos-file-linum linum
                     sos-file-keyword hl-word)
               ;; Find a appropriate major-mode for it.
               (dolist (mode auto-mode-alist)
                 (and (not (null (cdr mode)))
                      (string-match (car mode) file)
                      (funcall (cdr mode))))
               ;; Enable minor modes (read-only, ...etc).
               (and (featurep 'hl-line)
                    (hl-line-unhighlight))
               (sos-navigation-mode 1)
               ;; Move point and recenter.
               (and (integerp linum)
                    (goto-char (point-min))
                    (forward-line (- linum 1)))
               (recenter 3)
               ;; Highlight word or line.
               (move-overlay sos-hl-overlay 1 1)
               (or (and (stringp hl-word) (> (length hl-word) 0)
                        (search-forward hl-word (line-end-position) t)
                        (move-overlay sos-hl-overlay (- (point) (length hl-word)) (point)))
                   (and hl-line
                        (move-overlay sos-hl-overlay (line-beginning-position) (+ 1 (line-end-position))))))))))
    (:hide nil)
    (:update nil)))

(defun sos-tips-frontend (command &rest args)
  (case command
    (:show
     (when (stringp sos-tips)
       ;; TODO: draw a overlay.
       ;; (message "%s" sos-tips)
       ))
    (:hide nil)
    (:update nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sos-navigation-mode-hook '()
  "Hook run when entering `prj-grep-mode' mode."
  :type 'hook
  :group 'sos-group)

;; TODO: keymap.
(defvar sos-navigation-map nil)
(defvar sos-nav-file-map nil)
(defvar sos-nav-file-linum-map nil)

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

;; Sample: ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position  (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
;; (sos-navigation-mode-line)
(defun sos-navigation-mode-line ()
  `(,(propertize " %b "
                 'face 'mode-line-buffer-id)
    (:eval (and sos-file-name
                (concat "| file:" (propertize (abbreviate-file-name sos-file-name)
                                              'local-map sos-nav-file-map
                                              'face 'link
                                              'mouse-face 'mode-line-highlight)
                        (and sos-file-linum
                             (concat ", line:" (propertize (format "%d" sos-file-linum)
                                                           'local-map sos-nav-file-linum-map
                                                           'face 'link
                                                           'mouse-face 'mode-line-highlight)))
                        ", function:(yet supported)")))))

;;;###autoload
(define-minor-mode sos-navigation-mode
  "Minor mode for *Definition* buffers."
  :lighter " SOS:Navigation"
  :group 'sos-group
  (if sos-navigation-mode
      (progn
        (setq mode-line-format (sos-navigation-mode-line)
              buffer-read-only t))
    (setq buffer-read-only nil
          sos-file-name nil
          sos-file-linum nil
          sos-file-keyword nil)
    (mapc 'kill-local-variable '(mode-line-format))))

(provide 'sos-basic-frontend)
