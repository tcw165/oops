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

(defvar sos-definition-buffer nil)

(defvar sos-definition-window nil)

(defvar sos-definition-window-height 0)

(defmacro sos-with-definition-buffer (&rest body)
  "Get definition buffer and window ready then interpret the `body'."
  (declare (indent 0) (debug t))
  `(progn
     (unless sos-definition-buffer
       (setq sos-definition-buffer (get-buffer-create "*Definition*")))
     (unless (window-live-p sos-definition-window)
       (let* ((win (cond
                    ;; Outline window is present ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; Default ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    (t (frame-root-window))))
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
         ;; Disable minor modes (read-write enabled, ...etc) and update buffer.
         (sos-navigation-mode -1)
         ;; Overlays
         (unless (and sos-hl-overlay
                      (buffer-live-p (overlay-buffer sos-hl-overlay)))
           (setq sos-hl-overlay (make-overlay 1 1)))
         (overlay-put sos-hl-overlay 'face sos-hl-face)
         ;; `body' >>>
         (ignore-errors
           (progn ,@body))
         ;; Enable minor modes (read-only, ...etc).
         (sos-navigation-mode 1)))))

(defun sos-toggle-definition-buffer&window (toggle)
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
            sos-hl-overlay nil))))

(defun sos-hl-word (hl-word)
  (move-overlay sos-hl-overlay 1 1)
  (and (stringp hl-word) (> (length hl-word) 0)
       (if (search-forward hl-word (line-end-position) t)
           (move-overlay sos-hl-overlay
                         (- (point) (length hl-word))
                         (point))
         (message "Can't find  %s at line %s in the file!"
                  (propertize (concat "\"" hl-word "\"")
                              'face 'font-lock-string-face)
                  (propertize (format "%s" linum)
                              'face 'font-lock-string-face)))))

;;;###autoload
(defun sos-definition-buffer-frontend (command)
  (case command
    (:init (sos-toggle-definition-buffer&window 1))
    (:destroy
     (and (window-live-p sos-definition-window)
          (setq sos-definition-window-height (window-height sos-definition-window)))
     (sos-toggle-definition-buffer&window -1))
    (:show
     (sos-toggle-definition-buffer&window 1)
     (setq sos-index (if (sos-is-single-candidate)
                         0
                       ;; TODO: remember user choice at last session in the prj.
                       0))
     (when sos-candidates
       (let* ((candidate (nth sos-index sos-candidates))
              (doc (plist-get candidate :doc))
              (file (plist-get candidate :file))
              (linum (plist-get candidate :linum))
              (hl-word (plist-get candidate :hl-word))
              (mode-line (plist-get candidate :mode-line))
              (header-mode-line (sos-header-mode-line))
              (button-mode-line (sos-button-mode-line mode-line file linum))
              (func (plist-get candidate :funcall)))
         (cond
          ;; A file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ((and (stringp file)
                (file-exists-p file))
           (sos-with-definition-buffer
             (insert-file-contents file nil nil nil t)
             ;; Find a appropriate major-mode for it.
             (dolist (mode auto-mode-alist)
               (and (not (null (cdr mode)))
                    (string-match (car mode) file)
                    (funcall (cdr mode))))
             (and (featurep 'hl-line)
                  (hl-line-unhighlight))
             ;; Move point and recenter.
             (and (integerp linum)
                  (goto-char (point-min))
                  (forward-line (- linum 1)))
             (recenter 3)
             ;; Highlight word.
             (sos-hl-word hl-word)
             ;; Set header line and button line.
             (setq header-line-format header-mode-line
                   mode-line-format button-mode-line)))

          ;; A document string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ((stringp doc)
           (sos-with-definition-buffer
             (erase-buffer)
             (fundamental-mode)
             (and (featurep 'hl-line)
                  (hl-line-unhighlight))
             (insert doc)
             ;; Move point
             (goto-char (point-min))
             ;; Highlight word.
             (sos-hl-word hl-word)
             ;; Set header line and button line.
             (setq header-line-format header-mode-line
                   mode-line-format button-mode-line)
             ;; Apply function provided by back-end.
             (and (functionp func)
                  (funcall func))))))))))

;;;###autoload
(defun sos-tips-frontend (command)
  (and sos-tips (memq command '(:show :update))
       (let* ((tips (nth sos-index sos-tips)))
         (and tips
              ;; (message tips)
              ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sos-navigation-mode-hook '()
  "Hook run when entering `prj-grep-mode' mode."
  :type 'hook
  :group 'sos-group)

;; TODO: keymap.
(defvar sos-navigation-map nil)
(defvar sos-goto-file-map nil)
(defvar sos-goto-file-linum-map nil)

(defun sos-header-mode-line ()
  (unless (sos-is-single-candidate)
    `(,(propertize " < "
                   'face 'button
                   'mouse-face 'button)
      ,(propertize " > "
                   'face 'button
                   'mouse-face 'button)
      "  There're multiple candidates (implementing).")))

(defun sos-button-mode-line (&optional desc file line)
  ;; (setq sos-file-name file
  ;;       sos-file-linum linum)
  `(,(propertize "  %b "
                 'face 'mode-line-buffer-id)
    (:eval (and ,desc
                (concat "| " ,desc)))
    (:eval (and ,file (file-exists-p ,file)
                (concat "| file:" (propertize (abbreviate-file-name ,file)
                                              'local-map sos-goto-file-map
                                              'face 'link
                                              'mouse-face 'link)
                        (and ,line
                             (concat ", line:" (propertize (format "%d" ,line)
                                                           'local-map sos-goto-file-linum-map
                                                           'face 'link
                                                           'mouse-face 'link)))
                        ", function:(yet supported)")))))

;;;###autoload
(define-minor-mode sos-navigation-mode
  "Minor mode for *Definition* buffers."
  :lighter " SOS:Navigation"
  :group 'sos-group
  (if sos-navigation-mode
      (progn
        (setq buffer-read-only t))
    (setq buffer-read-only nil)
    (mapc 'kill-local-variable '(header-line-format
                                 mode-line-format))))

(provide 'sos-basic-frontend)
