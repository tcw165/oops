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

(require 'thingatpt)

;;;###autoload
(defun sos-definition-buffer-frontend (command)
  (case command
    (:init (sos-toggle-definition-buffer&window 1))
    (:destroy
     (and (window-live-p sos-def-win)
          (setq sos-def-win-height (window-height sos-def-win)))
     (sos-toggle-definition-buffer&window -1))
    (:show
     (let* ((candidate (nth sos-index sos-candidates))
            (doc (plist-get candidate :doc))
            (file (plist-get candidate :file))
            (linum (plist-get candidate :linum))
            (hl-word (plist-get candidate :hl-word))
            (mode-line (plist-get candidate :mode-line))
            (header-mode-line (sos-header-mode-line))
            (button-mode-line (sos-button-mode-line mode-line file linum)))
       (sos-toggle-definition-buffer&window 1)
       ;; TODO: remember user choice at last session in the prj.
       (setq sos-def-stack nil
             sos-index 0)
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
                 mode-line-format button-mode-line))))))))

;;;###autoload
(defun sos-tips-frontend (command)
  (and sos-tips (memq command '(:show :update))
       (let* ((tips (nth sos-index sos-tips)))
         (and tips
              ;; (message tips)
              ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface sos-hl
  '((t (:background "yellow" :foreground "black" :weight bold :height 2.0)))
  "Default face for highlighting keyword in definition window."
  :group 'sos-group)

(defvar sos-hl-face 'sos-hl)

(defvar sos-hl-overlay nil
  "The overlay for `sos-definition-buffer'.")

(defvar sos-def-buf nil
  "Definition buffer.")

(defvar sos-def-win nil
  "Definition window.")

(defvar sos-def-win-height 0
  "The height of definition window.")

(defvar sos-def-stack nil
  "Cache the current content of definition buffer when navigating into deeper.")

;; TODO: keymap.
(defvar sos-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left] (lambda ()
                             (interactive)
                             (forward-symbol -1)))
    (define-key map [right] (lambda ()
                              (interactive)
                              (forward-symbol 1)))
    (define-key map [return] (lambda ()
                               (interactive)
                               (message "\"Jump to definition\" is yet supported!")))
    map))

(defvar sos-goto-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-ml-open-file)
    (define-key map [mode-line mouse-3] 'sos-ml-copy-path)
    map))
(defvar sos-goto-file-linum-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-ml-goto-line)
    map))

(defmacro sos-with-definition-buffer (&rest body)
  "Get definition buffer and window ready then interpret the `body'."
  (declare (indent 0) (debug t))
  `(progn
     (unless sos-def-buf
       (setq sos-def-buf (get-buffer-create "*Definition*")))
     (unless (window-live-p sos-def-win)
       (let* ((win (cond
                    ;; Outline window is present ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; Default ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    (t (frame-root-window))))
              (height (or (and (> sos-def-win-height 0)
                               (- 0 sos-def-win-height))
                          (and win
                               (/ (window-height win) -3)))))
         (and win height
              (setq sos-def-win (split-window win height 'below)))))
     ;; Bind definition buffer to definition window.
     (set-window-buffer sos-def-win sos-def-buf t)
     (with-selected-window sos-def-win
       (with-current-buffer sos-def-buf
         ;; Make it read-writeable.
         (setq buffer-read-only nil)
         ;; Overlays
         (unless (and sos-hl-overlay
                      (buffer-live-p (overlay-buffer sos-hl-overlay)))
           (setq sos-hl-overlay (make-overlay 1 1)))
         (overlay-put sos-hl-overlay 'face sos-hl-face)
         ;; `body' >>>
         (ignore-errors
           (progn ,@body))
         ;; Make it read-only.
         (setq buffer-read-only t)
         (sos-navigation-mode 1)))))

(defun sos-toggle-definition-buffer&window (toggle)
  "Display or hide the `sos-def-buf' and `sos-def-win'."
  (let ((enabled (or (and (booleanp toggle) toggle)
                     (and (numberp toggle)
                          (> toggle 0)))))
    (if enabled
        (sos-with-definition-buffer
          (setq header-line-format (sos-header-mode-line)
                mode-line-format (sos-button-mode-line)))
      (when (windowp sos-def-win)
        (delete-window sos-def-win))
      (when (bufferp sos-def-buf)
        (kill-buffer sos-def-buf))
      (setq sos-def-buf nil
            sos-def-win nil
            sos-hl-overlay nil))))

(defun sos-is-defintion-buffer&window ()
  (or (eq (current-buffer) sos-def-buf)
      (eq (selected-window) sos-def-win)))

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

(defun sos-ml-open-file ()
  "Open file refer to `mode-line-format'."
  (interactive)
  (sos-with-definition-buffer
    (let ()
      (message "%s" mode-line-format))))

(defun sos-ml-copy-path ()
  "Copy file path refer to `mode-line-format'."
  (interactive)
  (let ()
    ))

(defun sos-ml-goto-line ()
  "Go to line refer to `mode-line-format'."
  (interactive)
  (let ()
    ))

(defun sos-header-mode-line ()
  (when (sos-is-multiple-candidates)
    `(,(propertize " < "
                   'face 'button
                   'mouse-face 'button)
      ,(propertize " > "
                   'face 'button
                   'mouse-face 'button)
      "  There're multiple candidates (implementing).")))

(defun sos-button-mode-line (&optional desc file line)
  `(,(propertize "  *Definition* "
                 'face 'mode-line-buffer-id)
    (:eval (and ,desc
                (concat "| " ,desc)))
    (:eval (and ,file (file-exists-p ,file)
                (concat "| file:"
                        (propertize (abbreviate-file-name ,file)
                                    'local-map sos-goto-file-map
                                    'face 'link
                                    'mouse-face 'link
                                    'help-echo "mouse-1: Open the file.\n\
mouse-3: Copy the path.")
                        (and ,line
                             (concat ", line:"
                                     (propertize (format "%d" ,line)
                                                 'local-map sos-goto-file-linum-map
                                                 'face 'link
                                                 'mouse-face 'link
                                                 'help-echo "mouse-1: Back to the line.")))
                        ", function:(yet supported)")))))

;;;###autoload
(define-minor-mode sos-navigation-mode
  "Minor mode for *Definition* buffers."
  :lighter " SOS:Navigation"
  :group 'sos-group
  (if sos-navigation-mode
      (progn
        (use-local-map sos-navigation-map))
    ))

(provide 'sos-basic-frontend)
