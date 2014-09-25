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

(require 'align)
(require 'cus-edit)
(require 'font-lock)
(require 'hl-line)
(require 'linum)
(require 'thingatpt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Front-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun sos-definition-buffer-frontend (command &optional arg)
  (case command
    (:init (sos-toggle-definition-buffer&window 1))
    (:destroy (sos-toggle-definition-buffer&window -1))
    (:show
     ;; TODO: remember user choice at last session in the prj.
     (setq sos-candidates arg
           sos-index 0
           ;; Clean the stack.
           sos-candidates-stack nil)
     (sos-toggle-definition-buffer&window 1)
     (if (sos-is-multiple-candidates)
         (sos-show-candidates)
       (sos-show-candidate))))
  ;; Save the height of definition window.
  (and (window-live-p sos-def-win)
       (setq sos-def-win-height (window-height sos-def-win))))

;;;###autoload
(defun sos-goto-definition-frontend (command &optional arg)
  (case command
    (:show
     (and (featurep 'history)
          (his-add-history))
     (setq sos-candidates arg)
     (if (sos-is-multiple-candidates)
         ;; TODO:
         (progn
           (message "yet support multiple definitions!"))
       (let* ((candidate (nth 0 sos-candidates))
              (file (plist-get candidate :file))
              (linum (plist-get candidate :linum)))
         (when (and (stringp file)
                    (integerp linum))
           (find-file-existing file)
           (goto-line linum))))
     (and (featurep 'history)
          (his-add-history)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun sos-open-definition-file ()
  "Open file refer to `mode-line-format'."
  (interactive)
  (let* ((info (sos-ml-info))
         (file (car info))
         (linum (cdr info)))
    (when (and (file-exists-p file)
               (integerp linum)
               (window-live-p sos-source-window))
      (with-selected-window sos-source-window
        (unless (string= file (buffer-file-name (window-buffer)))
          (and (featurep 'history)
               (his-add-position-type-history))
          (find-file file)
          (goto-line linum)
          (and (featurep 'history)
               (his-add-position-type-history))
          (recenter 3)))
      (select-window sos-source-window))))

;;;###autoload
(defun sos-copy-definition-file-path ()
  "Copy file path refer to `mode-line-format'."
  (interactive)
  (let* ((info (sos-ml-info))
         (file (car info)))
    (and file
         (with-temp-buffer
           (insert file)
           (clipboard-kill-ring-save 1 (point-max)))
         (message "File path is copied to clipboard!"))))

;;;###autoload
(defun sos-goto-definition-line ()
  "Go to line refer to `mode-line-format'."
  (interactive)
  (let* ((info (sos-ml-info))
         (linum (cdr info)))
    (and (integerp linum)
         (sos-with-definition-buffer
           (goto-line linum)
           (recenter 3)))))

;;;###autoload
(defun sos-jump-in-candidate ()
  (interactive)
  (sos-push-candidates-stack)
  (let ((index (1- (sos-with-definition-buffer
                     (line-number-at-pos)))))
    (setq sos-candidates `(,(nth index sos-candidates)))
    (sos-show-candidate)))

;;;###autoload
(defun sos-jump-out-candidate ()
  (interactive)
  (let ((candidates (sos-pop-candidates-stack)))
    (when candidates
      (setq sos-candidates candidates)
      (if (sos-is-multiple-candidates)
          (sos-show-candidates)
        (sos-show-candidate)))))

(defface sos-hl-face
  '((t (:background "yellow" :foreground "black" :weight bold :height 2.0)))
  "Default face for highlighting keyword in definition window."
  :group 'sos-group)

(defface sos-hl-line-face
  '((t (:background "yellow" :weight bold)))
  "Default face for highlighting line in definition window."
  :group 'sos-group)

(defvar sos-hl-line-overlay nil
  "The highlight for line in `sos-definition-buffer'.")
(make-variable-buffer-local 'sos-hl-line-overlay)

(defvar sos-def-buf nil
  "Definition buffer.")

(defvar sos-def-win nil
  "Definition window.")

(defvar sos-def-win-height 0
  "The height of definition window.")

(defvar sos-index 0
  "The index of current candidate in the list.")

(defvar sos-candidates nil
  "Cache the return value from back-end with `:candidates' command.")

(defvar sos-candidates-stack nil
  "A list containing `sos-candidates'. Engine will push the current candidates 
into the stack when user navigate to deeper definition in the definition window.")

(defvar sos-file-path-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-open-definition-file)
    (define-key map [mode-line mouse-3] 'sos-copy-definition-file-path)
    map))

(defvar sos-linum-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-goto-definition-line)
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
                    ;; TODO:
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
     (let (ret)
       (with-selected-window sos-def-win
         (with-current-buffer sos-def-buf
           ;; Make it read-writeable.
           (setq buffer-read-only nil)
           ;; `body' >>>
           (ignore-errors
             (setq ret (progn ,@body)))
           ;; Make it read-only.
           (setq buffer-read-only t)
           ret)))))

(defun sos-toggle-definition-buffer&window (toggle)
  "Display or hide the `sos-def-buf' and `sos-def-win'."
  (let ((enabled (or (and (booleanp toggle) toggle)
                     (and (numberp toggle)
                          (> toggle 0)))))
    (if enabled
        (sos-with-definition-buffer
          (setq header-line-format nil
                mode-line-format (sos-bottom-mode-line)))
      (when (windowp sos-def-win)
        (delete-window sos-def-win))
      (when (bufferp sos-def-buf)
        (kill-buffer sos-def-buf))
      (setq sos-def-buf nil
            sos-def-win nil))))

(defun sos-is-multiple-candidates ()
  (> (length sos-candidates) 1))

(defun sos-header-mode-line ()
  `("  Multiple Definitions | "
    (:eval (when sos-candidates-mode
             (format "Choose definition: %s %s "
                     (propertize " Up "
                                 'face 'custom-button
                                 'mouse-face 'custom-button-mouse
                                 'local-map sos-prev-candidate-button-map)
                     (propertize " Down "
                                 'face 'custom-button
                                 'mouse-face 'custom-button-mouse
                                 'local-map sos-next-candidate-button-map))))
    (:eval (if sos-candidates-stack
               (format "Back to options: %s "
                       (propertize " Back "
                                   'face 'custom-button
                                   'mouse-face 'custom-button-mouse
                                   'local-map sos-jump-out-button-map))
             (format "Open definition: %s "
                     (propertize " Go "
                                 'face 'custom-button
                                 'mouse-face 'custom-button-mouse
                                 'local-map sos-jump-in-button-map))))))

(defun sos-bottom-mode-line (&optional desc file line)
  `(,(propertize "  Definition "
                 'face 'mode-line-buffer-id)
    (:eval (and (stringp ,file) (file-exists-p ,file) (integerp ,line)
                (format "| file:%s, line:%s, function:(yet supported) "
                        (propertize (abbreviate-file-name ,file)
                                    'face 'link
                                    'mouse-face 'highlight
                                    'help-echo "mouse-1: Open the file.\n\
mouse-3: Copy the path."
                                    'local-map sos-file-path-map)
                        (propertize (number-to-string ,line)
                                    'face 'link
                                    'mouse-face 'highlight
                                    'help-echo "mouse-1: Back to the line."
                                    'local-map sos-linum-map))))
    (:eval (and ,desc (> (length ,desc) 0)
                (format "| %s " ,desc)))))

(defun sos-ml-info ()
  "Get file and line number from definition buffer's `mode-line-format'.
Return (FILE . LINUM) struct."
  (sos-with-definition-buffer
    (let* ((text (pp-to-string mode-line-format))
           (beg (string-match "(file-exists-p \".+\")" text))
           (end (match-end 0))
           (file (and beg end
                      (substring text
                                 (+ beg 16)
                                 (- end 2))))
           (beg (string-match "(integerp [0-9]+)" text))
           (end (match-end 0))
           (linum (and beg end
                       (string-to-int
                        (substring text
                                   (+ beg 10)
                                   (- end 1))))))
      (cons file linum))))

(defun sos-show-candidate ()
  "Show single candidate prompt."
  (let* ((candidate (nth sos-index sos-candidates))
         (doc (plist-get candidate :doc))
         (file (plist-get candidate :file))
         (linum (plist-get candidate :linum))
         (keywords (plist-get candidate :keywords))
         (mode-line (plist-get candidate :mode-line))
         (nav-tooltips (and sos-candidates-stack
                            (format "%s to back to options. "
                                    (propertize " q "
                                                'face 'tooltip))))
         (tooltips (concat mode-line nav-tooltips))
         (button-mode-line (sos-bottom-mode-line tooltips file linum)))
    (sos-with-definition-buffer
      (kill-all-local-variables)
      (remove-overlays)
      (erase-buffer)
      (insert doc)
      ;; Major mode.
      (fundamental-mode)
      (cond
       ;; A File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ((and (stringp file)
             (file-exists-p file))
        ;; Change major-mode refer to file name.
        (dolist (mode auto-mode-alist)
          (and (not (null (cdr mode)))
               (string-match (car mode) file)
               (funcall (cdr mode)))))
       ;; A Document ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (t
        ))
      ;; Minor mode.
      (sos-candidate-mode 1)
      (hl-line-unhighlight)
      ;; Highlight word.
      (when keywords
        (font-lock-add-keywords nil keywords 'append)
        (font-lock-fontify-buffer))
      ;; Move point and recenter.
      (and (integerp linum)
           (goto-char (point-min))
           (forward-line (- linum 1)))
      (recenter 3)
      ;; Set header line and button line.
      (setq header-line-format (and sos-candidates-stack
                                    (sos-header-mode-line))
            mode-line-format button-mode-line))))

(defun sos-show-candidates ()
  "Show multiple candidates prompt."
  (sos-with-definition-buffer
    (setq standard-output (current-buffer))
    (kill-all-local-variables)
    (remove-overlays)
    (erase-buffer)
    (dolist (candidate sos-candidates)
      (let* ((symb (plist-get candidate :symbol))
             (doc (plist-get candidate :doc))
             (file (plist-get candidate :file))
             (linum (plist-get candidate :linum))
             (type (plist-get candidate :type)))
        (cond
         ;; File ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (file
          (insert (format "%s | %s | %s | %s\n"
                          (propertize symb
                                      'face 'font-lock-function-name-face)
                          (propertize type
                                      'face 'font-lock-string-face)
                          (propertize (number-to-string linum)
                                      'face 'font-lock-constant-face)
                          (propertize file
                                      'face 'link))))
         ;; Document only ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (t
          (let* ((end (string-match "\n" doc))
                 (doc-line1 (substring-no-properties doc 0 (or (and (< end 48)
                                                                    end)
                                                               48))))
            (insert (format "%s | %s | %s | %s\n"
                            (propertize symb
                                        'face 'font-lock-function-name-face)
                            (propertize type
                                        'face 'font-lock-string-face)
                            (propertize "0"
                                        'face 'font-lock-constant-face)
                            (propertize (concat doc-line1 " ...")
                                        'face 'font-lock-string-face))))))))
    ;; Alignment.
    (align-region 1 (point-max) 'entire
                  `((sos-multiple-candidates
                     (regexp . "^\\([- ]\\|\\w\\)+\\s-\|\\s-\\([- ]\\|\\w\\)+\\s-\|\\s-\\([0-9]\\)+\\s-\|\\s-\\(.\\)+$")
                     (group . (1 2 3))))
                  nil)
    ;; Major mode.
    (fundamental-mode)
    ;; Minor modes.
    (sos-candidates-mode 1)
    (linum-mode 1)
    ;; TODO: use `hl-line-mode' instead.
    ;; Highlight line.
    (goto-char (point-min))
    (sos-hl-line)
    ;; Set header line and button line.
    (setq header-line-format (sos-header-mode-line)
          mode-line-format (sos-bottom-mode-line
                            (format "%s to open it."
                                    (propertize " ENTER "
                                                'face 'tooltip)))
          ;; Disable mouse cursor.
          cursor-type nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signle Candidate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode sos-candidate-mode
  "Minor mode for *Definition* buffers."
  :lighter " SOS:navigation"
  :group 'sos-group
  (if sos-candidate-mode
      (progn
        (use-local-map sos-candidate-mode-map))
    ))

(defvar sos-candidate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'sos-jump-out-candidate)
    (define-key map [return] (lambda ()
                               (interactive)
                               (message "\"Jump to definition\" is yet supported!")))
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode sos-candidates-mode
  "Minor mode for *Definition* buffers."
  :lighter " SOS:mcand"
  :group 'sos-group
  (if sos-candidates-mode
      (progn
        (sos-add-text-button)
        (add-hook 'post-command-hook 'sos-candidates-post-command t t))
    (remove-hook 'post-command-hook 'sos-candidates-post-command t)))

;;;###autoload
(defun sos-next-candidate ()
  (interactive)
  (sos-with-definition-buffer
    (when sos-candidates-mode
      (next-line)
      (sos-candidates-post-command))))

;;;###autoload
(defun sos-prev-candidate ()
  (interactive)
  (sos-with-definition-buffer
    (when sos-candidates-mode
      (previous-line)
      (sos-candidates-post-command))))

(defvar sos-candidates-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'sos-jump-in-candidate)
    (define-key map [mouse-1] 'sos-jump-in-candidate)
    map))

(defvar sos-next-candidate-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'sos-next-candidate)
    map))

(defvar sos-prev-candidate-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'sos-prev-candidate)
    map))

(defvar sos-jump-in-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'sos-jump-in-candidate)
    map))

(defvar sos-jump-out-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] 'sos-jump-out-candidate)
    map))

(defun sos-candidates-post-command ()
  (when (eobp)
    (backward-char))
  (end-of-line)
  (sos-hl-line))

(defun sos-push-candidates-stack ()
  (push sos-candidates sos-candidates-stack))

(defun sos-pop-candidates-stack ()
  (pop sos-candidates-stack))

(defun sos-add-text-button ()
  (save-excursion
    (goto-char (point-min))
    (dolist (candidate sos-candidates)
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (add-text-properties beg end
                             '(mouse-face highlight))
        (add-text-properties beg end
                             `(keymap ,sos-candidates-mode-map)))
      (forward-line))))

(defun sos-hl-line ()
  (unless sos-hl-line-overlay
    (setq sos-hl-line-overlay (make-overlay 1 1))
    (overlay-put sos-hl-line-overlay 'face 'sos-hl-line-face))
  (move-overlay sos-hl-line-overlay (line-beginning-position)
                (line-beginning-position 2)))

(provide 'sos-basic-frontend)
