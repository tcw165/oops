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
(require 'tabulated-list)
(require 'thingatpt)

(require 'history)
(require 'hl-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sos-def-buf nil
  "Definition buffer.")

(defvar sos-def-win nil
  "Definition window.")

(defvar sos-def-win-height 0
  "The height of definition window.")

(defvar sos-candidates nil
  "Cache the return value from back-end with `:candidates' command.")

(defvar sos-candidates-stack nil
  "A list containing `sos-candidates'. Engine will push the current candidates 
into the stack when user navigate to deeper definition in the definition window.")

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
     (with-selected-window sos-def-win
       (with-current-buffer sos-def-buf
         (setq buffer-read-only nil)
         ,@body
         ;; Avoid the engine to scan this buffer.
         (setq sos-is-skip t
               buffer-read-only t)))))

(defun sos-toggle-definition-buffer&window (toggle)
  "Display or hide the `sos-def-buf' and `sos-def-win'."
  (let ((enabled (or (and (booleanp toggle) toggle)
                     (and (numberp toggle)
                          (> toggle 0)))))
    (if enabled
        (sos-with-definition-buffer
          (setq header-line-format nil
                mode-line-format (sos-candidate-mode-line)))
      (when (windowp sos-def-win)
        (delete-window sos-def-win))
      (when (bufferp sos-def-buf)
        (kill-buffer sos-def-buf))
      (setq sos-def-buf nil
            sos-def-win nil))))

(defun sos-is-multiple-candidates ()
  (> (length sos-candidates) 1))

(defun sos-show-candidate ()
  "Show single candidate prompt."
  (let* ((candidate (nth 0 sos-candidates))
         (doc (plist-get candidate :doc))
         (file (plist-get candidate :file))
         (linum (plist-get candidate :linum))
         (keywords (plist-get candidate :keywords)))
    (sos-with-definition-buffer
      (kill-all-local-variables)
      (remove-overlays)
      ;; Insert content and set major mode.
      (or (when doc
            (erase-buffer)
            (insert doc))
          (when (and file (file-exists-p file))
            (insert-file-contents-literally file nil nil nil t)
            ;; Change major-mode refer to file name.
            (dolist (mode auto-mode-alist)
              (and (not (null (cdr mode)))
                   (string-match (car mode) file)
                   (funcall (cdr mode))))))
      ;; Minor mode.
      (sos-candidate-mode 1)
      ;; Move point and recenter.
      (when (integerp linum)
        (goto-char 1)
        (end-of-line linum)
        (recenter 3))
      ;; Highlight word.
      (when keywords
        (font-lock-add-keywords nil keywords 'append)
        (font-lock-fontify-buffer)))))

(defun sos-show-candidates (&optional select-index)
  "Show multiple candidates prompt."
  ;; TODO: Check if there're candidates need to be showed immediately.
  (sos-with-definition-buffer
    ;; Insert condidates.
    (sos-show-candidates-common)
    ;; Major mode.
    (sos-candidates-mode)
    ;; Recover the selection in the last session.
    (when (and select-index (integerp select-index))
      (goto-line (1+ select-index)))))

(defun sos-show-candidates-common ()
  "Show multiple candidates prompt."
  ;; TODO: Check if there're candidates need to be showed immediately.
  (kill-all-local-variables)
  (remove-overlays)
  (erase-buffer)
  ;; Content
  (let ((id 0)
        entries)
    (dolist (candidate sos-candidates)
      (let* ((symb (plist-get candidate :symbol))
             (type (plist-get candidate :type))
             (doc (plist-get candidate :doc))
             (linum (plist-get candidate :linum))
             (file (plist-get candidate :file))
             (show (plist-get candidate :show))
             (entry (when (and symb (stringp symb)
                               type (stringp type)
                               linum (integerp linum)
                               (or (and file (stringp file))
                                   (and doc (stringp doc))))
                      `(,id [,symb ,type ,(number-to-string linum)
                                   ,(or file "n/a")
                                   ;; Hidden information for document type 
                                   ;; candidate.
                                   ,doc]))))
        (when entry
          (setq entries (append entries `(,entry))
                id (1+ id)))))
    (setq tabulated-list-format '[("symbol" 24 t)
                                  ("type" 16 t)
                                  ("linum" 6 t)
                                  ("file" 128 t)]
          tabulated-list-entries entries)))

(defun sos-goto-definition-from-candidates-common ()
  (let* ((candidate (nth 0 sos-candidates))
         (file (plist-get candidate :file))
         (linum (plist-get candidate :linum))
         (keywords (plist-get candidate :keywords))
         regexp)
    (when (and (stringp file)
               (file-exists-p file))
      (his-add-history)
      (unless (string= (buffer-file-name) file)
        (find-file file))
      ;; Line number.
      (when (integerp linum)
        (goto-char 1)
        (end-of-line linum)
        (recenter 3))
      (end-of-line)
      ;; Keyword.
      ;; (when keywords
      ;;   (hl-highlight-keywords-local&temp keywords))
      (his-add-history))))

(defun sos-right-char ()
  (let (pos)
    (save-excursion
      (right-char)
      (unless (eobp)
        (setq pos (point))))
    (and pos (goto-char pos))))

(defun sos-left-char ()
  (right-char -1))

(defun sos-next-line ()
  (let (pos)
    (save-excursion
      (next-line)
      (unless (eobp)
        (setq pos (point))))
    (and pos (goto-char pos))))

(defun sos-previous-line ()
  (previous-line))

;;;###autoload
(defun sos-back-to-source-window ()
  (interactive)
  (when (window-live-p sos-source-window)
    (select-window sos-source-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signle Candidate Preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sos-file-path-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-open-candidate)
    (define-key map [mode-line mouse-3] 'sos-copy-candidate-filepath)
    map))

(defvar sos-candidate-mode-hook '((lambda ()
                                    (linum-mode 1)
                                    (hl-line-mode 1)
                                    (hl-line-unhighlight))
                                  (lambda ()
                                    ;; Use `hl-highlight-mode' to prevent highlights to being blocked.
                                    (setq hl-is-highlight-special-faces t)
                                    (hl-highlight-mode 1))))

(defvar sos-linum-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-goto-definition-line)
    map))

(defvar sos-candidate-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [?q] 'sos-back-to-source-window)
    (define-key map [return] 'sos-open-candidate)
    map))

(defun sos-candidate-mode-line (&optional file line)
  `(,(propertize "  Definition " 'face 'mode-line-buffer-id)
    (:eval (when sos-candidates-stack
             (format "| %s to Previous Place "
                     (propertize "Back"
                                 'face 'link
                                 'mouse-face 'highlight
                                 'help-echo "mouse-1: Back to Previous Place."
                                 'local-map sos-jump-out-button-map))))
    (:eval (and (stringp ,file) (file-exists-p ,file) (integerp ,line)
                (format "| file:%s, line:%s, function:(yet supported) "
                        (propertize (abbreviate-file-name ,file)
                                    'face 'link
                                    'mouse-face 'highlight
                                    'help-echo "mouse-1: Open the file.\nmouse-3: Copy the path."
                                    'local-map sos-file-path-map)
                        (propertize (number-to-string ,line)
                                    'face 'link
                                    'mouse-face 'highlight
                                    'help-echo "mouse-1: Back to the line."
                                    'local-map sos-linum-map))))))

;;;###autoload
(defun sos-open-candidate ()
  "Open file and goto the line for the situation of single candidate."
  (interactive)
  (let* ((candidate (nth 0 sos-candidates))
         (file (plist-get candidate :file))
         (linum (plist-get candidate :linum)))
    (when (and (stringp file) (file-exists-p file)
               (integerp linum)
               (window-live-p sos-source-window))
      (with-selected-window sos-source-window
        (his-add-position-type-history)
        (unless (string= file (buffer-file-name (window-buffer)))
          (find-file file))
        (goto-char 1)
        (end-of-line linum)
        (his-add-position-type-history)
        (recenter 3))
      (select-window sos-source-window))))

;;;###autoload
(defun sos-copy-candidate-filepath ()
  "Copy file path for the situation of single candidate."
  (interactive)
  (unless (sos-is-multiple-candidates)
    (let* ((candidate (nth 0 sos-candidates))
           (file (plist-get candidate :file)))
      (when (and file (stringp file))
        (with-temp-buffer
          (insert file)
          (clipboard-kill-ring-save 1 (point-max)))
        (message "File path is copied to clipboard!")))))

;;;###autoload
(defun sos-goto-definition-line ()
  "Go to line where the definition is for the situation of single candidate."
  (interactive)
  (unless (sos-is-multiple-candidates)
    (let* ((candidate (nth 0 sos-candidates))
           (linum (plist-get candidate :linum)))
      (when (and linum (integerp linum))
        (sos-with-definition-buffer
          (goto-line linum)
          (recenter 3))))))

;;;###autoload
(define-minor-mode sos-candidate-mode
  "Minor mode for *Definition* buffers."
  :lighter " sos:candidate"
  :group 'sos-group
  (if sos-candidate-mode
      (let* ((candidate (nth 0 sos-candidates))
             (file (plist-get candidate :file))
             (linum (plist-get candidate :linum)))
        (use-local-map sos-candidate-mode-map)
        (setq mode-line-format (sos-candidate-mode-line file linum)
              buffer-read-only t))
    (setq buffer-read-only nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Candidates Preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sos-candidates-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [?e] 'sos-jump-in-candidate)
    (define-key map [?q] 'sos-back-to-source-window)
    (define-key map [return] 'sos-jump-in-candidate)
    (define-key map [left] '(lambda () (interactive) (sos-left-char)))
    (define-key map [right] '(lambda () (interactive) (sos-right-char)))
    (define-key map [up] '(lambda () (interactive) (sos-previous-line)))
    (define-key map [down] '(lambda () (interactive) (sos-next-line)))
    (define-key map [mouse-1] 'sos-jump-in-candidate)
    map))

(defvar sos-candidates-mode-hook `(linum-mode
                                   hl-line-mode))

(defvar sos-jump-in-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-jump-in-candidate)
    map))

(defvar sos-jump-out-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-jump-out-candidate)
    map))

(defun sos-candidates-mode-line ()
  `(,(propertize "  Definition "
                 'face 'mode-line-buffer-id)
    (format "| %s to Definition "
            (propertize "Go"
                        'face 'link
                        'mouse-face 'highlight
                        'help-echo "mouse-1: Go to Definition."
                        'local-map sos-jump-in-button-map))))

;;;###autoload
(defun sos-next-candidate ()
  (interactive)
  (when (sos-is-multiple-candidates)
    (sos-with-definition-buffer
      (sos-next-line))))

;;;###autoload
(defun sos-prev-candidate ()
  (interactive)
  (when (sos-is-multiple-candidates)
    (sos-with-definition-buffer
      (sos-previous-line))))

;;;###autoload
(defun sos-jump-in-candidate ()
  (interactive)
  (sos-with-definition-buffer
    (let ((select-index (tabulated-list-get-id)))
      (if select-index
          (progn
            (push (cons select-index sos-candidates) sos-candidates-stack)
            (setq sos-candidates `(,(nth select-index sos-candidates)))
            (sos-show-candidate))
        (message "Invalid choice!")))))

;;;###autoload
(defun sos-jump-out-candidate ()
  (interactive)
  (let* ((info (pop sos-candidates-stack))
         (select-index (car info))
         (candidates (cdr info)))
    (when candidates
      (setq sos-candidates candidates)
      (if (sos-is-multiple-candidates)
          (sos-show-candidates select-index)
        (sos-show-candidate)))))

;;;###autoload
(define-derived-mode sos-candidates-mode tabulated-list-mode
  "sos:candidates"
  "doc"
  :group 'sos-group
  (setq mode-line-format (sos-candidates-mode-line)
        buffer-read-only t)
  ;; Make highlight line sticky only for current buffer.
  (setq-local hl-line-sticky-flag t)
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Candidates Preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sos-goto-candidates-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [?e] '(lambda () (interactive) (sos-goto-definition-from-candidates)))
    (define-key map [?q] '(lambda () (interactive) (kill-buffer (current-buffer))))
    (define-key map [escape] '(lambda () (interactive) (kill-buffer (current-buffer))))
    (define-key map [left] '(lambda () (interactive) (sos-left-char)))
    (define-key map [right] '(lambda () (interactive) (sos-right-char)))
    (define-key map [up] '(lambda () (interactive) (sos-previous-line)))
    (define-key map [down] '(lambda () (interactive) (sos-next-line)))
    (define-key map [return] '(lambda () (interactive) (sos-goto-definition-from-candidates)))
    map))

(defvar sos-goto-candidates-mode-hook `(linum-mode
                                        hl-line-mode))

(defun sos-goto-definition-from-candidates ()
  (let ((entry (tabulated-list-get-entry)))
    (if entry
        ;; Open definition if there is a file.
        (if (file-exists-p (aref entry 3))
            (progn
              (kill-buffer)
              (sos-goto-definition-from-candidates-common))
          (message "It is only a doucmentation, so cannot open this type of definition."))
      (message "Invalid choice!"))))

;;;###autoload
(define-derived-mode sos-goto-candidates-mode tabulated-list-mode
  "sos:goto-definitions"
  "doc"
  :group 'sos-group
  ;; Make highlight line sticky only for current buffer.
  (setq-local hl-line-sticky-flag t)
  ;; Mode line.
  (setq mode-line-format `(,(propertize "  Goto Definition "
                                        'face 'mode-line-buffer-id)
                           ,(format "| UP/DOWN to select; ENTER to open; q to exit.")))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Front-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun sos-definition-buffer-frontend (command &rest args)
  (case command
    (:init (sos-toggle-definition-buffer&window 1))
    (:destroy (sos-toggle-definition-buffer&window -1))
    (:show
     ;; TODO: remember user choice at last session in the prj.
     (setq sos-candidates (car args)
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
(defun sos-goto-definitions-frontend (command &rest args)
  (case command
    (:show
     (setq sos-candidates (car args))
     (let ((buf-name "*Goto Definition*"))
       (if (sos-is-multiple-candidates)
           ;; Multiple Candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (let ((buffer (get-buffer-create buf-name)))
             (his-add-history)
             (switch-to-buffer buffer)
             (setq buffer-read-only nil)
             ;; Insert condidates.
             (sos-show-candidates-common)
             ;; Major mode.
             (sos-goto-candidates-mode))
         ;; Single Candidate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (ignore-errors
           (kill-buffer buf-name))
         (sos-goto-definition-from-candidates-common))))))

(provide 'sos-default-frontend)
