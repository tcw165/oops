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
     (setq mark-active nil
           sos-candidates arg)
     (if (sos-is-multiple-candidates)
         ;; TODO:
         (progn
           (message "yet support multiple definitions!"))
       (let* ((candidate (nth 0 sos-candidates))
              (file (plist-get candidate :file))
              (linum (plist-get candidate :linum))
              (keywords (plist-get candidate :keywords))
              regexp)
         (when (stringp file)
           (find-file-existing file)
           ;; Line number.
           (when (integerp linum)
             (goto-line linum))
           ;; Keyword.
           (when (and keywords (listp keywords) (car keywords))
             (setq regexp (or (and (stringp (car keywords))
                                   (car keywords))
                              (and (stringp (caar keywords))
                                   (caar keywords))))
             (re-search-forward regexp nil t)
             (let ((beg (match-beginning 1))
                   (end (match-end 1)))
               (when end
                 (goto-char end))
               (when beg
                 (set-marker (mark-marker) beg)
                 (setq mark-active t)))))))
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
        (and (featurep 'history)
             (his-add-position-type-history))
        (unless (string= file (buffer-file-name (window-buffer)))
          (find-file file))
        (goto-line linum)
        (and (featurep 'history)
             (his-add-position-type-history))
        (recenter 5))
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
  (sos-with-definition-buffer
    (let ((select-index (tabulated-list-get-id)))
      (if select-index
          (progn
            (push (cons select-index sos-candidates) sos-candidates-stack)
            (setq sos-candidates `(,(nth select-index sos-candidates)))
            (sos-show-candidate))
        (message "Empty item!")))))

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

(defface sos-hl-symbol-face
  '((t (:background "gold" :foreground "black" :weight bold :height 2.0)))
  "Default face for highlighting keyword in definition window."
  :group 'sos-group)

(defun sos-hl-symbol-face ()
  "Concatenate (background-color . COLOR) and (foreground-color. COLOR) in order
 to prevent to being blocked by `hl-line-mode'."
  (let* ((fg (face-attribute 'sos-hl-symbol-face :foreground))
         (bg (face-attribute 'sos-hl-symbol-face :background))
         facespec)
    (when fg
      (setq facespec
            (append facespec `((foreground-color . ,fg)))))
    (when bg
      (setq facespec
            (append facespec `((background-color . ,bg)))))
    (setq facespec
          (append facespec (list 'sos-hl-symbol-face)))
    facespec))

(defface sos-hl-symbol-parameter-face
  '((t (:background "gold" :foreground "black" :underline t :weight bold :height 1.5)))
  "Default face for highlighting keyword in definition window."
  :group 'sos-group)

(defun sos-hl-symbol-parameter-face ()
  "Concatenate (background-color . COLOR) and (foreground-color. COLOR) in order
 to prevent to being blocked by `hl-line-mode'."
  (let* ((fg (face-attribute 'sos-hl-symbol-parameter-face :foreground))
         (bg (face-attribute 'sos-hl-symbol-parameter-face :background))
         facespec)
    (when fg
      (setq facespec
            (append facespec `((foreground-color . ,fg)))))
    (when bg
      (setq facespec
            (append facespec `((background-color . ,bg)))))
    (setq facespec
          (append facespec (list 'sos-hl-symbol-parameter-face)))
    facespec))

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
     (with-selected-window sos-def-win
       (with-current-buffer sos-def-buf
         (setq buffer-read-only nil)
         ,@body
         (setq sos-is-skip-current-buffer t
               buffer-read-only t)))))

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

;; (defun sos-header-mode-line ()
;;   `("  Multiple Definitions | "
;;     (:eval (when sos-candidates-mode
;;              (format "Choose definition: %s %s "
;;                      (propertize " Up "
;;                                  'face 'custom-button
;;                                  'mouse-face 'custom-button-mouse
;;                                  'local-map sos-prev-candidate-button-map)
;;                      (propertize " Down "
;;                                  'face 'custom-button
;;                                  'mouse-face 'custom-button-mouse
;;                                  'local-map sos-next-candidate-button-map))))
;;     (:eval (if sos-candidates-stack
;;                (format "Back to options: %s "
;;                        (propertize " Back "
;;                                    'face 'custom-button
;;                                    'mouse-face 'custom-button-mouse
;;                                    'local-map sos-jump-out-button-map))
;;              (format "Open definition: %s "
;;                      (propertize " Go "
;;                                  'face 'custom-button
;;                                  'mouse-face 'custom-button-mouse
;;                                  'local-map sos-jump-in-button-map))))))
;; `sos-candidate-mode'

(defun sos-bottom-mode-line (&optional desc file line)
  `(,(propertize "  Definition "
                 'face 'mode-line-buffer-id)
    (:eval (cond
            (sos-candidate-mode
             (when sos-candidates-stack
               (format "| %s to Previous Place "
                       (propertize "Back"
                                   'face 'link
                                   'mouse-face 'highlight
                                   'help-echo "mouse-1: Back to Previous Place."
                                   'local-map sos-jump-out-button-map))))
            (sos-candidates-mode
             (format "| %s to Definition "
                     (propertize "Go"
                                 'face 'link
                                 'mouse-face 'highlight
                                 'help-echo "mouse-1: Go to Definition."
                                 'local-map sos-jump-in-button-map)))))
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
  (let* ((candidate (nth 0 sos-candidates))
         (doc (plist-get candidate :doc))
         (file (plist-get candidate :file))
         (linum (plist-get candidate :linum))
         (keywords (plist-get candidate :keywords))
         (mode-line (plist-get candidate :mode-line))
         (button-mode-line (sos-bottom-mode-line mode-line file linum)))
    (sos-with-definition-buffer
      (kill-all-local-variables)
      (remove-overlays)
      (erase-buffer)
      ;; Insert content and set major mode.
      (or (when doc
            (insert doc)
            ;; Fundamental mode.
            (fundamental-mode))
          (when (and file (file-exists-p file))
            (insert-file-contents file)
            ;; Change major-mode refer to file name.
            (dolist (mode auto-mode-alist)
              (and (not (null (cdr mode)))
                   (string-match (car mode) file)
                   (funcall (cdr mode))))))
      ;; Minor mode.
      (sos-candidate-mode 1)
      (hl-line-unhighlight)
      ;; Highlight word.
      (when keywords
        (font-lock-add-keywords nil keywords 'append)
        (font-lock-fontify-buffer)
        ;; Use `hl-highlight-mode' to prevent highlights to being blocked.
        (when (featurep 'hl-anything)
          (setq hl-is-always-overlays-local t)
          (hl-highlight-mode 1)))
      ;; Move point and recenter.
      (when (integerp linum)
        (goto-char (point-min))
        (forward-line (- linum 1))
        (end-of-line)
        (recenter 3))
      ;; Set button line.
      (setq mode-line-format button-mode-line))))

(defun sos-show-candidates (&optional select-index)
  "Show multiple candidates prompt."
  ;; TODO: Check if there're candidates need to be showed immediately.
  (sos-with-definition-buffer
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
                                     ,(or file
                                          (with-temp-buffer
                                            (insert doc)
                                            (goto-char 1)
                                            (buffer-substring-no-properties
                                             1 (line-end-position))))]))))
          (when entry
            (setq entries (append entries `(,entry))
                  id (1+ id)))))
      (setq tabulated-list-format '[("symbol" 24 t)
                                    ("type" 20 t)
                                    ("linum" 6 t)
                                    ("file" 64 t)]
            tabulated-list-entries entries))
    ;; Major mode.
    (tabulated-list-mode)
    (tabulated-list-init-header)
    (tabulated-list-print)
    ;; Minor mode.
    (sos-candidates-mode 1)
    (linum-mode 1)
    (hl-line-mode 1)
    ;; Try to recover the selection.
    (when (and select-index (integerp select-index))
      (goto-line (1+ select-index)))
    ;; Set header line and button line.
    (setq mode-line-format (sos-bottom-mode-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signle Candidate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode sos-candidate-mode
  "Minor mode for *Definition* buffers."
  :lighter " sos:candidate"
  :group 'sos-group
  (if sos-candidate-mode
      (progn
        (use-local-map sos-candidate-mode-map)
        (setq buffer-read-only t))
    (setq buffer-read-only nil)))

(defvar sos-candidate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] 'sos-jump-out-candidate)
    (define-key map [return] (lambda ()
                               (interactive)
                               (message "\"Jump to definition\" is yet supported!")))
    map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-minor-mode sos-candidates-mode
  "doc"
  :lighter "sos:candidates"
  :group 'sos-group
  (if sos-candidates-mode
      (progn
        (use-local-map sos-candidates-mode-map)
        (setq buffer-read-only t)
        (setq-local hl-line-sticky-flag t))
    (setq buffer-read-only nil)))

;;;###autoload
(defun sos-next-candidate ()
  (interactive)
  (sos-with-definition-buffer
    (when sos-candidates-mode
      (next-line))))

;;;###autoload
(defun sos-prev-candidate ()
  (interactive)
  (sos-with-definition-buffer
    (when sos-candidates-mode
      (previous-line))))

(defvar sos-candidates-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [?e] 'sos-jump-in-candidate)
    (define-key map [return] 'sos-jump-in-candidate)
    (define-key map [mouse-1] 'sos-jump-in-candidate)
    map))

(defvar sos-jump-in-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-jump-in-candidate)
    map))

(defvar sos-jump-out-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'sos-jump-out-candidate)
    map))

(provide 'sos-basic-frontend)
