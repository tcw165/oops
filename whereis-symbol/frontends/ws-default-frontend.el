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

;; GNU Library.
(require 'font-lock)
(require 'hl-line)
(require 'linum)
(require 'tabulated-list)
(eval-when-compile (require 'cl))

;; 3rd Party Library.
(require 'history)
(require 'hl-anything)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ws-symbol-preview-buffer nil
  "Definition buffer.")

(defvar ws-symbol-preview-window nil
  "Definition window.")

(defvar ws-symbol-preview-window-height 0
  "The height of definition window.")

(defvar ws-candidates nil
  "Cache the return value from back-end with `:candidates' command.")

(defvar ws-candidates-stack nil
  "A list containing `ws-candidates'. Engine will push the current candidates 
into the stack when user navigate to deeper definition in the definition window.")

(defmacro ws-with-symbol-preview-window (&rest body)
  "Get definition buffer and window ready then interpret the `body'."
  (declare (indent 0) (debug t))
  `(progn
     (unless ws-symbol-preview-buffer
       (setq ws-symbol-preview-buffer (get-buffer-create "*Definition*")))
     (unless (window-live-p ws-symbol-preview-window)
       (let* ((win (cond
                    ;; Outline window is present ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    ;; TODO:
                    ;; Default ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    (t (frame-root-window))))
              (height (or (and (> ws-symbol-preview-window-height 0)
                               (- 0 ws-symbol-preview-window-height))
                          (and win
                               (/ (window-height win) -3)))))
         (and win height
              (setq ws-symbol-preview-window (split-window win height 'below)))))
     ;; Bind definition buffer to definition window.
     (set-window-buffer ws-symbol-preview-window ws-symbol-preview-buffer t)
     (with-selected-window ws-symbol-preview-window
       (with-current-buffer ws-symbol-preview-buffer
         (setq buffer-read-only nil)
         ,@body
         ;; Avoid the engine to scan this buffer.
         (setq ws-is-skip t
               buffer-read-only t)))))

(defun ws-toggle-symbol-preview-window (toggle)
  "Display or hide the `ws-symbol-preview-buffer' and `ws-symbol-preview-window'."
  (let ((enabled (or (and (booleanp toggle) toggle)
                     (and (numberp toggle)
                          (> toggle 0)))))
    (if enabled
        (ws-with-symbol-preview-window
          (setq header-line-format nil
                mode-line-format (ws-candidate-mode-line)))
      (when (windowp ws-symbol-preview-window)
        (delete-window ws-symbol-preview-window))
      (when (bufferp ws-symbol-preview-buffer)
        (kill-buffer ws-symbol-preview-buffer))
      (setq ws-symbol-preview-buffer nil
            ws-symbol-preview-window nil))))

(defun ws-is-multiple-candidates ()
  (> (length ws-candidates) 1))

(defun ws-show-candidate ()
  "Show single candidate prompt."
  (let* ((candidate (nth 0 ws-candidates))
         (doc (plist-get candidate :doc))
         (file (plist-get candidate :file))
         (linum (or (plist-get candidate :linum) 1))
         (keywords (plist-get candidate :keywords)))
    (and candidate
         (ws-with-symbol-preview-window
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
           ;; Move point and recenter.
           (when (integerp linum)
             (goto-char 1)
             (end-of-line linum)
             (recenter 3))
           ;; Highlight word.
           (when keywords
             (font-lock-add-keywords nil keywords 'append)
             (font-lock-fontify-buffer))
           ;; Minor mode.
           (ws-candidate-mode 1)))))

(defun ws-show-candidates (&optional select-index)
  "Show multiple candidates prompt."
  ;; TODO: Check if there're candidates need to be showed immediately.
  (ws-with-symbol-preview-window
    ;; Insert condidates.
    (ws-show-candidates-common)
    ;; Major mode.
    (ws-candidates-mode)
    ;; Recover the selection in the last session.
    (when (and select-index (integerp select-index))
      (forward-line select-index))))

(defun ws-show-candidates-common ()
  "Show multiple candidates prompt."
  ;; TODO: Check if there're candidates need to be showed immediately.
  (kill-all-local-variables)
  (remove-overlays)
  (erase-buffer)
  ;; Content
  (let ((id 0)
        entries)
    (setq tabulated-list-format '[("symbol" 24 t)
                                  ("type" 16 t)
                                  ("linum" 6 t)
                                  ("file" 128 t)]
          tabulated-list-entries nil)
    (dolist (candidate ws-candidates)
      (let* ((symb (plist-get candidate :symbol))
             (type (plist-get candidate :type))
             (doc (plist-get candidate :doc))
             (linum (or (plist-get candidate :linum) 1))
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
          (push entry tabulated-list-entries)
          (incf id))))
    (setq tabulated-list-entries (nreverse tabulated-list-entries))))

(defun ws-goto-symbol-from-candidates-common ()
  (let* ((candidate (nth 0 ws-candidates))
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
      ;;   (hl-highlight-keywords-temporarily keywords))
      (his-add-history))))

(defun ws-right-char ()
  (let (pos)
    (save-excursion
      (right-char)
      (unless (eobp)
        (setq pos (point))))
    (and pos (goto-char pos))))

(defun ws-left-char ()
  (right-char -1))

(defun ws-next-line ()
  (let (pos)
    (save-excursion
      (next-line)
      (unless (eobp)
        (setq pos (point))))
    (and pos (goto-char pos))))

(defun ws-previous-line ()
  (previous-line))

;;;###autoload
(defun ws-back-to-source-window ()
  (interactive)
  (when (window-live-p ws-source-window)
    (select-window ws-source-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signle Candidate Preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ws-file-path-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'ws-open-candidate)
    (define-key map [mode-line mouse-3] 'ws-copy-candidate-filepath)
    map))

(defvar ws-candidate-mode-hook '((lambda ()
                                   (linum-mode 1)
                                   (setq-local hl-line-sticky-flag t)
                                   (hl-line-mode 1))
                                 (lambda ()
                                   ;; Use `hl-highlight-mode' to prevent highlights to being blocked.
                                   (setq hl-is-highlight-special-faces t)
                                   (hl-highlight-mode 1))))

(defvar ws-linum-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'ws-goto-definition-line)
    map))

(defvar ws-candidate-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [?q] 'ws-jump-out-candidate)
    (define-key map [escape] 'ws-back-to-source-window)
    (define-key map [return] 'ws-open-candidate)
    map))

(defun ws-candidate-mode-line (&optional file line)
  `(,(propertize "  Definition " 'face 'mode-line-buffer-id)
    (:eval (when ws-candidates-stack
             (format "| %s to Previous Place "
                     (propertize "Back"
                                 'face 'link
                                 'mouse-face 'highlight
                                 'help-echo "mouse-1: Back to Previous Place."
                                 'local-map ws-jump-out-button-map))))
    (:eval (and (stringp ,file) (file-exists-p ,file) (integerp ,line)
                (format "| file:%s, line:%s, function:(yet supported) "
                        (propertize (abbreviate-file-name ,file)
                                    'face 'link
                                    'mouse-face 'highlight
                                    'help-echo "mouse-1: Open the file.\nmouse-3: Copy the path."
                                    'local-map ws-file-path-map)
                        (propertize (number-to-string ,line)
                                    'face 'link
                                    'mouse-face 'highlight
                                    'help-echo "mouse-1: Back to the line."
                                    'local-map ws-linum-map))))))

;;;###autoload
(defun ws-open-candidate ()
  "Open file and goto the line for the situation of single candidate."
  (interactive)
  (let* ((candidate (nth 0 ws-candidates))
         (file (plist-get candidate :file))
         (linum (plist-get candidate :linum)))
    (when (and (stringp file) (file-exists-p file)
               (integerp linum)
               (window-live-p ws-source-window))
      (with-selected-window ws-source-window
        (his-add-position-type-history)
        (unless (string= file (buffer-file-name (window-buffer)))
          (find-file file))
        (goto-char 1)
        (end-of-line linum)
        (his-add-position-type-history)
        (recenter 3))
      (select-window ws-source-window))))

;;;###autoload
(defun ws-copy-candidate-filepath ()
  "Copy file path for the situation of single candidate."
  (interactive)
  (unless (ws-is-multiple-candidates)
    (let* ((candidate (nth 0 ws-candidates))
           (file (plist-get candidate :file)))
      (when (and file (stringp file))
        (with-temp-buffer
          (insert file)
          (clipboard-kill-ring-save 1 (point-max)))
        (message "File path is copied to clipboard!")))))

;;;###autoload
(defun ws-goto-definition-line ()
  "Go to line where the definition is for the situation of single candidate."
  (interactive)
  (unless (ws-is-multiple-candidates)
    (let* ((candidate (nth 0 ws-candidates))
           (linum (plist-get candidate :linum)))
      (when (and linum (integerp linum))
        (ws-with-symbol-preview-window
          (goto-char 1)
          (forward-line (1- linum))
          (end-of-line)
          (recenter 3))))))

;;;###autoload
(define-minor-mode ws-candidate-mode
  "Minor mode for *Definition* buffers."
  :lighter " ws:candidate"
  :group 'whereis-symbol
  (if ws-candidate-mode
      (let* ((candidate (nth 0 ws-candidates))
             (file (plist-get candidate :file))
             (linum (plist-get candidate :linum)))
        (use-local-map ws-candidate-mode-map)
        (setq mode-line-format (ws-candidate-mode-line file linum)
              buffer-read-only t))
    (setq buffer-read-only nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Candidates Preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ws-candidates-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [?e] 'ws-jump-in-candidate)
    (define-key map [return] 'ws-jump-in-candidate)
    (define-key map [escape] 'ws-back-to-source-window)
    (define-key map [left] '(lambda () (interactive) (ws-left-char)))
    (define-key map [right] '(lambda () (interactive) (ws-right-char)))
    (define-key map [up] '(lambda () (interactive) (ws-previous-line)))
    (define-key map [down] '(lambda () (interactive) (ws-next-line)))
    (define-key map [mouse-1] 'ws-jump-in-candidate)
    map))

(defvar ws-candidates-mode-hook `(linum-mode
                                  hl-line-mode))

(defvar ws-jump-in-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'ws-jump-in-candidate)
    map))

(defvar ws-jump-out-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'ws-jump-out-candidate)
    map))

(defun ws-candidates-mode-line ()
  `(,(propertize "  Definition "
                 'face 'mode-line-buffer-id)
    (format "| %s to Definition "
            (propertize "Go"
                        'face 'link
                        'mouse-face 'highlight
                        'help-echo "mouse-1: Go to Definition."
                        'local-map ws-jump-in-button-map))))

;;;###autoload
(defun ws-next-candidate ()
  (interactive)
  (when (ws-is-multiple-candidates)
    (ws-with-symbol-preview-window
      (ws-next-line))))

;;;###autoload
(defun ws-prev-candidate ()
  (interactive)
  (when (ws-is-multiple-candidates)
    (ws-with-symbol-preview-window
      (ws-previous-line))))

;;;###autoload
(defun ws-jump-in-candidate ()
  (interactive)
  (ws-with-symbol-preview-window
    (let ((select-index (tabulated-list-get-id)))
      (if select-index
          (progn
            (push (cons select-index ws-candidates) ws-candidates-stack)
            (setq ws-candidates `(,(nth select-index ws-candidates)))
            (ws-show-candidate))
        (message "Invalid choice!")))))

;;;###autoload
(defun ws-jump-out-candidate ()
  (interactive)
  (let* ((info (pop ws-candidates-stack))
         (select-index (car info))
         (candidates (cdr info)))
    (when candidates
      (setq ws-candidates candidates)
      (if (ws-is-multiple-candidates)
          (ws-show-candidates select-index)
        (ws-show-candidate)))))

;;;###autoload
(define-derived-mode ws-candidates-mode tabulated-list-mode
  "sos:candidates"
  "doc"
  :group 'whereis-symbol
  (setq mode-line-format (ws-candidates-mode-line)
        buffer-read-only t)
  ;; Make highlight line sticky only for current buffer.
  (setq-local hl-line-sticky-flag t)
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Candidates Preview ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ws-goto-candidates-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [?e] '(lambda ()
                            (interactive)
                            (ws-goto-symbol-from-candidates)))
    (define-key map [?q] '(lambda ()
                            (interactive)
                            (kill-buffer (current-buffer))))
    (define-key map [escape] '(lambda ()
                                (interactive)
                                (kill-buffer (current-buffer))))
    (define-key map [left] '(lambda () (interactive) (ws-left-char)))
    (define-key map [right] '(lambda () (interactive) (ws-right-char)))
    (define-key map [up] '(lambda () (interactive) (ws-previous-line)))
    (define-key map [down] '(lambda () (interactive) (ws-next-line)))
    (define-key map [return] '(lambda ()
                                (interactive)
                                (ws-goto-symbol-from-candidates)))
    map))

(defvar ws-goto-candidates-mode-hook `(linum-mode
                                       hl-line-mode))

(defun ws-goto-symbol-from-candidates ()
  (let ((entry (tabulated-list-get-entry)))
    (if entry
        ;; Open definition if there is a file.
        (if (file-exists-p (aref entry 3))
            (progn
              (kill-buffer)
              (ws-goto-symbol-from-candidates-common))
          (message "It is only a doucmentation, so cannot open this type of definition."))
      (message "Invalid choice!"))))

;;;###autoload
(define-derived-mode ws-goto-candidates-mode tabulated-list-mode
  "sos:goto-definitions"
  "doc"
  :group 'whereis-symbol
  ;; Make highlight line sticky only for current buffer.
  (setq-local hl-line-sticky-flag t)
  ;; Mode line.
  (setq mode-line-format `(,(propertize "  Goto Definition "
                                        'face 'mode-line-buffer-id)
                           ,(format "| UP/DOWN to select; ENTER to open; q to exit.")))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search Symbol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom ws-search-symbol-display-lines 10
  "Maximum lines of symbols to be showed when searching local or global symbol."
  :type 'integer
  :group 'whereis-symbol)

(defvar ws-search-symbol-timer nil)

(defvar ws-search-symbol-index 0)

(defvar ws-symbol-searcher nil
  "A function provided by engine. The frontend can call this function to get 
CANDIDATES list.")

(defvar ws-minibuffer-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [up] 'ws-selection-1)
    (define-key map [down] 'ws-selection+1)
    (define-key map [prior] 'ws-selection-page)
    (define-key map [next] 'ws-selection+page)
    map))

(defun ws-candidates-to-string (candidates)
  (let* ((candidate (nthcdr ws-search-symbol-index candidates))
         (total (min ws-search-symbol-display-lines
                     (length candidates)))
         (lines total)
         string)
    (while (> lines 0)
      (setq string (concat string
                           "  " (plist-get (car candidate) :symbol)
                           (and (> lines 1) "\n"))
            candidate (cdr candidate))
      (decf lines))
    (cons total string)))

(defun ws-search-symbol-mode-line ()
  "-----")

(defun ws-display-search-result ()
  (delete-all-overlays)
  (let* ((data (ws-candidates-to-string (funcall ws-symbol-searcher
                                                 (minibuffer-contents))))
         (lines (car data))
         (matches (cdr data))
         (mode-line (ws-search-symbol-mode-line)))
    (overlay-put (make-overlay (point-min) (point-min))
                 'before-string
                 (format "%s\n%s\n" matches mode-line))
    (set-window-text-height nil (max 3 (+ lines 2)))))

(defun ws-exit-minibuffer-hook ()
  (ws-minibuffer-search-mode -1))

;;;###autoload
(defun ws-selection+1 ()
  (interactive))

;;;###autoload
(defun ws-selection-1 ()
  (interactive))

;;;###autoload
(defun ws-selection+page ()
  (interactive))

;;;###autoload
(defun ws-selection-page ()
  (interactive))

;;;###autoload
(define-minor-mode ws-minibuffer-search-mode
  ""
  :group 'whereis-symbol
  (unless (minibufferp)
    (error "The minor-mode is only for minibuffer!"))
  (if ws-minibuffer-search-mode
      (progn
        (setq ws-search-symbol-timer (run-with-idle-timer 0.3 t
                                                          'ws-display-search-result)
              ws-search-symbol-index 0)
        (add-hook 'minibuffer-exit-hook 'ws-exit-minibuffer-hook nil t))
    (when (timerp ws-search-symbol-timer)
      (cancel-timer ws-search-symbol-timer)
      (setq ws-search-symbol-timer nil))
    (remove-hook 'minibuffer-exit-hook 'ws-exit-minibuffer-hook t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Front-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ws-symbol-preview-frontend (command &rest args)
  (case command
    (:init (ws-toggle-symbol-preview-window 1))
    (:destroy (ws-toggle-symbol-preview-window -1))
    (:show
     ;; TODO: remember user choice at last session in the project.
     (setq ws-candidates (car args)
           ws-index 0
           ;; Clean the stack.
           ws-candidates-stack nil)
     (and ws-candidates
          (ws-toggle-symbol-preview-window 1)
          (if (ws-is-multiple-candidates)
              (ws-show-candidates)
            (ws-show-candidate)))))
  ;; Save the height of definition window.
  (and (window-live-p ws-symbol-preview-window)
       (setq ws-symbol-preview-window-height (window-height
                                              ws-symbol-preview-window))))

;;;###autoload
(defun ws-goto-symbol-frontend (command &rest args)
  (case command
    (:show
     (setq ws-candidates (car args))
     (let ((buf-name "*Goto Definition*"))
       (if (ws-is-multiple-candidates)
           ;; Multiple Candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (let ((buffer (get-buffer-create buf-name)))
             (his-add-history)
             (switch-to-buffer buffer)
             (setq buffer-read-only nil)
             ;; Insert condidates.
             (ws-show-candidates-common)
             ;; Major mode.
             (ws-goto-candidates-mode))
         ;; Single Candidate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (ignore-errors
           (kill-buffer buf-name))
         (ws-goto-symbol-from-candidates-common))))))

;;;###autoload
(defun ws-fuzzy-search-symbol-frontend (command &rest args)
  (case command
    (:show
     (setq ws-symbol-searcher (nth 0 args))
     (minibuffer-with-setup-hook
         (lambda ()
           (ws-minibuffer-search-mode 1)
           (ws-display-search-result))
       (read-from-minibuffer ">>> ")))))

(provide 'ws-default-frontend)
;;; ws-default-frontend.el ends here
