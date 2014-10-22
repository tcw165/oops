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

;; Built-in extensions
(require 'font-lock)
(require 'saveplace)

;; 3rd party extensions.
(require 'hl-anything)

(defgroup grep-mode-group nil
  "Major mode for text outputed by unix-liked grep."
  :tag "Grep")

(defcustom grep-mode-hook `(save-place-find-file-hook
                            font-lock-mode
                            linum-mode
                            hl-line-mode
                            hl-highlight-mode)
  "Hook run when entering `grep-mode' mode."
  :type 'hook
  :group 'grep-mode-group)

(defvar grep-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    ;; (define-key map [up] )
    ;; (define-key map [down] )
    (define-key map [return] 'grep-open-item)
    (define-key map [?q] 'grep-kill-buffer)
    (define-key map [escape] 'grep-kill-buffer)
    (define-key map [?d] 'grep-kill-item-at-point)
    map))

(defvar grep-mode-font-lock-keywords
  '((("^\\([[:alnum:] $_\/.+-]+\\):\\([0-9]+\\):.*$" (1 'hl-file-face) (2 'hl-number-face))
     ("^\\(>>>>>\\s-\\)\\(.+\\)$" (1 'hl-title-3-face) (2 'hl-symbol-face))
     ("^\\(<<<<<\\)$" (1 'hl-title-3-face)))
    ;; don't use syntactic fontification.
    t
    ;; Case insensitive.
    nil))

(defvar grep-mode-bottom-line
  `(,(format "  %s | line:%s, col:%s"
             (propertize "Search Result" 'face 'mode-line-buffer-id)
             (propertize "%l" 'face 'link)
             (propertize "%c" 'face 'link))))

(defvar grep-mode-header-line
  `(,(format "  Tips: %s and %s to navigate; %s to open item; %s to delete item; %s to quit"
             (propertize "UP" 'face 'tooltip)
             (propertize "DOWN" 'face 'tooltip)
             (propertize "ENTER" 'face 'tooltip)
             (propertize "d" 'face 'tooltip)
             (propertize "q" 'face 'tooltip))))

(defun grep-is-valid-item ()
  (save-excursion
    (beginning-of-line)
    (not (or (looking-at ">>>>>")
             (looking-at "<<<<<")
             (looking-at "$")))))

(defun grep-clean-empty-item ()
  (save-excursion
    (goto-char 1)
    (while (re-search-forward "^>>>>>.*\n<<<<<" nil t)
      (goto-char (match-beginning 0))
      (delete-region (line-beginning-position 1)
                     (line-beginning-position 4)))))

;;;###autoload
(defun grep-kill-item-at-point ()
  (interactive)
  (if mark-active
      (let ((end-mark (set-marker (copy-marker (mark-marker) t) (region-end))))
        ;; TODO: Large region slows very much.
        (goto-char (region-beginning))
        (beginning-of-line)
        (setq mark-active nil)
        (while (< (point) (marker-position end-mark))
          (if (grep-is-valid-item)
              (delete-region (line-beginning-position 1)
                             (line-beginning-position 2))
            (forward-line)))
        (set-marker end-mark nil))
    (and (grep-is-valid-item)
         (delete-region (line-beginning-position 1)
                        (line-beginning-position 2))))
  (save-buffer))

;;;###autoload
(defun grep-kill-buffer ()
  (interactive)
  (and (buffer-modified-p)
       (save-buffer))
  (kill-buffer))

;;;###autoload
(defun grep-open-item ()
  (interactive)
  (beginning-of-line)
  (when (looking-at "^\\(.+\\):\\([0-9]+\\):")
    (let ((file (match-string 1))
          (linum (string-to-int (match-string 2))))
      (message "ready to open:%s" file)
      (when (file-exists-p file)
        (find-file file)
        (goto-char 1)
        (forward-line (1- linum))
        (end-of-line)
        (recenter 3)))))

;;;###autoload
(define-derived-mode grep-mode nil "Grep"
  "Major mode for search buffers."
  :group 'grep-group
  (remove-overlays)
  (setq font-lock-defaults grep-mode-font-lock-keywords
        truncate-lines t
        ;; Highlight for specific faces.
        hl-is-highlight-special-faces t
        ;; Header line.
        header-line-format grep-mode-header-line
        ;; Bottom line.
        mode-line-format grep-mode-bottom-line)
  (add-hook 'before-save-hook 'grep-clean-empty-item nil t))

;; Add association when required.
(add-to-list 'auto-mode-alist '("\\.grep\\'" . grep-mode))

(provide 'grep-mode)
