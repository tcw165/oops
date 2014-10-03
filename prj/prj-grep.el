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

(require 'font-lock)

(require 'hl-anything)

(defcustom prj-grep-mode-hook `(linum-mode
                                hl-line-mode
                                font-lock-mode
                                hl-highlight-mode)
  "Hook run when entering `prj-grep-mode' mode."
  :type 'hook
  :group 'prj-group)

(defvar prj-grep-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (if (featurep 'sos)
        (progn
          (define-key map [return] '(lambda ()
                                      (interactive)
                                      (beginning-of-line)
                                      (sos-goto-definition))))
      )
    ;; (define-key map [up] )
    ;; (define-key map [down] )
    (define-key map [?q] '(lambda ()
                            (interactive)
                            (prj-kill-grep-buffer)))
    (define-key map [?d] '(lambda () (interactive) (prj-kill-grep-item-at-point)))
    map))

(defvar prj-grep-mode-font-lock-keywords
  '((("^\\([[:alnum:] $_\/.+-]+\\):\\([0-9]+\\):.*$" (1 'hl-file-face) (2 'hl-number-face))
     ("^\\(>>>>>\\s-\\)\\(.+\\)$" (1 'hl-title-3-face) (2 'hl-symbol-face))
     ("^\\(<<<<<\\)$" (1 'hl-title-3-face)))
    ;; don't use syntactic fontification.
    t
    ;; Case insensitive.
    nil))

(defvar prj-grep-mode-header-line
  `(,(format "  Tips: %s and %s to navigate; %s to open item; %s to delete item; %s to quit"
             (propertize "UP" 'face 'tooltip)
             (propertize "DOWN" 'face 'tooltip)
             (propertize "ENTER" 'face 'tooltip)
             (propertize "d" 'face 'tooltip)
             (propertize "q" 'face 'tooltip))))

(defun prj-kill-grep-item-at-point ()
  (setq buffer-read-only nil)
  (dolist (bound (prj-grep-mode-get-kill-regions))
    (delete-region (car bound) (cdr bound)))
  (and (buffer-modified-p)
       (save-buffer))
  (setq buffer-read-only t))

(defun prj-grep-mode-get-kill-regions ()
  (if mark-active
      (let ((beg (region-beginning))
            (end (region-end))
            reg-beg reg-end regions)
        (if (= (buffer-size) (- end beg))
            (setq regions `((1 . ,(point-max))))
          ;; TODO: Large region slows very much.
          (save-excursion
            (goto-char beg)
            (while (<= (point) end)
              (if (prj-is-valid-grep-item)
                  (progn
                    (unless reg-beg
                      (setq reg-beg (line-beginning-position 1)))
                    (setq reg-end (line-beginning-position 2)))
                (when (and reg-beg reg-end)
                  (setq regions (append regions `((,reg-beg . ,reg-end)))
                        reg-beg nil
                        reg-end nil)))
              (forward-line))
            (when (and reg-beg reg-end)
              (setq regions (append regions `((,reg-beg . ,reg-end)))))))
        regions)
    (when (prj-is-valid-grep-item)
      ;; TODO: what if there's nothing between ">>>>>" and "<<<<<"?
      `((,(line-beginning-position 1) . ,(line-beginning-position 2))))))

(defun prj-is-valid-grep-item ()
  (save-excursion
    (beginning-of-line)
    (not (or (looking-at ">>>>>")
             (looking-at "<<<<<")
             (looking-at "$")))))

(defun prj-kill-grep-buffer ()
  (when (buffer-modified-p)
    (save-buffer))
  (kill-buffer))

;;;###autoload
(define-derived-mode prj-grep-mode nil "prj:grep"
  "Major mode for search buffers."
  :group 'prj-group
  (remove-overlays)
  (setq font-lock-defaults prj-grep-mode-font-lock-keywords
        truncate-lines t
        ;; Highlight for specific faces.
        hl-is-highlight-special-faces t
        ;; Header line.
        header-line-format prj-grep-mode-header-line))

(provide 'prj-grep)
