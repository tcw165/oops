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
;; Add the following to your .emacs file:
;; (require 'hl-anything)
;;
;;; Change Log:
;;
;; 2014-05-16 (0.0.1)
;;    Initial release, fork from http://nschum.de/src/emacs/highlight-parentheses.

(eval-when-compile (require 'cl))

(defgroup hl-anything nil
  "Highlight anything."
  :group 'faces
  :group 'matching)

;; Parentheses =================================================================

(defun hl-paren-set (variable value)
  (set variable value)
  (when (fboundp 'hl-paren-color-update)
    (hl-paren-color-update)
    )
  )

(defcustom hl-paren-foreground-colors '("black")
  "List of colors for the highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :set 'hl-paren-set
  :group 'hl-anything)

(defcustom hl-paren-background-colors '("cyan" "wheat1")
  "List of colors for the background highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :set 'hl-paren-set
  :group 'hl-anything)

;; (defcustom hl-paren-inner-foreground-colors '("black")
;;   "List of colors for the highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
;;   :type '(repeat color)
;;   :set 'hl-paren-set
;;   :group 'hl-anything)

;; (defcustom hl-paren-inner-background-colors '("green")
;;   "List of colors for the background highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
;;   :type '(repeat color)
;;   :set 'hl-paren-set
;;   :group 'hl-anything)

(defface hl-paren-face nil
  "Face used for highlighting parentheses. Color attributes might be overriden by `hl-paren-foreground-colors' and
`hl-paren-background-colors'."
  :group 'hl-anything)

(defvar hl-paren-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl-paren-overlays)

(defvar hl-paren-last-point 0
  "The last point for which parentheses were highlighted. This is used to prevent analyzing the same context over and over.")
(make-variable-buffer-local 'hl-paren-last-point)

(defun hl-paren-create-overlays ()
  (let ((fg hl-paren-foreground-colors)
        (bg hl-paren-background-colors)
        attributes)
    (while (or fg bg)
      (setq attributes (face-attr-construct 'hl-paren-face))
      (when (car fg)
        (setq attributes (plist-put attributes :foreground (car fg)))
        )
      (pop fg)
      (when (car bg)
        (setq attributes (plist-put attributes :background (car bg)))
        )
      (pop bg)
      (dotimes (i 2) ;; front and back
        (push (make-overlay 0 0) hl-paren-overlays)
        (overlay-put (car hl-paren-overlays) 'face attributes)
        )
      )
    (setq hl-paren-overlays (nreverse hl-paren-overlays))
    )
  )

(defun hl-paren-highlight ()
  "Highlight the parentheses around point."
  (unless (= (point) hl-paren-last-point)
    (setq hl-paren-last-point (point))
    (let ((overlays hl-paren-overlays)
          (pos (point))
          pos1 pos2)
      (save-excursion
        (condition-case err
            (while (and (setq pos1 (cadr (syntax-ppss pos1)))
                        (cdr overlays))
              (move-overlay (pop overlays) pos1 (1+ pos1))
              (when (setq pos2 (scan-sexps pos1 1))
                (move-overlay (pop overlays) (1- pos2) pos2)
                )
              )
          (error nil)
          )
        (goto-char pos)
        )
      (dolist (ov overlays)
        (move-overlay ov 1 1)
        )
      )
    )
  )

(defun hl-paren-color-update ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when hl-paren-overlays
        (mapc 'delete-overlay hl-paren-overlays)
        (setq hl-paren-overlays nil)
        (hl-paren-create-overlays)
        (let ((hl-paren-last-point -1)) ;; force update
          (hl-paren-highlight)
          )
        )
      )
    )
  )

;;;###autoload
(define-minor-mode hl-paren-mode
  "Minor mode to highlight the surrounding parentheses."
  nil " hl-p" nil
  (mapc 'delete-overlay hl-paren-overlays)
  (kill-local-variable 'hl-paren-overlays)
  (kill-local-variable 'hl-paren-last-point)
  (remove-hook 'post-command-hook 'hl-paren-highlight t)
  (when hl-paren-mode
    (hl-paren-create-overlays)
    (add-hook 'post-command-hook 'hl-paren-highlight nil t)
    )
  )

(provide 'hl-anything)
