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
;; 2014-05-19 (0.0.2)
;;    Support one inward parentheses highlight.
;;
;; 2014-05-16 (0.0.1)
;;    Initial release, fork from http://nschum.de/src/emacs/highlight-parentheses.

(require 'thingatpt)
(eval-when-compile (require 'cl))

;; Common ======================================================================

(defgroup hl-anything nil
  "Highlight anything."
  :group 'faces
  :group 'matching)

(defun hl--thingatpt ()
  "Return string on which the point is or just string of selection."
  (if mark-active
      ;; return the selection.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; else.
    (thing-at-point 'symbol)
    )
  )

;; Parentheses =================================================================

(defun hl--paren-custom-set (symbol value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (set symbol value)
      (when hl-paren-mode
        (hl-paren-mode -1)
        (hl-paren-mode 1)
        )
      )
    )
  )

(defcustom hl-outward-paren-fg-colors nil
  "List of colors for the highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl--paren-custom-set
  :group 'hl-anything)

(defcustom hl-outward-paren-bg-colors '("cyan" "yellow")
  "List of colors for the background highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl--paren-custom-set
  :group 'hl-anything)

;; TODO: make choice 1 or nil
(defcustom hl-inward-paren-fg-colors nil
  "List of colors for the highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl--paren-custom-set
  :group 'hl-anything)

;; TODO: make choice 1 or nil
(defcustom hl-inward-paren-bg-colors '("hot pink")
  "List of colors for the background highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl--paren-custom-set
  :group 'hl-anything)

(defface hl-paren-face nil
  "Face used for highlighting parentheses."
  :group 'hl-anything)

(defvar hl--outward-paren-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl--outward-paren-overlays)

(defvar hl--inward-paren-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl--inward-paren-overlays)

(defvar hl--paren-last-point 0
  "The last point for which parentheses were highlighted. This is used to prevent analyzing the same context over and over.")
(make-variable-buffer-local 'hl--paren-last-point)

(defun hl--paren-create-overlays ()
  ;; outward overlays.
  (let ((fg hl-outward-paren-fg-colors)
        (bg hl-outward-paren-bg-colors)
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

      ;; Make pair overlays for every attribute.
      (dotimes (i 2)
        (push (make-overlay 0 0) hl--outward-paren-overlays)
        (overlay-put (car hl--outward-paren-overlays) 'face attributes)
        )
      )
    (setq hl--outward-paren-overlays (nreverse hl--outward-paren-overlays))
    )
  ;; inward overlays.
  (let ((fg hl-inward-paren-fg-colors)
        (bg hl-inward-paren-bg-colors)
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
      )

    ;; Make pair overlays for every attribute.
    (dotimes (i 2)
      (push (make-overlay 0 0) hl--inward-paren-overlays)
      (overlay-put (car hl--inward-paren-overlays) 'face attributes)
      )
    )
  )

(defun hl--paren-update ()
  "Highlight the parentheses around point."
  (unless (= (point) hl--paren-last-point)
    ;; Outward overlays.
    (setq hl--paren-last-point (point))
    (let ((overlays hl--outward-paren-overlays)
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
        )
      ;; Hide unused overlays.
      (dolist (ov overlays)
        (move-overlay ov 1 1)
        )
      )

    ;; Inward overlays.
    (let ((overlays hl--inward-paren-overlays)
          (pos1 (point))
          pos2)
      (save-excursion
        (condition-case err
            (cond
             ((and (or (looking-back ")")
                       (looking-back "]"))
                   ;; TODO: skip comment.
                   )
              (move-overlay (pop overlays) pos1 (1- pos1))
              (setq pos2 (scan-sexps pos1 -1))
              (move-overlay (pop overlays) pos2 (1+ pos2))
              )
             ((and (or (looking-at "(")
                       (looking-at "["))
                   ;; TODO: skip comment.
                   )
              (move-overlay (pop overlays) pos1 (1+ pos1))
              (setq pos2 (scan-sexps pos1 1))
              (move-overlay (pop overlays) pos2 (1- pos2))
              )
             )
          (error nil)
          )
        )
      ;; Hide unused overlays.
      (dolist (ov overlays)
        (move-overlay ov 1 1)
        )
      )
    )
  )

;;;###autoload
(define-minor-mode hl-paren-mode
  "Minor mode to highlight the surrounding parentheses."
  :lighter " hl-p"

  (mapc 'delete-overlay hl--outward-paren-overlays)
  (mapc 'delete-overlay hl--inward-paren-overlays)
  (kill-local-variable 'hl--outward-paren-overlays)
  (kill-local-variable 'hl--inward-paren-overlays)
  (kill-local-variable 'hl--paren-last-point)
  (remove-hook 'post-command-hook 'hl--paren-update t)
  (when hl-paren-mode
    (hl--paren-create-overlays)
    (add-hook 'post-command-hook 'hl--paren-update nil t)
    )
  )

;; Symbol or Selection =========================================================
;; TODO:
;; * Use font-lock-mode to make it.
;; * Handle the keywords under overlay, make temporary overlays to make them highlighted.
;; (font-lock-add-keywords 'emacs-lisp-mode "test")

(defun test ()
  (interactive)
  (unless (search-forward "hl-anything" nil t 1)
    (message "EOF")
    )
  )

(defface hl-thing-face nil
  "Face used for highlighting thing (a symbol or a text selection)."
  :group 'hl-anything)

(defvar hl--things-list nil
  "A list storing things \(text string\) to be highlighted.")

(defvar hl--things-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl--things-overlays)


(defun hl--thing-create-overlays ()
  )

(defun hl--thing-p (thing)
  "Test if the thing is currently highlighted."
  (member thing hl--things-list)
  )

(defun hl--thing-add (thing)
  (push thing hl--things-list)
  )

(defun hl--thing-remove (thing)
  nil
  )

;;;###autoload
(defun hl-thing-at-point ()
  "Toggle highlighting of the thing at point."
  (interactive)
  (let ((thing (hl--thingatpt)))
    (when thing
      (if (hl--thing-p thing)
          (hl--thing-remove thing)
        (hl--thing-add thing)
        )
      )
    )
  )

(provide 'hl-anything)
