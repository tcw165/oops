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

(defcustom hl-outward-paren-fg-colors '("black" "black")
  "List of colors for the highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (if (not hl-paren-mode)
                 (set sym val)
               (hl-paren-mode -1)
               (set sym val)
               (hl-paren-mode 1))
             )
           )
         )
  :group 'hl-anything)

(defcustom hl-outward-paren-bg-colors '("cyan" "yellow")
  "List of colors for the background highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (if (not hl-paren-mode)
                 (set sym val)
               (hl-paren-mode -1)
               (set sym val)
               (hl-paren-mode 1))
             )
           )
         )
  :group 'hl-anything)

;; TODO: make choice 1 or nil
(defcustom hl-inward-paren-fg-colors "black"
  "List of colors for the highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type 'color
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (if (not hl-paren-mode)
                 (set sym val)
               (hl-paren-mode -1)
               (set sym val)
               (hl-paren-mode 1))
             )
           )
         )
  :group 'hl-anything)

;; TODO: make choice 1 or nil
(defcustom hl-inward-paren-bg-colors "cyan"
  "List of colors for the background highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type 'color
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (if (not hl-paren-mode)
                 (set sym val)
               (hl-paren-mode -1)
               (set sym val)
               (hl-paren-mode 1))
             )
           )
         )
  :group 'hl-anything)

(defface hl-paren-face nil
  "Face used for highlighting parentheses."
  :group 'hl-anything)

(defvar hl-outward-paren-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl-outward-paren-overlays)

(defvar hl-inward-paren-overlays nil
  "This buffers currently active overlays.")
(make-variable-buffer-local 'hl-inward-paren-overlays)

(defvar hl-paren-last-point 0
  "The last point for which parentheses were highlighted. This is used to prevent analyzing the same context over and over.")
(make-variable-buffer-local 'hl-paren-last-point)

(defun hl-paren-create-overlays ()
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
        (push (make-overlay 0 0) hl-outward-paren-overlays)
        (overlay-put (car hl-outward-paren-overlays) 'face attributes)
        )
      )
    (setq hl-outward-paren-overlays (nreverse hl-outward-paren-overlays))
    )
  ;; inward overlays.
  (let ((fg hl-inward-paren-fg-colors)
        (bg hl-inward-paren-bg-colors)
        attributes)
    (setq attributes (face-attr-construct 'hl-paren-face))
    (setq attributes (plist-put attributes :foreground fg))
    (setq attributes (plist-put attributes :background bg))

    ;; Make pair overlays for every attribute.
    (dotimes (i 2)
      (push (make-overlay 0 0) hl-inward-paren-overlays)
      (overlay-put (car hl-inward-paren-overlays) 'face attributes)
      )
    )
  )

(defun hl-paren-update ()
  "Highlight the parentheses around point."
  (unless (= (point) hl-paren-last-point)
    ;; Outward overlays.
    (setq hl-paren-last-point (point))
    (let ((overlays hl-outward-paren-overlays)
          (pos (point))
          pos1 pos2)
      (save-excursion
        (while (and (setq pos1 (cadr (syntax-ppss pos1)))
                    (cdr overlays))
          (move-overlay (pop overlays) pos1 (1+ pos1))
          (when (setq pos2 (scan-sexps pos1 1))
            (move-overlay (pop overlays) (1- pos2) pos2)
            )
          )
        (goto-char pos)
        )
      ;; Hide unused overlays.
      (dolist (ov overlays)
        (move-overlay ov 1 1)
        )
      )

    ;; Inward overlays.
    (let ((overlays hl-inward-paren-overlays)
          (pos1 (point))
          pos2)
      (save-excursion
        (when (and (looking-back ")"))
          (move-overlay (pop overlays) pos1 (1- pos1))
          (setq pos2 (scan-sexps pos1 -1))
          (move-overlay (pop overlays) pos2 (1+ pos2))
          )
        (when (and (looking-at "("))
          (move-overlay (pop overlays) pos1 (1+ pos1))
          (setq pos2 (scan-sexps pos1 1))
          (move-overlay (pop overlays) pos2 (1- pos2))
          )
        )
      ;; Hide unused overlays.
      (dolist (ov overlays)
        (move-overlay ov 1 1)
        )
      )
    )
    ;; test: (get-char-property (point))
    (list 0 '(1 2) 3 '(4 (5 6)))
  )

;;;###autoload
(define-minor-mode hl-paren-mode
  "Minor mode to highlight the surrounding parentheses."
  :lighter " hl-p"

  (mapc 'delete-overlay hl-outward-paren-overlays)
  (mapc 'delete-overlay hl-inward-paren-overlays)
  (kill-local-variable 'hl-outward-paren-overlays)
  (kill-local-variable 'hl-inward-paren-overlays)
  (kill-local-variable 'hl-paren-last-point)
  (remove-hook 'post-command-hook 'hl-paren-update t)
  (when hl-paren-mode
    (hl-paren-create-overlays)
    (add-hook 'post-command-hook 'hl-paren-update nil t)
    )
  )

(provide 'hl-anything)
