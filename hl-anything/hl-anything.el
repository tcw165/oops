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
;; Enable parenethese highlighting.
;; (hl-paren-mode 1)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-05-25 (0.0.4)
;;    Support searching thing. The thing might be a symbol text or a selection text.
;;
;; 2014-05-20 (0.0.3)
;;    Support one inward parentheses highlight.
;;
;; 2014-05-19 (0.0.2)
;;    Support multiple outward parentheses highlight.
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

(defcustom hl-inward-paren-fg-colors nil
  "List of colors for the highlighted parentheses. The list starts with the the inside parentheses and moves outwards."
  :type '(repeat color)
  :initialize 'custom-initialize-default
  :set 'hl--paren-custom-set
  :group 'hl-anything)

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

(defun hl--thing-custom-set (symbol value)
  (set symbol value)
  )

(defface hl-thing-face nil
  "Face used for highlighting thing (a symbol or a text selection)."
  :group 'hl-anything)

(defcustom hl-thing-fg-colors nil
  "The foreground colors for `hl-thing-at-point'."
  :type '(repeat color)
  :group 'hl-anything)

(defcustom hl-thing-bg-colors '("yellow"
                                "cyan"
                                "SpringGreen1"
                                "moccasin"
                                "violet")
  "The background colors for `hl-thing-at-point'."
  :type '(repeat color)
  :group 'hl-anything)

(defvar hl--things-color-index 0)

(defvar hl-thing-before-find-hook nil
  "Hook for doing something before `hl--thing-find' do the searching.
This hook has one argument, (REGEXP_STRING BEG END).
Maybe you'll need it for history and navigation feature.")

(defvar hl-thing-after-find-hook nil
  "Hook for doing something after `hl--thing-find' do the searching.
This hook has one argument, (REGEXP_STRING BEG END).
Maybe you'll need it for history and navigation feature.")

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

(defun hl--thingatpt ()
  "Return a list, (REGEXP_STRING BEG END), on which the point is or just string of selection."
  (if mark-active
      ;; return the selection.
      (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
        (list (regexp-quote str)
              (region-beginning)
              (region-end))
        )
    ;; else.
    ;; TODO: Use the highlight if point is on it.
    (let ((bound (bounds-of-thing-at-point 'symbol))
          thing)
      (when bound
        (setq thing (buffer-substring-no-properties (car bound) (cdr bound)))
        ;; (format "\\<%s\\>" (regexp-quote thing))
        (list (regexp-quote thing)
              (car bound)
              (cdr bound))
        )
      )
    )
  )

(defun hl--thing-add (thing)
  ;; TODO: get understand what it is exactly doing.
  (let ((fg-color (nth hl--things-color-index hl-thing-fg-colors))
        (bg-color (nth hl--things-color-index hl-thing-bg-colors))
        color)
    (if (and (null fg-color)
             (null bg-color))
        (setq hl--things-color-index 1)
      (setq hl--things-color-index (1+ hl--things-color-index))
      )
    (and (null fg-color)
         ;; (setq fg-color (car hl-thing-fg-colors))
         ;; TODO: if no fg-color, use default.
         (setq fg-color "red")
         )
    (and (null bg-color)
         (setq bg-color (car hl-thing-bg-colors))
         )
    ;; TODO: if no fg-color, use default.
    (setq color `((foreground-color . ,fg-color)
                  (background-color . ,bg-color)))
    ;; highlight
    (font-lock-add-keywords nil `((,thing 0 ',color prepend)) 'append)
    (font-lock-fontify-buffer)
    (push thing hl--things-list)
    )
  (message "[hl-anything] highlight thing, \"%s\"." thing)
  )

(defun hl--thing-remove (thing)
  (setq hl--things-list (delete thing hl--things-list))
  (let ((keyword (assoc thing (if (eq t (car font-lock-keywords))
                                    (cadr font-lock-keywords)
                                  font-lock-keywords
                                  ))))
    (font-lock-remove-keywords nil (list keyword))
    (font-lock-fontify-buffer)
    )
  (message "[hl-anything] unhighlight thing, \"%s\"." thing)
  )

(defun hl--thing-find (step)
  (let* ((case-fold-search t)
         (thing (hl--thingatpt))
         (str (nth 0 thing))
         beg
         end)
    (when (and thing
               (not (= step 0)))
      ;; Hook before searching.
      (run-hook-with-args hl-thing-before-find-hook thing)
      ;; Disable selection.
      (setq mark-active nil)
      ;; Regexp search.
      (goto-char (or (and (> step 0)
                          ;; Move to end.
                          (nth 2 thing))
                     (and (< step 0)
                          ;; Move to beginning.
                          (nth 1 thing))))
      (re-search-forward str nil t step)
      ;; Make a selection.
      (save-excursion
        (when (> step 0)
          (setq end (point))
          (re-search-backward str nil t 1)
          (setq beg (point))
          )
        (when (< step 0)
          (setq beg (point))
          (re-search-forward str nil t 1)
          (setq end (point))
          )
        )
      (set-marker (mark-marker) beg)
      (goto-char end)
      (setq mark-active t)
      (message "[hl-anything] find \"%s\" %s."
               str
               (if (> step 0) "forwardly" "backwardly"))
      ;; Hook after searching.
      (run-hook-with-args hl-thing-after-find-hook (list str beg end))
      )
    )
  )

;;;###autoload
(defun hl-thing-at-point ()
  "Toggle highlighting of the thing at point."
  (interactive)
  (let* ((thing (hl--thingatpt))
         (str (car thing)))
    (when thing
      (if (hl--thing-p str)
          (hl--thing-remove str)
        (hl--thing-add str)
        )
      )
    )
  )

;;;###autoload
(defun hl-thing-remove-all ()
  "Remove all the highlights in buffer."
  (interactive)
  (mapc 'hl--thing-remove hl--things-list)
  (message "[hl-anything] Discard all the highlights.")
  )

;;;###autoload
(defun hl-thing-find-forward ()
  "Find thing forwardly and jump to it."
  (interactive)
  (hl--thing-find 1)
  )

;;;###autoload
(defun hl-thing-find-backward ()
  "Find thing backwardly and jump to it."
  (interactive)
  (hl--thing-find -1)
  )

;; TODO: Save highlights of last session.
;;;###autoload
(define-minor-mode hl-thing-mode ()
  "This mode supports following features:
1. Remember highlights of last session."
  lighter: " hl-t"
  )

(provide 'hl-anything)
