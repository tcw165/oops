;; Author:       Boy Wang
;; Last-Updated: xxx
;; URL:          xxx
;;
;;

(add-to-list 'load-path "~/.emacs.d/starter/3rd-party")

(require 'highlight-symbol)
(require 'highlight-parentheses)
(require 'auto-complete)

;; ====================== Common Feature =======================

(defun starter-thing-at-point-or-selection ()
  "Return string according to syntax-table, \"_w\", to get the
symbol string in which the point is.
Or just return the text selection."
  (if mark-active
      ;; if there is already a selection, use the selection.
      (let (
            (text (buffer-substring-no-properties (region-beginning) (region-end)))
            )
        (and (stringp text) text) ;; return string.
        )
    ;; else.
    (save-excursion
      (let* (
            (tmp (skip-syntax-backward "_w"))
            (a (point))
            (tmp (skip-syntax-forward "_w"))
            (b (point))
            (text (buffer-substring-no-properties a b))
            )
        (and (stringp text) text) ;; return string.
        )
      )
    )
  )

;; Find tag with symbol under cursor.
(defun starter-find-definition ()
  "Find the symbol definition both for function and variable.
test: find-function"
  (interactive)
  (cond
   ;; Clause: major-mode == "emacs-lisp-mode"
   (
    (or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'lisp-interaction-mode)
        )
    (let (
          (symb (read (starter-thing-at-point-or-selection)))
          )
      (cond
       ;; function
       (
        (fboundp symb)
        (find-function symb)
        (message "function: %s" (symbol-name symb))
        )
       ;; variable
       (
        (boundp symb)
        (find-variable symb)
        (message "variable: %s" (symbol-name symb))
        )
       )
      )
    )
   ;; Clause: major-mode == ...
   (
    (eq major-mode 'c-mode)
    (message "not implement yet...")
    )
   )
  )

;;
(defun starter-push-mark (mark &optional index)
  ""
  )

;;
(defun starter-mark-iterator ()
  ""
  )

;;
(defun starter-previous-mark ()
  ""
  (interactive)
  )

;;
(defun starter-next-mark ()
  ""
  (interactive)
  )

;; Get current buffer and kill it.
(defun starter-kill-current-buffer ()
  ""
  (interactive)
  (kill-buffer (current-buffer))
  )

;; duplicate current line
(defun starter-duplicate-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
          (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
        (insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
        (insert current-line)
        (decf n))))
  )

;; ====================== Common Setting =======================
;; TODO: Move common-setting to a more generic file.

;; enable CUA mode
(cua-mode t)

;; turn on highlight matching brackets
;;(show-paren-mode 1)
;;(setq show-paren-style 'parenthesis)

;; insert brackets by pairs
(electric-pair-mode 1)

;; highlight current line
(global-hl-line-mode 1)

;; cursor display as a bar
(setq-default cursor-type 'bar)

;; tab width
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; turn off backup file
(setq make-backup-files nil)

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; highlight function, require 'highlight-symbol
(highlight-symbol-mode t)
(setq highlight-symbol-idle-delay 0)

;; highlight parentheses function, require 'highlight-parentheses
;; only for:
;; emacs-lisp-mode-hook
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (highlight-parentheses-mode)))

;; auto-complete function, require 'auto-complete-config
;; only for:
;; emacs-lisp-mode-hook
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (auto-complete-mode)))

;; test for "semantic + ede""
;;(semantic-mode t)
;;(global-ede-mode t)

;; ======================== KEY BINDING ========================
;; TODO: Move key-binding to a more generic file.

;; ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ctrl + w, command + w
(global-set-key (kbd "C-w") 'starter-kill-current-buffer)
(if (eq system-type 'darwin)
  (global-set-key (kbd "s-w") 'starter-kill-current-buffer)
  )
;; ctrl + d
(global-set-key (kbd "C-d") 'starter-duplicate-line)
;; ctrl + shift + d
(global-set-key (kbd "C-S-d") 'kill-whole-line)

;; Alt + l
(global-set-key (kbd "M-l") 'bs-show)
;; Ctrl + l
(global-set-key (kbd "C-l") 'goto-line)

;; f1
;; F2
(global-set-key (kbd "<f2>") 'highlight-symbol-at-point)
;; F3
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
;; F4
(global-set-key (kbd "<f4>") 'starter-find-definition)
;; F5
(global-set-key (kbd "<f5>") 'find-function)

;; Shift + F1
;; Shift + F2
(global-set-key (kbd "S-<f2>") 'highlight-symbol-remove-all)
;; Shift + F3
(global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
;; Shift + F4

;; HOME
(global-set-key (kbd "<home>") 'move-beginning-of-line)
;; END
(global-set-key (kbd "<end>") 'move-end-of-line)

;; ------------------------ End Of File ------------------------
(provide 'starter-common)
