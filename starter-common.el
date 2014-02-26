;; Author:       Boy Wang
;; Last-Updated: xxx
;; URL:          xxx
;;
;;

(add-to-list 'load-path "~/.emacs.d/Starter/3rd-party")

(require 'highlight-symbol)

;; ====================== Common Feature =======================

;; Get current buffer and kill it.
(defun starter-kill-current-buffer ()
  ""
  (interactive)
  (kill-buffer (current-buffer))
  )

;; Find tag with symbol under cursor.
(defun starter-find-definition ()
  (interactive)
  ""
  ;; todo: thingatpt
  (message "smart find tag")
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
        (decf n)))))

;; ====================== Common Setting =======================
;; TODO: Move common-setting to a more generic file.

;; enable CUA mode
(cua-mode t)

;; turn on highlight matching brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

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
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; highlight function, require 'highlight-symbol
(highlight-symbol-mode t)
(setq highlight-symbol-idle-delay 0)

;; ======================== KEY BINDING ========================
;; TODO: Move key-binding to a more generic file.

;; ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ctrl + w, command + w
(global-set-key (kbd "C-w") 'starter-kill-current-buffer)
(if (eq system-type 'darwin)
  (global-set-key (kbd "s-w") 'starter-kill-current-buffer))

;; ctrl + d
(global-set-key (kbd "C-d") 'starter-duplicate-line)
;; ctrl + shift + d
(global-set-key (kbd "C-S-d") 'kill-whole-line)

;; F1
;; F2
(global-set-key (kbd "<f2>") 'highlight-symbol-at-point)
;; F3
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
;; F4
(global-set-key (kbd "<f4>") 'semantic-ia-fast-jump)

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
