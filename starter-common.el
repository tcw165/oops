;; Author:       Boy Wang
;; Last-Updated: xxx
;; URL:          xxx
;;
;;

(add-to-list 'load-path "~/.emacs.d/Starter/3rd-party")

(require 'thingatpt+)

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

;; ======================== KEY BINDING ========================

;; ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ctrl + w, command + w
(global-set-key (kbd "C-w") 'starter-kill-current-buffer)
(if (eq system-type 'darwin)
  (global-set-key (kbd "s-w") 'starter-kill-current-buffer))

;; F1
;; F3
;; F4
(global-set-key (kbd "<f4>") 'starter-find-definition)

;; Shift + F1
;; Shift + F3
;; Shift + F4

;; ------------------------ End Of File ------------------------
(provide 'starter-common)
