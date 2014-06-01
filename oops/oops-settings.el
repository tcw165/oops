;; ====================== Common Setting =======================
;; TODO: Move common-setting to a more generic file.

;; ido-mode
(ido-mode 1)

;; insert brackets by pairs
(electric-pair-mode 1)

;; tab width
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; turn off backup file
(setq make-backup-files nil)

;; scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; highlight function, require 'highlight-symbol
(highlight-symbol-mode t)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

;; test for "semantic + ede""
;;(semantic-mode 1)
;;(global-ede-mode 1)

;; ======================== KEY BINDING ========================
;; TODO: Move key-binding to a more generic file.

(when (eq system-type 'darwin)
  (global-set-key (kbd "s-w") 'oops-kill-current-buffer)
  (global-set-key (kbd "s-z") 'oops-undo)
  )

;; ESC
(global-set-key (kbd "<escape>") 'oops-common-escape)
;; Ctrl + ESC
(global-set-key (kbd "C-x <escape>") 'delete-other-windows)

;; Ctrl + w, command + w
(global-set-key (kbd "C-w") 'oops-kill-current-buffer-or-window)
;; Ctrl + d
(global-set-key (kbd "C-d") 'oops-duplicate-lines)
;; Ctrl + shift + d
(global-set-key (kbd "C-S-d") 'oops-kill-lines)
;; Ctrl + z
(global-set-key (kbd "C-z") 'oops-undo)

;; Ctrl + l
(global-set-key (kbd "C-l") 'goto-line)
;; Ctrl + Alt + l
(global-set-key (kbd "C-M-l") 'ido-switch-buffer)

;; Ctrl + /
(global-set-key (kbd "C-/") 'oops-toggle-comment)
;; Alt + /
(global-set-key (kbd "M-/") 'oops-toggle-comment)

;; Ctrl + up
(global-set-key (kbd "C-<up>") 'oops-move-lines-up)
;; Ctrl + down
(global-set-key (kbd "C-<down>") 'oops-move-lines-down)
;; Alt + [
(global-set-key (kbd "M-[") 'his-prev-history)
;; Alt + ]
(global-set-key (kbd "M-]") 'his-next-history)

;; Ctrl + o
(global-set-key (kbd "C-o") 'oops-goto-local-symbol)
;; Ctrl + Shift + o
(global-set-key (kbd "C-S-o") 'oops-goto-global-symbol)

;; Ctrl + x, Ctrl + b
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; Alt + left/right/up/down
(global-set-key (kbd "M-<left>") 'oops-windmove-left)
(global-set-key (kbd "M-<right>") 'oops-windmove-right)
(global-set-key (kbd "M-<up>") 'oops-windmove-up)
(global-set-key (kbd "M-<down>") 'oops-windmove-down)

;; TAB
(global-set-key "\t" 'oops-indent-or-company)

;; F1
;; F2
(global-set-key (kbd "<f2>") 'hl-thing-at-point)
;; F3
(global-set-key (kbd "<f3>") 'hl-thing-find-forward)
;; F4
(global-set-key (kbd "<f4>") 'oops-jump-to-definition-atpt)

;; Shift + F1
;; Shift + F2
(global-set-key (kbd "S-<f2>") 'hl-thing-remove-all)
;; Shift + F3
(global-set-key (kbd "S-<f3>") 'hl-thing-find-backward)
;; Shift + F4

;; HOME
(global-set-key (kbd "<home>") 'move-beginning-of-line)
;; END
(global-set-key (kbd "<end>") 'move-end-of-line)

(provide 'oops-settings)
