(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(enable-local-eval nil)
 '(enable-local-variables nil)
 '(hl-line-sticky-flag nil)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(normal-erase-is-backspace t)
 '(parse-sexp-ignore-comments t)
 '(scroll-step 1)
 '(show-paren-mode nil)
 '(standard-indent 2)
 '(vc-handled-backends nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(link ((t (:foreground "blue" :underline t :weight bold))))
 '(tooltip ((t (:background "cornsilk" :foreground "Black" :weight bold :family "Monaco")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Binding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ESC
(global-set-key (kbd "<escape>") 'oops-common-escape)
;; Ctrl + ESC
(global-set-key (kbd "C-x <escape>") 'delete-other-windows)

;; Ctrl + x Ctrl + f
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Ctrl + w, command + w
(global-set-key (kbd "C-w") 'oops-kill-buffer-or-window-or-frame)
(global-set-key (kbd "s-w") 'oops-kill-buffer-or-window-or-frame)
;; Ctrl + d
(global-set-key (kbd "C-d") 'oops-duplicate-lines)
;; Ctrl + shift + d
(global-set-key (kbd "C-S-d") 'oops-kill-lines)
;; Ctrl + z
(global-set-key (kbd "C-z") 'oops-undo)
;; Ctrl + f
(global-set-key (kbd "C-f") 'helm-occur)
;; Ctrl + c f
(global-set-key (kbd "C-c f") 'helm-google-suggest)
;; Ctrl + l
(global-set-key (kbd "C-l") 'goto-line)
;; Ctrl + b
(global-set-key (kbd "C-b") 'helm-buffers-list)

;; Alt + x
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; Ctrl + /
(global-set-key (kbd "C-/") 'oops-toggle-comment)
;; Alt + /
(global-set-key (kbd "M-/") 'oops-toggle-comment)

;; Ctrl + up
(global-set-key (kbd "C-<up>") 'smart-shift-up)
;; Ctrl + down
(global-set-key (kbd "C-<down>") 'smart-shift-down)
;; Alt + [
(global-set-key (kbd "M-[") 'his-prev-history)
;; Alt + ]
(global-set-key (kbd "M-]") 'his-next-history)

;; Ctrl + o
(global-set-key (kbd "C-o") 'helm-imenu)
(global-set-key (kbd "C-S-o") 'ws-search-symbol)
;; Ctrl + r
(global-set-key (kbd "C-r") 'prj-find-file)

;; Alt + left/right/up/down
(global-set-key (kbd "M-<left>") 'oops-windmove-left)
(global-set-key (kbd "M-<right>") 'oops-windmove-right)
(global-set-key (kbd "M-<up>") 'oops-windmove-up)
(global-set-key (kbd "M-<down>") 'oops-windmove-down)

;; TAB
(global-set-key "\t" 'oops-indent-or-company)

;; F1
(global-set-key (kbd "S-<f1>") 'prj-search-project)
(global-set-key (kbd "M-<f1>") 'prj-toggle-search-buffer)
;; F2
(global-set-key (kbd "<f2>") 'hl-highlight-thingatpt-local)
;; F3
(global-set-key (kbd "<f3>") 'hl-find-thing-forwardly)
;; F4
(global-set-key (kbd "<f4>") 'ws-goto-symbol)

;; Shift + F1
;; Shift + F2
(global-set-key (kbd "S-<f2>") 'hl-unhighlight-all-local)
;; Shift + F3
(global-set-key (kbd "S-<f3>") 'hl-find-thing-backwardly)
;; Shift + F4

;; HOME
(global-set-key (kbd "<home>") 'move-beginning-of-line)
;; END
(global-set-key (kbd "<end>") 'move-end-of-line)
;; DELETE
(global-set-key (kbd "s-<backspace>") 'delete-forward-char)
(global-set-key (kbd "C-s-<backspace>") 'kill-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Oops Extension ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/oops")
(require 'oops)

