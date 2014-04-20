;; ====================== Common Setting =======================
;; TODO: Move common-setting to a more generic file.

;; ido-mode
(ido-mode t)
(ido-everywhere t)

;; icomplete-mode
;; (icomplete-mode t)

;; enable CUA mode
(cua-mode t)

;; turn on highlight matching brackets
(show-paren-mode 1)

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

;; Ctrl + w, command + w
(global-set-key (kbd "C-w") 'oops-kill-current-buffer)
(if (eq system-type 'darwin)
  (global-set-key (kbd "s-w") 'oops-kill-current-buffer)
  )
;; Ctrl + d
(global-set-key (kbd "C-d") 'oops-duplicate-line)
;; Ctrl + shift + d
(global-set-key (kbd "C-S-d") 'kill-whole-line)

;; Alt + l
(global-set-key (kbd "M-l") 'ido-switch-buffer)
;; Ctrl + l
(global-set-key (kbd "C-l") 'goto-line)

;; Ctrl + /
(global-set-key (kbd "C-/") 'oops-toggle-comment)
;; Alt + /
(global-set-key (kbd "M-/") 'oops-toggle-comment)

;; Ctrl + o
(global-set-key (kbd "C-o") 'oops-goto-symbol)
;; Alt + o
(global-set-key (kbd "M-o") 'oops-goto-symbol)

;; Ctrl + x, Ctrl + b
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; F1
;; F2
(global-set-key (kbd "<f2>") 'highlight-symbol-at-point)
;; F3
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
;; F4
(global-set-key (kbd "<f4>") 'oops-jump-to-definition-atpt)

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

(provide 'oops-settings)
