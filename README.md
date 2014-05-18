## Usage
Add following script in your .emacs file.
```
(add-to-list 'load-path "~/.emacs.d/oops")
(require 'oops-mode)

;; Init all the features of `oops-mode'
(oops-mode 1)
```

This is my .emacs setting.
```
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(global-mark-ring-max 128)
 '(hl-paren-foreground-colors (quote ("black")))
 '(hl-paren-background-colors (quote ("cyan" "wheat1")))
 '(show-paren-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/oops")
(require 'oops-mode)

;; Init all the features of `oops-mode'
(oops-mode 1)
```

## TODO List
* Merge `hl-param` and `hl-symb` into it.
* Design a multiple help window framework in `oops-win-mode.el`.
* Create a multiple help buffer framework in `oops-help-buffer.el`.
* Use `helm` to implement multiple help mechanism.
* Try to use hook mechanism to make strategy pattern (refer `to company`).
* Is this feature a mode or a framework? Maybe use `(oops-framework 1)` is better.
* Add copyright.
* Improve history mechanism since there's already a MARKER-INSERTION-TYPE.
