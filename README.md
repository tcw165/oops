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
 '(highlight-symbol-colors (quote ("yellow" "cyan" "SpringGreen1" "moccasin" "violet")))
 '(highlight-symbol-idle-delay 0.125)
 '(highlight-symbol-on-navigation-p t)
 '(show-paren-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Indicate which definition of function or class you are inside. (global mode)
(which-function-mode 1)

(add-to-list 'load-path "~/.emacs.d/oops")
(require 'oops-mode)

;; Init all the features of `oops-mode'
(oops-mode 1)
```

## TODO List
* ~~Merge `hl-param` into my repo, `hl-anything`.~~
* Merge `hl-symb` into `hl-anything` and release beta version of `hl-anything`.
* Refer to `helm-projectile` to implement project management and workspace concept.
* Design a multiple help window framework in `oops-win-mode.el`.
* Create a multiple help buffer framework in `oops-help-buffer.el`.
* Use `helm` to implement multiple help mechanism.
* Try to use hook mechanism to make strategy pattern (refer `to company`).
* Is this feature a mode or a framework? Maybe use `(oops-framework 1)` is better.
* Add copyright.
* Enhance history mechanism since there's already a MARKER-INSERTION-TYPE.
* Enhance `hl-anything`, make it flexibly support more languages.
