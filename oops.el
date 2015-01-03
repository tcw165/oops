;; Copyright (C) 2015
;;
;; Author: boyw165
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-06-07 (0.0.1)
;;    Initial release.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization for built-in features ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Packages.
(require 'package)
(dolist (arch '(("melpa-stable" . "https://stable.melpa.org/packages/")
                ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                ))
  (add-to-list 'package-archives arch t))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook (quote (copyright-update delete-trailing-whitespace)))
 '(c-basic-offset 2)
 '(cua-mode t nil (cua-base))
 '(css-indent-offset 2)
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(hl-line-sticky-flag nil)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mode-line-format '("-- L%l:C%c:S[%*] - "
                      (:eval (propertize (oops-simple-path 1 7)
                                         'face 'mode-line-buffer-id))
                      " - "
                      mode-line-misc-info
                      "%-"
                      ))
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(recentf-exclude (list "recentf" ".*ignore"))
 '(recentf-max-menu-items 64)
 '(recentf-mode t)
 ;; '(normal-erase-is-backspace t)
 ;; '(parse-sexp-ignore-comments t)
 '(save-place t)
 '(save-place-limit 128)
 '(scroll-step 1)
 '(show-paren-mode nil)
 '(standard-indent 2)
 '(speedbar-default-position 'left)
 '(speedbar-use-images nil)
 '(tab-width 2)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1C1C1C" :foreground "#FFFFFF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))

;; Make default prompt query using y-or-n.
(defalias 'yes-or-no-p 'y-or-n-p)
;; Enable `which-function-mode'.
(which-function-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Oops Things ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)

;; Themes
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (concat (file-name-directory load-file-name)
                       "themes"))
  (load-theme 'zenburn t))

(defun oops-simple-path (lmax rmax)
  "Generates simplified path from file path"
  (let ((truepath (or buffer-file-truename
                      whereis-file-truename)))
    (if truepath
        (let ((path (split-string truepath "/" t))
              (result ""))
          (if (<= (length path) (+ lmax rmax))
              truepath
            (let ((start-point (substring truepath 0 1))
                  (lcount 0)
                  (rcount 0))
              (while (and path (< lcount lmax))
                (setq result (concat
                              (if (string= (car path) "~")
                                  ""
                                "/")
                              (car path))
                      lcount (1+ lcount)
                      path (cdr path)))
              (setq path (last path rmax)
                    result (concat result "/..."))
              (while (and path (< rcount rmax))
                (setq result (concat result "/" (car path))
                      rcount (1+ rcount)
                      path (cdr path)))
              (when whereis-file-truename
                (setq result (format "%s [%s]" (buffer-name) result)))
              result)))
      (buffer-name))))

(defun oops-shift-lines (step)
  "Move the current line or region to STEP lines forwardly. Negative value of
STEP means move backwardly. Notice: It won't modify `kill-ring'."
  (and (not (integerp step))
       (error "oops-shift's argument STEP should be an integer! step = %s"
              step))
  ;; There're two situation:
  ;;
  ;; (point) ---------------
  ;; ---------------- (mark)
  ;;
  ;; or
  ;;
  ;; (mark) ----------------
  ;; --------------- (point)
  ;;
  ;; So here are the point-excursion and mark-excursion.
  (let* ((beg (if (use-region-p)
                  (save-excursion
                    (goto-char (region-beginning))
                    (line-beginning-position 1))
                (line-beginning-position 1)))
         (end (if (use-region-p)
                  (save-excursion
                    (goto-char (region-end))
                    (line-beginning-position 2))
                (line-beginning-position 2)))
         (point-excursion (- (point) end))
         (mark-excursion (- (mark) (point)))
         (text (delete-and-extract-region beg end)))
    ;; Shift text.
    (forward-line step)
    (insert text)
    ;; Set new point.
    (goto-char (+ (point) point-excursion))
    ;; Set new mark.
    (when (use-region-p)
      (push-mark (+ (point) mark-excursion) t t))))

(defun oops-indent-lines (step)
  (and (not (integerp step))
       (error "oops-indent-lines's argument STEP should be an integer! step = %s"
              step))
  (let ((beg (if (use-region-p)
                 (save-excursion
                   (goto-char (region-beginning))
                   (line-beginning-position))
               (line-beginning-position)))
        (end (if (use-region-p)
                 (save-excursion
                   (goto-char (region-end))
                   (line-end-position))
               (line-end-position))))
    (indent-rigidly beg end step)))

;;;###autoload
(defun oops-undo ()
  "Discard the selection and then undo something."
  (interactive)
  (deactivate-mark)
  (undo))

;;;###autoload
(defun oops-toggle-comment ()
  "Toggle comment smartly. It support comment on both single line and multiple lines."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (line-beginning-position))
            end (save-excursion
                  (goto-char (region-end))
                  (line-end-position))))
    (if (/= beg end)
        (comment-or-uncomment-region beg end)
      (message "No need to comment."))))

;;;###autoload
(defun oops-duplicate-lines ()
  "Duplciate the current line or the lines between point and mark without modifying `kill-ring'."
  (interactive)
  ;; There're two situation:
  ;; (point) ---------------
  ;; -----------------(mark)
  ;; or
  ;; (mark)-----------------
  ;; ----------------(point)
  (let* ((beg (line-beginning-position 1))
         (end (line-beginning-position 2))
         (str (buffer-substring-no-properties beg end)))
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line 1)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (beginning-of-line 2)
                  (point))
            str (buffer-substring-no-properties beg end)))
    (save-excursion
      (if (and mark-active
               (> (mark) (point)))
          (goto-char (region-end)))
      (beginning-of-line 2)
      (insert str))
    ;; Restore selection.
    (setq deactivate-mark nil)))

;;;###autoload
(defun oops-kill-lines ()
  "Kill the current line or the lines between point and mark without modifying `kill-ring'."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (when mark-active
      (setq beg (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line 1)
                  (point))
            end (save-excursion
                  (goto-char (region-end))
                  (beginning-of-line 2)
                  (point))))
    (delete-region beg end))
  (and (eobp) (forward-line -1)))

;;;###autoload
(defun oops-tab ()
  "Prompt completion or do format for region."
  (interactive)
  (cond
   ;; For Custom-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((string-match "^\*Customize.*\*$" (buffer-name))
    (widget-forward 1))
   ;; Default action ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (t
    (if (or (region-active-p)
            (looking-at "^"))
        (indent-for-tab-command)
      (if (minibufferp)
          (minibuffer-complete)
        (company-complete))))))

;;;###autoload
(defun oops-kill-files ()
  (interactive)
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (buffer-file-name buffer))
      (kill-buffer buffer))))

;;;###autoload
(defun oops-rename ()
  "To rename symbol in batch way."
  (interactive)
  (let* ((fnmap
          ;; '((js-mode . tern-rename-variable)
          ;;   (js2-mode . tern-rename-variable))
          '())
         (fn (cdr (assoc major-mode fnmap))))
    (if fn
        ;; Specific function in terms of major mode.
        (call-interactively fn)
      ;; Use a default way.
      (let ((thing (if (region-active-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     (let ((bounds (bounds-of-thing-at-point 'symbol)))
                       (and bounds
                            (buffer-substring-no-properties
                             (car bounds)
                             (cdr bounds))))))
            from to)
        (save-excursion
          (if thing
              (and (setq from thing
                         to (read-string (format "Replace %s with: "
                                                 from)))
                   (replace-string from to nil (point-min) (point-max)))
            (call-interactively 'query-replace)))))))

;;;###autoload
(defun oops-split-window ()
  (interactive)
  (let ((dir (ido-completing-read "Split direction: "
                                  '("horizontal" "vertical")
                                  nil t)))
    (select-window
     (cond
      ((string= dir "vertical")
       (split-window (selected-window) nil 'below))
      ((string= dir "horizontal")
       (split-window (selected-window) nil 'right))))
    ;; Resize window.
    (balance-windows (window-parent (selected-window)))))

;;;###autoload
(defun oops-kill-buffer ()
  "Kill selected buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;;###autoload
(defun oops-kill-window ()
  "Kill selected window and balance the rest windows if there're more than two."
  (interactive)
  ;; Delete window when there're more than two.
  (and (>= (length (window-list)) 2)
       (delete-window (selected-window)))
    (unless (eq (frame-root-window) (window-parent))
      (balance-windows (window-parent))))

;;;###autoload
(defun oops-show/copy-file-path ()
  "Show the file name and copy it to the kill ring."
  (interactive)
  (let ((name (and (buffer-file-name)
                   (file-truename (buffer-file-name)))))
    (when name
      (kill-new name)
      (message "Current File: %s" name))))

;; Emacs instance started from the GUI inherits a default set of environment
;; variables on OSX.
(cond
 ;; Running in windows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ((memq window-system '(mac ns))
  ;; `exec-path-from-shell'
  (ignore-errors
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize))
  ;; Show buffer-file-name in the `frame-title-format'
  (setq frame-title-format
        (list '(buffer-file-name "%f" (dired-directory dired-directory "%b")))))
 ;; Running in terminal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ((member (tty-type) '("xterm" "xterm-256color"))
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2C" [S-right])
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;2F" [S-end])
  (define-key input-decode-map "\e[1;2H" [S-home])

  (define-key input-decode-map "\e[1;4A" [M-S-up])
  (define-key input-decode-map "\e[1;4B" [M-S-down])
  (define-key input-decode-map "\e[1;4C" [M-S-right])
  (define-key input-decode-map "\e[1;4D" [M-S-left])
  (define-key input-decode-map "\e[1;4F" [M-S-end])
  (define-key input-decode-map "\e[1;4H" [M-S-home])

  (define-key input-decode-map "\e[1;5A" [C-up])
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5C" [C-right])
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;5F" [C-end])
  (define-key input-decode-map "\e[1;5H" [C-home])

  (define-key input-decode-map "\e[1;6A" [C-S-up])
  (define-key input-decode-map "\e[1;6B" [C-S-down])
  (define-key input-decode-map "\e[1;6C" [C-S-right])
  (define-key input-decode-map "\e[1;6D" [C-S-left])
  (define-key input-decode-map "\e[1;6F" [C-S-end])
  (define-key input-decode-map "\e[1;6H" [C-S-home])

  ;; (define-key input-decode-map "\e[1;7A" [C-M-up])
  ;; (define-key input-decode-map "\e[1;7B" [C-M-down])
  ;; (define-key input-decode-map "\e[1;7C" [C-M-right])
  ;; (define-key input-decode-map "\e[1;7D" [C-M-left])
  ;; (define-key input-decode-map "\e[1;7F" [C-M-end])
  ;; (define-key input-decode-map "\e[1;7H" [C-M-home])
  ;; (define-key input-decode-map "\e[1;8A" [C-M-S-up])
  ;; (define-key input-decode-map "\e[1;8B" [C-M-S-down])
  ;; (define-key input-decode-map "\e[1;8C" [C-M-S-right])
  ;; (define-key input-decode-map "\e[1;8D" [C-M-S-left])

  ;; (define-key input-decode-map "\e[1;8F" [C-M-S-end])
  ;; (define-key input-decode-map "\e[1;8H" [C-M-S-home])﻿
  ))

;; Necessary packages.
(when load-file-name
  (mapc (lambda (subdir)
          (add-to-list 'load-path
                       (concat (file-name-directory load-file-name)
                               subdir)))
        '("hl-anything"
          "searchq"
          "history"
          "whereis")))

(require 'history)
(require 'hl-anything)
(require 'whereis)
(require 'searchq)

(history-mode 1)
(hl-highlight-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3rd Party Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `smart-shift'
(when (ignore-errors
        (require 'smart-shift))
  (global-smart-shift-mode 1))

;; `guide-key'.
(when (ignore-errors
        (require 'guide-key))
  (custom-set-variables
   '(guide-key/idle-delay 0.7)
   '(guide-key/guide-key-sequence t)
   '(guide-key/popup-window-position 'bottom))
  ;; Enable global minor mode.
  (guide-key-mode 1))

;; `helm'.
(when (ignore-errors
        (and (require 'helm-imenu)
             (require 'helm-files)))
  ;; helm core.
  (custom-set-variables
   '(helm-split-window-in-side-p t)
   '(helm-buffers-fuzzy-matching t)
   '(helm-truncate-lines t)
   `(helm-sources-using-default-as-input ',(delq
                                            'helm-source-imenu
                                            helm-sources-using-default-as-input)))

  ;; helm imenu.
  (custom-set-variables
   '(helm-imenu-fuzzy-match t)
   '(helm-imenu-execute-action-at-once-if-one nil))
  ;; helm find file.
  (custom-set-variables
   '(helm-ff-skip-boring-files t)))

;; `company'.
(when (ignore-errors
        (require 'company))
  (custom-set-variables
   '(company-idle-delay 0.2)
   '(company-minimum-prefix-length 2)
   '(company-frontends '(company-pseudo-tooltip-frontend
                         company-preview-frontend
                         company-echo-metadata-frontend))
   '(company-backends `(;; For ELISP.
                        (company-elisp
                         company-yasnippet company-files
                         company-dabbrev-code company-keywords)
                        ;; For Makefile.
                        (company-cmake
                         company-yasnippet company-files
                         company-dabbrev-code company-keywords)
                        ;; For Python/C/C++.
                        ;; (company-ycmd company-files company-yasnippet)
                        ;; For JavaScript.
                        ,(and (ignore-errors
                                (require 'company-tern))
                              '(company-tern
                                company-yasnippet company-files
                                company-dabbrev-code company-keywords))
                        ;; For CSS.
                        (company-css
                         company-files)
                        ;; For Java.
                        ,(and (ignore-errors
                                (require 'eclim))
                              '(company-eclim
                                company-yasnippet company-files
                                company-dabbrev-code company-keywords))
                        ;; For generic file.
                        (company-capf
                         ,(and (ignore-errors
                                 (require 'yasni))
                               'company-yasnippet)
                         company-dabbrev-code company-files
                         company-gtags company-etags company-keywords))))
  ;; `company-keywords' for specific languages.
  (when (require 'company-keywords)
    (let ((keywords `((js-mode
                       ;; Standard HTML.
                       "console" "prototype" "module" "exports" "require" "import" "window"
                       "document" "target" "getElementById" "getElementsByName"
                       "getElementsByClassName" "getElementsByTagName" "getElementsByTagNameNS"
                       "childNodes" "addEventListener" "removeEventListener" "parentNode" "style"
                       "display" "none" "block" "flex"
                       ;; For React.JS framework.
                       "componentWillMount" "componentDidMount" "componentWillUnmount"
                       "isMounted" "getDOMNode" "shouldComponentUpdate" "componentWillUpdate"
                       "componentDidUpdate" "getInitialState" "currentTarget"
                       "render" "forceUpdate" "setState" "replaceState" "setProps" "replaceProps"
                       "onCopy" "onCut" "onPaste" "onKeyDown" "onKeyPress" "onKeyUp" "onFocus"
                       "onBlur" "onChange" "onInput" "onSubmit" "onClick" "onContextMenu"
                       "onDoubleClick" "onDrag" "onDragEnd" "onDragEnter" "onDragExit" "onDragLeave"
                       "onDragOver" "onDragStart" "onDrop" "onMouseDown" "onMouseEnter" "onMouseLeave"
                       "onMouseMove" "onMouseOut" "onMouseOver" "onMouseUp" "onTouchCancel"
                       "onTouchEnd" "onTouchMove" "onTouchStart" "onScroll" "onWheel")
                      ,(and (featurep 'js2-mode)
                            '(js2-mode . js-mode))
                      ,(and (featurep 'jade-mode)
                            '(jade-mode
                              "doctype" "html" "head" "body" "link" "form" "input"
                              "button" "br" "script" "extends" "block")))))
      (mapc (lambda (alist)
              (let* ((key (car alist))
                     (val (cdr alist))
                     (field (assq key company-keywords-alist)))
                (if field
                    ;; If key already exists.
                    (if (listp (cdr field))
                        ;; If val field is a list.
                        (setf (cdr field) (append (cdr field) val))
                      ;; If not.
                      (setf (cdr field) val))
                  ;; If not.
                  (add-to-list 'company-keywords-alist (cons key val)))))
            keywords)))
  ;; Enable global/minor mode.
  (global-company-mode 1))

;; `yasnippet'.
(when (ignore-errors
        (require 'yasnippet))
  (custom-set-variables
   `(yas-snippet-dirs ',(add-to-list 'yas-snippet-dirs
                                     (format "%s/snippets"
                                             (file-name-directory (or load-file-name
                                                                      buffer-file-name))))))
  ;; Enable global/minor mode.
  (yas-global-mode 1))

;; `projectile'.
(when (ignore-errors
        (and (require 'projectile)
             (require 'helm-projectile)))
  (custom-set-variables
   '(projectile-completion-system 'helm)
   ;; '(projectile-require-project-root nil)
   ))

;; `js2-mode' for JavaScript.
(when (ignore-errors
        (require 'js2-mode))
  (custom-set-variables
   '(js2-basic-offset 2)
  ;;  ;; Turn off all the warning for performance.
   '(js2-mode-show-strict-warnings nil)
   '(js2-strict-cond-assign-warning nil)
   '(js2-strict-inconsistent-return-warning nil)
   '(js2-strict-missing-semi-warning nil)
   '(js2-strict-trailing-comma-warning nil)
   '(js2-strict-var-hides-function-arg-warning nil)
   '(js2-strict-var-redeclaration-warning nil)
   '(js2-mode-show-parse-errors nil))
  ;; Hook
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'js-mode-hook 'js2-imenu-extras-mode)
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  )

;; `tern-mode' for JavaScript.
(when (ignore-errors
        (require 'tern))
  (add-to-list 'history-advised-before-functions 'tern-find-definition)
  (add-to-list 'history-advised-after-functions 'tern-find-definition)
  (add-hook 'js-mode-hook 'tern-mode)
  (add-hook 'js2-mode-hook 'tern-mode)
  ;; Assign default directory for searchq package.
  (add-hook 'tern-mode-hook (lambda ()
                              (and
                               (featurep 'searchq)
                               (setq searchq-file-history (list (tern-project-dir)))))))

;; `jade-mode' and `stylus-mode'
(when (ignore-errors
        (and (require 'jade-mode)
             (require 'stylus-mode)))
  (add-to-list 'auto-mode-alist '("\\.styl\\'" . stylus-mode))
  (add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode)))

;; `eclim' for Java.
(when (ignore-errors
        (require 'eclim))
  (custom-set-variables
   '(eclim-eclipse-dirs '("~/nonStandard/eclipse"))
   '(eclim-executable "~/nonStandard/eclipse/eclim"))
  ;; Hook
  (add-hook 'java-mode-hook 'eclim-mode))

;; `ycmd' for Python/C/C++.
;; (when (ignore-errors
;;         (require 'ycmd))
;;   (ycmd-setup)
;;   (setq ycmd-server-command `("python"
;;                               ,(expand-file-name "~/_CODE/ycmd/ycmd"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Necessary Minor Modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapc (lambda (hook)
        (mapc (lambda (mode)
                (and hook mode
                     (add-hook hook mode)))
              ;; Customized minor modes list.
              `(
                linum-mode
                hl-line-mode
                hl-paren-mode
                ,(ignore-errors
                   (and (require 'diff-hl)
                        'diff-hl-mode)))))
      ;; Customized major modes hook.
      `(fundamental-mode-hook
        emacs-lisp-mode-hook
        python-mode-hook
        java-mode-hook
        js-mode-hook
        ,(ignore-errors
           (and (require 'js2-mode)
                'js2-mode-hook))
        html-mode-hook
        ,(ignore-errors
           (and (require 'json-mode)
                'json-mode-hook))
        ,(ignore-errors
           (and (require 'jade-mode)
                'jade-mode-hook))
        ,(ignore-errors
           (and (require 'stylus-mode)
                'stylus-mode-hook))
        css-mode-hook
        c-mode-hook
        c++-mode-hook
        shell-mode
        sh-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Binding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a prefix map for window.
(defvar oops-window-prefix-map
  (let ((map (make-sparse-keymap)))
    map))
(define-prefix-command 'oops-window-prefix-map)

;; Create a prefix map for window.
(defvar oops-project-prefix-map
  (let ((map (make-sparse-keymap)))
    map))
(define-prefix-command 'oops-project-prefix-map)

;; Ctrl + keys.
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-b") 'helm-mini)
(global-set-key (kbd "C-d") 'oops-duplicate-lines)
(global-set-key (kbd "C-S-d") 'oops-kill-lines)
(global-set-key (kbd "C-f") 'helm-occur)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-o") 'helm-imenu)
(global-set-key (kbd "C-r") 'oops-project-prefix-map)
(global-set-key (kbd "C-r b") 'projectile-find-file)
(global-set-key (kbd "C-r f") 'speedbar)
;; (global-set-key (kbd "C-r d") 'prj-delete-project)
;; (global-set-key (kbd "C-r f") 'prj-search-project)
;; (global-set-key (kbd "C-r l") 'prj-load-project)
;; (global-set-key (kbd "C-r n") 'prj-new-project)
(global-set-key (kbd "C-r p") 'oops-show/copy-file-path)
(global-set-key (kbd "C-r r") 'oops-rename) ;; TODO: to helm**replace.
(global-set-key (kbd "C-r u") 'revert-buffer)
;; (global-set-key (kbd "C-r ,") 'prj-edit-project)
(global-set-key (kbd "C-r <left>") 'history-prev-history)
(global-set-key (kbd "C-r <right>") 'history-next-history)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-w") 'oops-window-prefix-map)
(global-set-key (kbd "C-w w") 'oops-kill-window)
(global-set-key (kbd "C-w b") 'oops-kill-buffer)
(global-set-key (kbd "C-w d") 'oops-split-window)
(global-set-key (kbd "C-w s") 'whereis-symbol-mode)
(global-set-key (kbd "C-w <up>") 'windmove-up)
(global-set-key (kbd "C-w <down>") 'windmove-down)
(global-set-key (kbd "C-w <right>") 'windmove-right)
(global-set-key (kbd "C-w <left>") 'windmove-left)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r l") 'helm-bookmarks)
(global-set-key (kbd "C-z") 'oops-undo)
(global-set-key (kbd "C-/") 'oops-toggle-comment)
(global-set-key (kbd "C-<space>") '(lambda ()
                                     (interactive)
                                     (message "dude")))

;; Meta + keys.
;; (global-set-key (kbd "C-M-<left>") 'backward-sexp)
;; (global-set-key (kbd "C-M-<right>") 'forward-sexp)

;; Function keys.
(global-set-key (kbd "<tab>") 'oops-tab)
;; ! The override for built-in indent.el library. Without this, it won't work
;; in the terminal.
(global-set-key "\t" 'oops-tab)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "<f1>") 'searchq-search)
(global-set-key (kbd "S-<f1>") 'searchq-toggle-result)
(global-set-key (kbd "<f2>") 'hl-highlight-thingatpt-global)
(global-set-key (kbd "S-<f2>") 'hl-unhighlight-all-global)
(global-set-key (kbd "<f3>") 'hl-find-next-thing)
(global-set-key (kbd "S-<f3>") 'hl-find-prev-thing)
(global-set-key (kbd "<f4>") 'whereis-goto-symbol)

(provide 'oops)
