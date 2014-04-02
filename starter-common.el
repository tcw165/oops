;; Author:       Boy Wang
;; Last-Updated: xxx
;; URL:          xxx
;;
;;

(add-to-list 'load-path "~/.emacs.d/starter/3rd-party")

;; 3rd party library.
(require 'highlight-symbol)
(require 'highlight-parentheses)
(require 'auto-complete)
;; native library.
(require 'thingatpt)

;; ====================== Common Feature =======================

;; TODO: to oops-lisp-core.el
(defun oops-lisp-is-library ()
  "Check if the cursor or the selection is on which just after 'require statement.
Return t if is on which just after 'require statement. Other is nil."
  (save-excursion
    (backward-up-list)
    (forward-char 1)
    (if (string-equal "require" (buffer-substring-no-properties (point) (+ (point) 7)))
        t ;; return t.
      nil ;; return nil.
      )
    )
  )

;; TODO: to oops-lisp-core.el
(defun oops-lisp-skip-parenthesis ()
  "Move the point forward 1 word if it's just before a \"(\".
Move the point backward 1 word if it's just after a \")\"."
  (unless mark-active
    (cond
     ;; Move the point forward 1 word if it's just before a "(".
     ((char-equal (get-byte nil "(") (char-after (point)))
      (forward-word 1)
      )
     ;; Move the point backward 1 word if it's just after a ")".
     ((char-equal (get-byte nil ")") (char-before (point)))
      (backward-word 1)
      )
     )
    )
  )

;; TODO: to oops-core.el
(defun oops-thing-at-point ()
  "Return string on which the point is or just string of selection."
  (if mark-active
      ;; if there is already a selection, return the selection.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; else.
    (thing-at-point 'symbol)
    )
  )

;; TODO: to oops-core.el
(defun oops-find-definition ()
  "Find the symbol definition both for function, variable or library."
  (interactive)
  (cond
   ;; major-mode == "emacs-lisp-mode", lisp-interaction-mode
   ((or (eq major-mode 'emacs-lisp-mode)
        (eq major-mode 'lisp-interaction-mode)
        )
    ;; Force to skip parenthesis.
    (oops-lisp-skip-parenthesis)
    (let* ((is-lib (oops-lisp-is-library))
           (text (oops-thing-at-point))
           (symb (read text))
           )
      ;; Force to unselect text.
      (deactivate-mark)
      (cond
       ;; library
       ((and is-lib (not (string-equal "require" text)))
        (find-library text)
        (message "library: %s" text)
        ;; TODO: go to (require 'text) line
        )
       ;; Function:
       ((fboundp symb)
        (find-function symb)
        (message "function: %s" text)
        )
       ;; Variable:
       ((boundp symb)
        (find-variable symb)
        (message "variable: %s" text)
        )
       ;; Unknown symbol:
       (t
        (message "unknown symbol: %s" text)
        )
       )
      )
    )
   ;; Clause: major-mode == ...
   ((eq major-mode 'c-mode)
    (message "not implement yet...")
    )
   )
  )

;; TODO: to oops-core.el
(defun starter-kill-current-buffer ()
  "Kill current-buffer."
  (interactive)
  (kill-buffer (current-buffer))
  )

;; TODO: to oops-core.el
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
(global-set-key (kbd "<f4>") 'oops-find-definition)

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
