;;; whereis-symbol.el --- Smartly detect symbol at point and show symbol's whence in an isolated window.
;;
;; Copyright (C) 2014
;;
;; Author: boyw165
;; Version: 0.0.1
;; Compatibility: GNU Emacs 24.3+
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
;; This is a framework that refers to the point and show useful information.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-10-31 (0.0.x)
;; * Support asynchronous backends which use `deferred' library.
;;
;; 2014-08-01 (0.0.1)
;; * Initial release which was inspired by `company-mode'.
;;   https://github.com/company-mode/company-mode
;;
;;; Code:

;; GNU library.
(eval-when-compile (require 'cl))

;; whereis-symbol's library.
(and load-file-name
     (let ((dir (file-name-directory load-file-name)))
       (add-to-list 'load-path (concat dir "/frontends"))
       (add-to-list 'load-path (concat dir "/backends"))))
;; Default frontends.
(require 'ws-default-frontend)
;; Default backends.
(require 'ws-elisp-backend)
(require 'ws-jedi-backend)
(require 'ws-cc++-backend)
(require 'ws-grep-backend)
(require 'ws-candidates-preview-backend)

(defgroup whereis-symbol nil
  "An utility to show you documentation in an isolated bottom window by finding 
meaningful information at point.")

(defcustom ws-backends '(ws-elisp-backend
                         ws-jedi-backend
                         ws-cc++-backend
                         ws-grep-backend
                         ws-candidates-preview-backend)
  "The list of backends for the purpose of collecting candidates. The engine 
will dispatch all the back-ends and pass specific commands in order. By passing 
command and get return data from a backend, the engine gets information to show 
the result in an isolated bottom window, minibuffer or popup a GUI dialog, etc.

Example:
--------
  (defun some-backend (command &rest args)
    (case command
      (:init t)
      (:symbol (and (member major-mode MAJOR_MODE_CANDIDATES)
                    (thing-at-point 'symbol))))
      (:candidates (list STRING01 STRING02 STRING03 ...)))

Each backend is a function that takes numbers of arguments. The first argument is 
the COMMAND requested from the enine. The remaining arguments depends on the 1st 
argument, COMMAND.

Commands:
---------
`:init': Called once for each buffer. The backend can check for external programs
and files and load any required libraries.

`:symbol': The back-end should return anything non-nil, nil or 'stop.
  * Return non-nil except `:stop' tells engine that it is for the current buffer.
    Then, the engine will stop iteration and ask backend for candidates refer 
    to returned value; `:stop' tells engine to do nothing.
    Note: Return value will be saved in `ws-symbol'.
  * Return nil tells engine to skip the backend and continue the iteration.

`:candidates': The backend should return a CANDIDATE list or nil.
  The 2nd argument, SYMBOL, is the symbol (stored in `ws-symbol');
  The 3rd argument, IS-PREFIX, is a boolean indicating whether the symbol is just a 
  prefix or not;
  The 4th argument, SEARCH-GLOBAL, is a boolean indicating whether to search 
  globally or just locally in the current file.

  Backend could use the symbol and remaining argument to find candidates.
  * Return a CANDIDATE list tells engine where the definition are at. The CANDIDATE 
    is an alist with pre-defined keys and respective values.
  * Return nil tells engine there's no definition.

  CANDIDATE's Keys:
  -----------------
  `:doc': Buffer text which is the file content (buffer-string).
  or
  `:file': The absolute file path.
  Note: `:doc' and `:file' are exclusive.

  `:linum': The line number (integer).
  or
  `:offset': The offset in the file (integer).
  Note: `:linum' and `:offset' are exclusive.

  CANDIDATE's Optional Keys:
  --------------------------
  `:keywords': A list containing elements to highlight keywords. Its format is 
               `font-lock-keywords'. The 1st matcher must be for the symbol 
               definition.

  `:symbol': The symbol's name (string).

  `:type': A string describing the type of symbol.

  Example - File Candidates:
  --------------------------
  ((:symbol \"HelloFunc\"
    :type   \"function\"
    :file   \"~/project/a/b/c\"
    :linum  100
    :match  '((\"HelloFunc\" . highlight))) ...)

  Example - Document Candidates:
  ------------------------------
  ((:symbol \"HelloFunc\"
    :doc    \"This is a documentary of the built-in ...\"
    :type   \"built-in\"
    :linum  1) ...)"
  :type '(repeat (symbol :tag "Back-end"))
  :group 'whereis-symbol)

(defvar ws-symbol nil
  "Cache the returned value from backends by `:symbol' command.")
(make-variable-frame-local 'ws-symbol)

(defvar ws-backend nil
  "Cache backend which is specifically for the current buffer.")
(make-variable-buffer-local 'ws-backend)

(defun ws-call-backend (command &rest args)
  "Find workable BACKEND and call it."
  (if ws-backend
      (apply ws-backend command args)
    (dolist (backend ws-backends)
      (when (setq ws-symbol (funcall backend :symbol))
        (return (apply (setq ws-backend backend)
                       command
                       args))))))

(defun ws-init-backend (backend)
  "Ask BACKEND to initialize itself."
  (funcall backend :init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Where Is Symbol Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom ws-frontends '(ws-definition-window-frontend)
  "The list of frontends for the purpose of visualization.

Commands:
---------
`:init'     - Initialize something (called once).
`:destroy'  - Destroy something (called once).
`:show'     - Show visualization.
`:hide'     - Hide visualization.
`:update'   - Update visualization.

Example:
--------
  (defun some-frontend (command &rest args)
    (case command
      (:init t)
      (:show (message PROMPTY)))
      (:hide ...)
      (:destroy ...))"
  :type '(repeat (symbol :tag "Front-end"))
  :group 'whereis-symbol)

(defcustom ws-idle-delay 0.3
  "The idle delay in seconds before the engine starts."
  :type '(number :tag "Seconds"))

(defvar ws-timer nil
  "The idle timer to call `ws-idle-begin'.")

(defvar ws-source-buffer nil
  "Cached source code buffer.")

(defvar ws-source-window nil
  "Cached window where the source code buffer is at.")

(defvar ws-is-skip nil
  "t to skip `ws-idle-begin'.")
(make-variable-buffer-local 'ws-is-skip)

(defun ws-call-frontends (command &rest args)
  (dolist (frontend ws-frontends)
    (apply frontend command args)))

(defun ws-is-skip-command (&rest commands)
  "Return t if `this-command' should be skipped.
If you want to skip additional commands, try example:
  (ws-is-skip-command 'self-insert-command 'previous-line ...)"
  (memq this-command `(mwheel-scroll
                       save-buffer
                       eval-buffer
                       eval-last-sexp
                       ;; Additional commands.
                       ,@commands)))

(defun ws-is-idle-begin ()
  (not (or (active-minibuffer-window)
           (ws-is-skip-command)
           ws-is-skip)))

(defun ws-idle-begin ()
  (when (ws-is-idle-begin)
    (setq ws-source-buffer (current-buffer)
          ws-source-window (selected-window))
    (condition-case err
        (let ((old-symbol ws-symbol))
          (setq ws-symbol (ws-call-backend :symbol))
          (if (memq ws-symbol '(nil :stop))
              ;; Return nil or `:stop' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              (ws-call-frontends :hide)
            ;; non-nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (if (equal ws-symbol old-symbol)
                (ws-call-frontends :update)
              (let ((candidates (ws-call-backend :candidates ws-symbol)))
                (if candidates
                    (cond
                     ;; Normal candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ((listp candidates)
                      (ws-call-frontends :show candidates))
                     ;; `deferred' candidate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ((and (require 'deferred) (deferred-p candidates))
                      (deferred:nextc
                        candidates
                        (lambda (real-candidates)
                          (ws-call-frontends :show real-candidates)))))
                  (ws-call-frontends :hide))))))
      (error (error "whereis-symbol-mode error: %s" err)))))

;;;###autoload
(define-minor-mode whereis-symbol-mode
  "This local minor mode gethers symbol returned from backends around the point 
and show the reference visually through frontends. Usually frontends output the 
result to the `ws-def-buf' displayed in the `ws-def-win'."
  :global t
  :group 'whereis-symbol
  (if whereis-symbol-mode
      (progn
        ;; Initialize front-ends & back-ends.
        (ws-call-frontends :init)
        (mapc 'ws-init-backend ws-backends)
        (setq ws-timer (run-with-idle-timer ws-idle-delay t 'ws-idle-begin)))
    ;; Destroy front-ends & back-ends.
    (ws-call-frontends :destroy)
    (when ws-timer
      (cancel-timer ws-timer)
      (setq ws-timer nil))))

;; TODO: menu-bar and tool-bar keymap.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go to Definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom ws-goto-frontend 'ws-goto-definitions-frontend
  "A frontend controls the way to goto definition both for single candidate and 
multiple candidates. Default is `ws-goto-definitions-frontend'.

Commands:
---------
`:show': Show the result. The 1st argument is the candidates."
  :type '(symbol :tag "Front-end")
  :group 'whereis-symbol)

(defcustom ws-before-goto-hook nil
  "A hook triggered before going to the definition."
  :type '(repeat function)
  :group 'whereis-symbol)

(defcustom ws-after-goto-hook nil
  "A hook triggered after going to the definition."
  :type '(repeat function)
  :group 'whereis-symbol)

(defun ws-call-goto-frontends (command &rest arg)
  "Call goto-frontends."
  (apply ws-goto-frontend command arg))

;;;###autoload
(defun ws-goto-definition ()
  (interactive)
  (when (ws-is-idle-begin)
    (condition-case err
        (let ((symbol (ws-call-backend :symbol)))
          ;; not nil or `:stop'.
          (unless (memq symbol '(nil :stop))
            (let ((candidates (ws-call-backend :candidates symbol)))
              (when candidates
                (run-hooks ws-before-goto-hook)
                (cond
                 ;; Normal candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 ((listp candidates)
                  (ws-call-goto-frontends :show candidates))
                 ;; `deferred' candidate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                 ((and (require 'deferred) (deferred-p candidates))
                  (deferred:nextc
                    candidates
                    (lambda (real-candidates)
                      (ws-call-goto-frontends :show real-candidates)))))
                (run-hooks ws-after-goto-hook)))))
      (error (error "ws-goto-definition error: %s" err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ws-display-goto-result ()
  (let ((candidates (ws-call-backend :candidates "" t)))
    ))

;;;###autoload
(defun ws-goto-local-symbol ()
  (interactive)
  ;; TODO:
  (minibuffer-with-setup-hook
      (lambda ()
        (setq *grizzl-current-result* nil)
        (setq *grizzl-current-selection* 0)
        (grizzl-mode 1)
        (lexical-let*
            ((hookfun (lambda ()
                        (setq *grizzl-current-result*
                              (grizzl-search (minibuffer-contents)
                                             index
                                             *grizzl-current-result*))
                        (grizzl-display-result index prompt)))
             (exitfun (lambda ()
                        (grizzl-mode -1)
                        (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t))
        )
    (read-from-minibuffer ">>> ")
    (grizzl-selected-result index)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ws-goto-global-symbol ()
  (interactive)
  ;; TODO:
  )

(provide 'whereis-symbol)
