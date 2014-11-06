;; Copyright (C) 2014
;;
;; Author: BoyW165
;; Version: 0.0.1
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
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
;; 2014-10-01 (0.0.1)
;;    Initial release.

;; GNU library.
(require 'thingatpt)

;; 3rd party library.
(require 'hl-anything)

(defun sos-jedi-thingatpt ()
  "Find symbol string around the point or text selection."
  (let ((bound (if mark-active
                   (cons (region-beginning) (region-end))
                 (unless (memq (get-text-property (point) 'face)
                               '(font-lock-doc-face
                                 font-lock-string-face
                                 font-lock-comment-face))
                   (bounds-of-thing-at-point 'symbol)))))
    (and bound
         ;; Return (THING BEG END FILENAME)
         (list (buffer-substring-no-properties (car bound) (cdr bound))
               (car bound)
               (cdr bound)
               (substring-no-properties (or buffer-file-name ""))))))

;; ((:column 19 :module_name "jediepcserver" :description "*args" :line_nr 103 :module_path "/Users/Boy/Documents/_LISP/emacs-jedi/jediepcserver.py"))
;; `font-lock-add-keywords'
(defun sos-jedi-call:goto_definitions (thing)
  (lexical-let ((thing (car thing)))
    (deferred:nextc
      (jedi:call-deferred 'goto)
      (lambda (reply &rest ignore)
        (let (candidates)
          (dolist (cand reply)
            (let* ((type "symbol")
                   (file (plist-get cand :module_path))
                   (linum (or (plist-get cand :line_nr) 1))
                   (column (or (plist-get cand :column) 1))
                   (description (plist-get cand :description))
                   (keywords `((,(if (string-match thing description)
                                     (replace-regexp-in-string
                                      "\\s-+"
                                      "\\\\s-*"
                                      (format "%s\\\(%s\\\)%s"
                                              (regexp-quote (substring-no-properties description 0 (match-beginning 0)))
                                              (regexp-quote thing)
                                              (regexp-quote (substring-no-properties description (match-end 0)))))
                                   thing)
                                1 'hl-symbol-face prepend))))
              (and file (file-exists-p file)
                   (push (list :symbol thing
                               :type type
                               :file file
                               :linum linum
                               :keywords keywords)
                         candidates))))
          candidates)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun sos-jedi-backend (command &rest args)
  (case command
    (:symbol
     (when (and (eq major-mode 'python-mode))
       (or (sos-jedi-thingatpt) :stop)))
    (:candidates
     (let ((thing (car args)))
       (require 'jedi)
       ;; (setq epc:debug-out t)
       (sos-jedi-call:goto_definitions thing)))))

(provide 'sos-jedi-backend)
