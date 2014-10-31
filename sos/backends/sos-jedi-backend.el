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

(require 'thingatpt)

(require 'hl-faces)

(defun sos-jedi-thingatpt ()
  "Find symbol string around the point or text selection."
  (let ((bound (if mark-active
                   (cons (region-beginning) (region-end))
                 (unless (memq (get-text-property (point) 'face)
                               '(font-lock-doc-face
                                 font-lock-string-face
                                 font-lock-comment-face))
                   (bounds-of-thing-at-point 'symbol)))))
    (and bound (buffer-substring-no-properties (car bound) (cdr bound)))))

(defun sos-jedi-call:goto_definitions (thing)
  (lexical-let ((thing thing))
    (deferred:nextc
      (epc:call-deferred (jedi:get-epc) 'goto
                         (list (buffer-substring-no-properties 1 (point-max))
                               (line-number-at-pos)
                               (current-column)
                               (jedi:-buffer-file-name)))
      (lambda (reply)
        (let (candidates)
          (dolist (cand reply)
            (let ((type "symbol")
                  (file (plist-get cand :module_path))
                  (linum (or (plist-get cand :line_nr) 1))
                  (keywords))
              (and file (push (list :symbol thing
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
