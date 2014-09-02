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

;; (defun oops-lisp-show-help-atpt ()
;;   (let* ((symbol (intern-soft (oops--lisp-thingatpt)))
;;          search-result)
;;     (when symbol
;;       (cond
;;        ;; TODO: Support feature.
;;        ;; Library:
;;        ;; ((featurep symbol)
;;        ;;  nil
;;        ;;  )
;;        ;; Function:
;;        ((fboundp symbol)
;;         (setq search-result (oops--lisp-find-function symbol))
;;         (if search-result
;;             (oops-update-help search-result)
;;           ;; Built-in function, show HELP.
;;           (oops-update-help (oops--lisp-describe-function symbol))))
;;        ;; Variable:
;;        ((boundp symbol)
;;         (setq search-result (oops--lisp-find-variable symbol))
;;         (if search-result
;;             (oops-update-help search-result)
;;           ;; Built-in variable, show HELP.
;;           (oops-update-help (oops--lisp-describe-variable symbol))))
;;        ;; Face:
;;        ((facep symbol)
;;         (setq search-result (oops--lisp-find-face symbol))
;;         (if search-result
;;             (oops-update-help search-result)
;;           ;; Built-in variable, show HELP.
;;           (oops-update-help (oops--lisp-describe-variable symbol))))
;;        ;; Keyword:
;;        ((keywordp symbol)
;;         (message "[Definition] It's a keyword, %s" symbol))))))

(defun sos-elisp-backend (command &optional arg)
  (case command
    (:init t)
    (:symbol
     (when (member major-mode '(emacs-lisp-mode lisp-interaction-mode))
       (format "%s" (current-time))))
    (:candidates
     ;; Single candidate case:
     (let* ((num (random 1000))
            (i (% num 10)))
       (list (nth i '((:file "~/.emacs.d/oops/sos/text1.el" :line 5)
                      (:file "~/.emacs.d/oops/sos/text2.el" :line 10)
                      (:file "~/.emacs.d/oops/sos/text3.el" :line 15)
                      (:file "~/.emacs.d/oops/sos/text4.java" :line 20)
                      (:file "~/.emacs.d/oops/sos/text5.el" :line 25)
                      (:file "~/.emacs.d/oops/sos/text6.c" :line 30)
                      (:file "~/.emacs.d/oops/sos/text7.el" :line 35)
                      (:file "~/.emacs.d/oops/sos/text8.java" :line 40)
                      (:file "~/.emacs.d/oops/sos/text9.c" :line 45)
                      (:file "~/.emacs.d/oops/sos/text10.txt" :line 5)
                      )))))
    (:tips
     (format "Test tips .... %s" (current-time)))))

(provide 'sos-elisp-backend)
