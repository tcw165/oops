(require 'complete)
(completing-read "prompt: " '("111" "222" "333" "444" "555" "666"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'deferred)

(deferred:$
  (deferred:next
    (lambda ()
      (message "deferred start")))
  (deferred:nextc it
    (lambda () 
      (message "chain 1")
      1))
  (deferred:nextc it
    (lambda (x)
      (message "chain 2 : %s" x))))

(deferred:$
  (deferred:wait 1000) ; 1000msec
  (deferred:nextc it
    (lambda (x)
      (message "Timer sample! : %s msec" x))))

(deferred:$
  (deferred:process "ls" "-la")
  (deferred:nextc it
    (lambda (x)
      (insert "\n")
      (insert x))))

(deferred:$
  (deferred:parallel
    (lambda ()
      (deferred:url-get "http://www.google.co.jp/intl/en_com/images/srpr/logo1w.png"))
    (lambda ()
      (deferred:url-get "http://www.google.co.jp/images/srpr/nav_logo14.png")))
  (deferred:nextc it
    (lambda (buffers)
      (loop for i in buffers
            do 
            (insert 
             (format 
              "size: %s\n"
              (with-current-buffer i (length (buffer-string)))))
            (kill-buffer i)))))

(deferred:$
  (deferred:timeout
    1000 "canceled!"
    (deferred:process "sh" "-c" "sleep 3 | echo 'hello!'"))
  (deferred:nextc it
    (lambda (x) (insert x))))

(let ((highlight 'highlight))
  (message "%s" highlight))
(lexical-let ((count 0)
              (anm "-/|\\-")
              (end 60)
              (pos (point))
              (wait-time 50))
  (deferred:$
    (deferred:next
      (lambda (x) (message "Animation started.")))
    (deferred:nextc it
      (deferred:lambda (x)
        (save-excursion
          (when (< 0 count)
            (goto-char pos) (delete-char 1))
          (insert (char-to-string 
                   (aref anm (% count (length anm))))))
        (if (> end (incf count)) ; return nil to stop this loop
            (deferred:nextc (deferred:wait wait-time) self)))) ; return the deferred
    (deferred:nextc it
      (lambda (x)
        (save-excursion
          (goto-char pos) (delete-char 1))
        (message "Animation finished.")))))

(make-deferred :callback (lambda ()
                           (message "test")))
;; [cl-struct-deferred (lambda nil (message "test")) deferred:default-errorback deferred:default-cancel nil nil nil]

(deferred:pack 1 2 3)
;; (1 2 . 3)

;; 111
;;   111
;;     111
;;      111 1111
;;         111
;;           111

(font-lock-add-keywords nil
                        `(((lambda (limit)
                             (catch 'break
                               (while (re-search-forward "111" limit t)
                                 (when (= (line-number-at-pos) 89)
                                   (throw 'break t))
                                 nil)))
                           0 'hl-symbol-face prepend))
                        'append)
(font-lock-fontify-region (point-min) (point-max) t)
;; (font-lock-fontify-buffer)
;; `font-lock-keywords'

/home/boyw165/.emacs.d/oops/whereis-symbol/frontends/ws-default-frontend.el
~/123123???? \/12323.el
c:\home\boyw165\.emacs.d\oops\whereis-symbol\frontends\ws-default-frontend.el
~\font.xxcc
d:\adsfadsfadsf\adsfadsfads\adsfadsfds
~/.emacs.d/oops
;; `ws-candidate-mode'
;; Make number highlighted.
(font-lock-add-keywords nil
                        `(("\\<[0-9]+\\>"
                           0 font-lock-keyword-face append))
                        'append)
;; Unix-liked path.
(font-lock-add-keywords nil
                        `(("~?\\(\/[-a-zA-Z0-9_.\\ *#$%@?|]*\\)+"
                           0 font-lock-keyword-face append))
                        'append)
;; Windows path.
(font-lock-add-keywords nil
                        `(("\\(~\\|[a-zA-Z]:\\)\\(\\\\[-a-zA-Z0-9_. @]*\\)+"
                           0 font-lock-keyword-face append))
                        'append)
(save-excursion
  (font-lock-fontify-region (point-min) (point-max)))

