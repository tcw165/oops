;; (require 'grizzl)
;; (grizzl-completing-read "> " (grizzl-make-index '("/Users/Boy/loopy/lib/python2.7/site-packages/atom/enum.py"
;;                                                   "/Users/Boy/loopy/lib/python2.7/site-packages/atom/intenum.py"
;;                                                   "/Users/Boy/Documents/_CPP/ycmd/cpp/ycm/Utils.cpp"
;;                                                   "/Users/Boy/Documents/_CPP/ycmd/ycmd/completers/general/tests/testdata/filename_completer/test.cpp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'deferred)

;; `deferred:schedule-worker' use `run-at-time' to schedule works.
;; `deferred:queue' is the task queue.

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
;; (let (it)
;;   (setq it (deferred:next (function (lambda nil (message deferred start)))))
;;   (setq it (deferred:nextc it (function (lambda nil (message chain 1) 1))))
;;   (setq it (deferred:nextc it (function (lambda (x) (message chain 2 : %s x)))))
;;   it)

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

