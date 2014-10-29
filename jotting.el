;; (require 'grizzl)
;; (grizzl-completing-read "> " (grizzl-make-index '("/Users/Boy/loopy/lib/python2.7/site-packages/atom/enum.py"
;;                                                   "/Users/Boy/loopy/lib/python2.7/site-packages/atom/intenum.py"
;;                                                   "/Users/Boy/Documents/_CPP/ycmd/cpp/ycm/Utils.cpp"
;;                                                   "/Users/Boy/Documents/_CPP/ycmd/ycmd/completers/general/tests/testdata/filename_completer/test.cpp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq python-indent-offset 2)

;; (require 'auto-complete)
;; (require 'jedi)
;; (add-hook 'python-mode-hook 'auto-complete-mode)
;; (add-hook 'python-mode-hook 'jedi:setup)

;; (jedi:install-server)
;; (jedi:show-setup-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'jedi)
;; (deferred:nextc
;;   (jedi:call-deferred
;;    (case deftype
;;      ((assignment nil) 'goto)
;;      (definition 'get_definition)
;;      (t (error "Unsupported deftype: %s" deftype))))
;;   (lambda (reply)
;;     (jedi:goto-definition--callback reply other-window)))

;; (deferred:$)
;; (deferred:next)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; client ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'epc)
;; (setq epc (epc:start-epc "perl" '("echo-server.pl")))
;; (deferred:$
;;   (epc:call-deferred epc 'echo '(10))
;;   (deferred:nextc it 
;;     (lambda (x) (message "Return : %S" x))))
;; (deferred:$
;;   (epc:call-deferred epc 'add '(10 40))
;;   (deferred:nextc it 
;;     (lambda (x) (message "Return : %S" x))))
;; ;; calling synchronously
;; (message "%S" (epc:call-sync epc 'echo '(10 40)))
;; ;; Request peer's methods
;; (message "%S" (epc:sync epc (epc:query-methods-deferred epc)))
;; (epc:stop-epc epc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'epcs)
;; (let ((connect-function
;;        (lambda (mngr) 
;;          (epc:define-method mngr 'echo (lambda (&rest x) x) "args" "just echo back arguments.")
;;          (epc:define-method mngr 'add '+ "args" "add argument numbers.")))
;;       server-process)
;;   (setq server-process (epcs:server-start connect-function))
;;   (sleep-for 10)
;;   (epcs:server-stop server-process))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deferred ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'deferred)
(deferred:$
  (deferred:next 
    (lambda () (message "deferred start")))
  (deferred:nextc it
    (lambda () 
      (message "chain 1")
      1))
  (deferred:nextc it
    (lambda (x)
      (message "chain 2 : %s" x)))
  (deferred:nextc it
    (lambda ()
      (read-minibuffer "Input a number: ")))
  (deferred:nextc it
    (lambda (x)
      (message "Got the number : %i" x)))
  (deferred:error it
    (lambda (err)
      (message "Wrong input : %s" err))))

