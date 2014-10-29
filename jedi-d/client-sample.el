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

(require 'jedi)
(deferred:nextc
  (jedi:call-deferred
   (case deftype
     ((assignment nil) 'goto)
     (definition 'get_definition)
     (t (error "Unsupported deftype: %s" deftype))))
  (lambda (reply)
    (jedi:goto-definition--callback reply other-window)))

(let ((source      (buffer-substring-no-properties (point-min) (point-max)))
      (line        (count-lines (point-min) (min (1+ (point)) (point-max))))
      (column      (current-column))
      (source-path (jedi:-buffer-file-name)))
  (epc:call-deferred (jedi:get-epc)
                     method-name
                     (list source line column source-path)))

