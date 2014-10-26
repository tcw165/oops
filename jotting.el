;; (require 'grizzl)
;; (grizzl-completing-read "> " (grizzl-make-index '("/Users/Boy/loopy/lib/python2.7/site-packages/atom/enum.py"
;;                                                   "/Users/Boy/loopy/lib/python2.7/site-packages/atom/intenum.py"
;;                                                   "/Users/Boy/Documents/_CPP/ycmd/cpp/ycm/Utils.cpp"
;;                                                   "/Users/Boy/Documents/_CPP/ycmd/ycmd/completers/general/tests/testdata/filename_completer/test.cpp")))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setq python-indent-offset 2)

(require 'auto-complete)
(require 'jedi)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t
      ;; jedi:server-command "/Library/Frameworks/Python.framework/Versions/3.4/lib/python3.4/site-packages//jediepcserver.py"
      )
;; (jedi:start-server)
;; (jedi:install-server)
;; (jedi:show-setup-info)
;; jedi:server-command
;; (getenv "PATH")


