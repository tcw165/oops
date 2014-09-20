(start-process "grep" nil (list (current-buffer) nil) t "-nH" "(defun " file)
