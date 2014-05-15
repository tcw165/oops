not-release-yet
===============

Add following script in your .emacs file.
===============
(add-to-list 'load-path "~/.emacs.d/oops")
(require 'oops-mode)

;; Init all the features of `oops-mode'.
(oops-mode 1)

TODO List
===============
# Merge `hl-param' and `hl-symb' into it.
# Design a multiple help window framework in "oops-win-mode.el".
# Create a multiple help buffer framework in "oops-help-buffer.el".
# Is this feature a mode or a framework? Maybe use (oops-framework 1) is better.
