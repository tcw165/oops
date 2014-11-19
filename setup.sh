#!/bin/sh

# Install necessary packages for Emacs.
git submodule update --init --recursive
emacs --batch -q --no-site-file -l oops-install.el
# Replace .emacs with oops version.
[ -e "$HOME/.emacs" ] && rm "$HOME/.emacs"
ln -s "$PWD/.emacs" "$HOME/.emacs"
