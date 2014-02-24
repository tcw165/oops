rm /var/tmp/etags-list.txt 1>/dev/null 2>&1
find ~/Documents/_PRJ_CPP/emacs-24.3/lisp -name "*.el" -print > /var/tmp/etags-list.txt
find ~/.emacs.d/ -name "*.el" -print >> /var/tmp/etags-list.txt
cat /var/tmp/etags-list.txt|etags -o ~/.emacs.d/starter-project/starter.TAGS -