(require 'oops)

(json-read-from-string "{\"name\":\"Emacs\", \"filepath\":[\"~/.emacs\", \"~/.emacs.d/elpa\", \"~/.emacs.d/etc\"], \"doctypes\":[[\"Text\", \"*.txt;*.md\"], [\"Lisp\", \"*.el\"], [\"Python\", \"*.py\"]]}")

(setq json-object-type 'plist)
(setq json-key-type 'string)
(setq json-array-type 'list)

(let* ((json-object-type 'plist)
       (json-key-type 'string)
       (json-array-type 'list)
       (config (json-read-from-string "{\"name\":\"Emacs\", \"filepath\":[\"~/.emacs\", \"~/.emacs.d/elpa\", \"~/.emacs.d/etc\"], \"doctypes\":[[\"Text\", \"*.txt;*.md\"], [\"Lisp\", \"*.el\"], [\"Python\", \"*.py\"]]}")))
  (format "%s" config))

(json-encode '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))
(json-encode-array '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))
(json-encode-list '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))
(json-encode-plist '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))
(json-encode-alist '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))

