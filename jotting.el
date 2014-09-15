(require 'oops)
(require 'json)

(json-read-from-string "{\"name\":\"Emacs\", \"filepath\":[\"~/.emacs\", \"~/.emacs.d/elpa\", \"~/.emacs.d/etc\"], \"doctypes\":[[\"Text\", \"*.txt;*.md\"], [\"Lisp\", \"*.el\"], [\"Python\", \"*.py\"]]}")

(aref [1 2 3] 0)
(setq test nil)
(plist-put test "name" "Boy")
(plist-put test "name" "Theresa")

(setq json-object-type 'plist)
(setq json-key-type 'string)
(setq json-array-type 'list)

;; import json.
(let* ((json-object-type 'plist)
       (json-key-type 'string)
       (json-array-type 'list)
       (config (json-read-from-string "{\"name\":\"Emacs\", \"filepath\":[\"~/.emacs\", \"~/.emacs.d/elpa\", \"~/.emacs.d/etc\"], \"doctypes\":[[\"Text\", \"*.txt;*.md\"], [\"Lisp\", \"*.el\"], [\"Python\", \"*.py\"]]}")))
  (prin1 config))
;; export json.
(princ (json-encode-plist '("name" "Emacs"
                            "doctypes" (("Text" "*.txt;*.md") ("Lisp" "*.el") ("Python" "*.py"))
                            "filepath" ("~/.emacs" "~/.emacs.d/elpa" "~/.emacs.d/etc"))))

(json-encode '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))
(json-encode-array '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))
(json-encode-list '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))
(json-encode-plist '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))
(json-encode-alist '(("Text" "*.txt;*.md") ("Lisp" ".emacs;*.el")))

