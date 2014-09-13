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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

type | line | file
function | 372 | /Users/Boy/.emacs.d/oops/sos/frontends/sos-basic-frontend.el
variable | 12345 | /Users/Boy/.emacs.d/oops/sos/frontends/sos-basic-frontend.el
feature | 1 | /Users/Boy/.emacs.d/oops/sos/frontends/sos-basic-frontend.el


type     | line  | file                                                         
function | 372   | /Users/Boy/.emacs.d/oops/sos/frontends/sos-basic-frontend.el 
variable | 12345 | /Users/Boy/.emacs.d/oops/sos/frontends/sos-basic-frontend.el 
feature  | 1     | /Users/Boy/.emacs.d/oops/sos/frontends/sos-basic-frontend.el 


(align-region 1073 1327
              'entire
              `((sos
                 (regexp . "^\\(\\w\\)+\\s-\|\\s-\\([0-9line]\\)+\\s-\|\\s-\\(.\\)+$")
                 (group . (1 2 3))))
              nil)

(111 . 222)

"A list describing all of the available alignment rules.
The format is:

   ((TITLE
     (ATTRIBUTE . VALUE) ...)
    ...)

The following attributes are meaningful:

`regexp'    This required attribute must be either a string describing
	    a regular expression, or a function (described below).
	    For every line within the section that this regular
	    expression matches, the given rule will be applied to that
	    line.  The exclusion rules denote which part(s) of the
	    line should not be modified; the alignment rules cause the
	    identified whitespace group to be contracted/expanded such
	    that the \"alignment character\" (the character
	    immediately following the identified parenthesis group),
	    occurs in the same column for every line within the
	    alignment section (see `align-region-separate' for a
	    description of how the region is broken up into alignment
	    sections).

	    The `regexp' attribute describes how the text should be
	    treated.  Within this regexp, there must be at least one
	    group of characters (typically whitespace) identified by
	    the special opening and closing parens used in regexp
	    expressions (`\\\\(' and `\\\\)') (see the Emacs manual on
	    the syntax of regular expressions for more info).

	    If `regexp' is a function, it will be called as a
	    replacement for `re-search-forward'.  This means that it
	    should return nil if nothing is found to match the rule,
	    or it should set the match data appropriately, move point
	    to the end of the match, and return the value of point.

`group'     For exclusion rules, the group identifies the range of
	    characters that should be ignored.  For alignment rules,
	    these are the characters that will be deleted/expanded for
	    the purposes of alignment.  The \"alignment character\" is
	    always the first character immediately following this
	    parenthesis group.  This attribute may also be a list of
	    integers, in which case multiple alignment characters will
	    be aligned, with the list of integers identifying the
	    whitespace groups which precede them.  The default for
	    this attribute is 1.

`modes'     The `modes' attribute, if set, should name a list of
	    major modes -- or evaluate to such a value -- in which the
	    rule is valid.  If not set, the rule will apply to all
	    modes.

`case-fold' If `regexp' is an ordinary regular expression string
	    containing alphabetic character, sometimes you may want
	    the search to proceed case-insensitively (for languages
	    that ignore case, such as Pascal for example).  In that
	    case, set `case-fold' to a non-nil value, and the regular
	    expression search will ignore case.  If `regexp' is set to
	    a function, that function must handle the job of ignoring
	    case by itself.

`tab-stop'  If the `tab-stop' attribute is set, and non-nil, the
	    alignment character will always fall on a tab stop
	    (whether it uses tabs to get there or not depends on the
	    value of `indent-tabs-mode').  If the `tab-stop' attribute
	    is set to nil, tab stops will never be used.  Otherwise,
	    the value of `align-to-tab-stop' determines whether or not
	    to align to a tab stop.  The `tab-stop' attribute may also
	    be a list of t or nil values, corresponding to the number
	    of parenthesis groups specified by the `group' attribute.

`repeat'    If the `repeat' attribute is present, and non-nil, the
	    rule will be applied to the line continuously until no
	    further matches are found.

`valid'     If the `valid' attribute is set, it will be used to
	    determine whether the rule should be invoked.  This form
	    is evaluated after the regular expression match has been
	    performed, so that it is possible to use the results of
	    that match to determine whether the alignment should be
	    performed.  The buffer should not be modified during the
	    evaluation of this form.

`run-if'    Like `valid', the `run-if' attribute tests whether the
	    rule should be run at all -- even before any searches are
	    done to determine if the rule applies to the alignment
	    region.  This can save time, since `run-if' will only be
	    run once for each rule.  If it returns nil, the rule will
	    not be attempted.

`column'    For alignment rules, if the `column' attribute is set --
	    which must be an integer, or a symbol whose value is an
	    integer -- it will be used as the column in which to align
	    the alignment character.  If the text on a particular line
	    happens to overrun that column, a single space character,
	    or tab stop (see `align-to-tab-stop') will be added
	    between the last text character and the alignment
	    character.

`spacing'   Alignment rules may also override the amount of spacing
	    that would normally be used by providing a `spacing'
	    attribute.  This must be an integer, or a list of integers
	    corresponding to the number of parenthesis groups matched
	    by the `group' attribute.  If a list of value is used, and
	    any of those values is nil, `align-default-spacing' will
	    be used for that subgroup.  See `align-default-spacing'
	    for more details on spacing, tab stops, and how to
	    indicate how much spacing should be used.  If TAB-STOP is
	    present, it will override the value of `align-to-tab-stop'
	    for that rule.

`justify'   It is possible with `regexp' and `group' to identify a
	    character group that contains more than just whitespace
	    characters.  By default, any non-whitespace characters in
	    that group will also be deleted while aligning the
	    alignment character.  However, if the `justify' attribute
	    is set to a non-nil value, only the initial whitespace
	    characters within that group will be deleted.  This has
	    the effect of right-justifying the characters that remain,
	    and can be used for outdenting or just plain old right-
	    justification.

`separate'  Each rule can define its own section separator, which
	    describes how to identify the separation of \"sections\"
	    within the region to be aligned.  Setting the `separate'
	    attribute overrides the value of `align-region-separate'
	    (see the documentation of that variable for possible
	    values), and any separation argument passed to `align'."


