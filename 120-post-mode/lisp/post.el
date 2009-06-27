;	$Id: post.el,v 1.10 2008/02/24 23:49:23 rreid Exp $
;; post.el --- Use (X?)Emacs(client) as an external editor for mail and news.
 
;;; Authors: Eric Kidd <eric.kidd@pobox.com>,
;;;          Dave Pearson <davep@davep.org>,
;;;          Rob Reid <rreid@nrao.edu>,
;;;          Roland Rosenfeld <roland@spinnaker.de>

;; This is free software distributed under the GPL, yadda, yadda, yadda.
;; It has no warranty. See the GNU General Public License for more
;; information. Send us your feature requests and patches, and we'll try
;; to integrate everything.

;;; Maintainers: Rob Reid <rreid@nrao.edu> and
;;;              Philip J. Hollenback <philiph@pobox.com>

;;; Keywords: mail

;;; Commentary:
;; This is a major mode for use with mutt (http://www.mutt.org/), slrn, or most
;; email and newsreaders that allow you to use an external editor.
;;
;; Installation:
;;
;; Add the following line to the .emacs(.el) file in your home directory:
;;
;;   (load "/your/local/path/to/this/file/post")
;;
;; Note that you can omit the ".el" from the file name when calling load.
;;
;; If you want to make it available to all your users, type \C-h v
;; load-path RET, pick an appropriate directory for post.el, and modify
;; your sitewide default.el to (require 'post).
;;
;; You may find the latest version of this mode at
;; http://www.cv.nrao.edu/~rreid/software/email/ or possibly
;; http://sourceforge.net/projects/post-mode/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BUGS:
;;
;; Rob: I predict that some buffers (*Original*<2>, *Composing*<2>?)
;; will be left behind if you edit more than one message at a time.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thanks
;;;
;;; Dave Pearson: Code, feature ideas, Mutt experience. Many thanks!
;;; Louis Theran: Encouragement to make Mutt mode work like Emacs MUAs.
;;; Ronald: Enlightening gripes about what Emacs should do, but doesn't.
;;; Robert Napier: Bug reports about font-lock mode, fancy wrapping.
;;; Kevin Rodgers: Answered RR's question on gnu.emacs.help on
;;; overwriting server-process-filter's annoying message at startup.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Revision History
;;;
;;; $Log: post.el,v $
;;; Revision 1.10  2008/02/24 23:49:23  rreid
;;; *** empty log message ***
;;;
;;; Revision 1.9  2008/02/24 23:46:23  rreid
;;; I really hate the way RCS/VC mucks around with the version number and log.
;;; It's not as dwim as it thinks it is.
;;;
;;; Revision 1.8  2008/02/24 23:43:46  rreid
;;; Updated email address.
;;;
;;; Revision 2.402  2008/02/24 23:31:19  rreid
;;; Emacs 22 finally fixed (how-many) to return an int instead of a string,
;;; i.e. 13 instead of '13 occurrences'.  A few people noticed that the change
;;; broke my workaround, but Erik Mugele submitted the winning patch that works
;;; for all emacs versions (I hope).
;;;
;;; This, and the below, are real entries, with real version numbers.  Some of
;;; the above are fake commits to get the version number back in sync.
;;;
;;; Revision 2.401  2004/07/23 16:27:29  rreid
;;; Fixed post-delete-quoted-signatures to not remove sneaky things like quoted
;;; double dash arrows.  Thanks go to Felix Klee for a clear bug report.
;;;
;;; Revision 2.4  2002/04/22 22:04:29  reid
;;; Tweaked post-emoticon-pattern yet again.  Made cl mandatory for all
;;; versions of emacs.  (Thanks to Eric Dorland and Mike Schiraldi for bug
;;; reports.)  Fixed post-unquote-region.  (Thanks to Mike Schiraldi for the
;;; bug report.)
;;;
;;; Revision 2.3  2002/04/21 20:13:55  reid
;;; Improved post-emoticon-pattern.
;;;
;;; Revision 2.2  2002/04/20 04:12:54  reid
;;; Improved post-emoticon-pattern.
;;;
;;; Revision 2.1  2002/04/20 03:17:48  reid
;;; - A major (but not total) synchronization with Dave Pearson's post-mode.
;;;   header-set-followup-to and header-set-organization should work now.
;;; - Syntax highlighting now works for quoted email addresses and URLs.
;;; - *bold* words are now highlighted.
;;; - Emoticons can now be highlighted, and the default regexp,
;;;   post-emoticon-pattern, might be too enthusiastic for your taste.  In case
;;;   you're curious, I verified that gnus' smiley-ems.el works with post, but I
;;;   decided that it wasn't ideal.
;;; - post-url-text-pattern changed to post-url-pattern and made more enthusiastic.
;;;
;;; revision 1.95 2002/04/10 00:06:26 reid
;;; Fixed the regexp in post-kill-signature to not delete everything between
;;; mutt's standard forwarding lines.  post-kill-signature is called indirectly
;;; by many functions.
;;;
;;; Revision 1.9  2002/04/04 22:24:31  reid
;;; Applied a patch (not quite verbatim) from The Anarcat
;;; <anarcat@anarcat.dyndns.org> to make the entity separating siglets in
;;; `post-variable-signature-source' a regexp, `post-signature-sep-regexp'.  The
;;; default works either either the old post file format or strfiled (fortune)
;;; files.
;;;;
;;; Changed default `post-random-signature-command' to `fortune
;;; ~/.mutt/sigs.fortune'.
;;;
;;; `post-random-signature-command' should now NOT supply a fixed sig portion!
;;;
;;; (post-el-random-signature) supplied by The Anarcat to do random sig
;;; selection purely within Emacs Lisp.
;;;
;;; Revision 1.8  2002/02/06 22:24:31  eric
;;; clean up
;;;
;;; Revision 1.7.2  2002/02/06 22:17:01  eric
;;; tweak regexps, make font-lock-comment-face be post-signature-text-face
;;;
;;; Revision 1.7.1  2002/02/06 21:58:58  eric
;;; tweak regexp, change some types to regexp
;;;
;;; Revision 1.7.0  2002/02/06 21:36:56  eric
;;; hilight signatures, urls and emails
;;;
;;; Revision 1.6.3.10  1999/10/11 00:29:41  roland
;;; Corrected color quoting again: Now allows ">" in the middle of
;;; a line which is quoted twice.
;;;
;;; Revision 1.6.3.9  1999/10/08 10:43:18  roland
;;; Add third level of quoting faces.
;;; Allow super-cite name prefixes before quote signs.
;;;
;;; Revision 1.6.3.8  1999/10/08 08:39:00  roland
;;; post-font-lock-keywords now detects lines with only "> "in it
;;; correctly (merged following line into it before).
;;;
;;; Revision 1.6.3.7  1999/10/04 10:07:48  roland
;;; Add post-quote-region and post-unquote-region commands to quote and
;;; unquote a region (one level).
;;;
;;; Revision 1.6.3.6  1999/09/03 23:13:55  reid
;;; Valeriy E. Ushakov <uwe@ptc.spbu.ru> pointed out that (GNU) Emacs <20 has
;;; fewer (optional) arguments to (read-string) than what I was using to
;;; inherit the input method.  I didn't find a way off the top of my head
;;; to redefine (read-string) without causing an infinite loop, so I have
;;; substituted a macro (string-read prompt) which does the right thing,
;;; so please use it instead of read-string.
;;;
;;; Revision 1.6.3.5  1999/08/29 19:58:49  reid
;;; Changed default post-mail-message to handle hostnames with digits.
;;; Thanks to Brian D. Winters <brianw@alumni.caltech.edu>.
;;;
;;; Revision 1.6.3.4  1999/03/20 03:02:05  reid
;;; Made post compatible with emacs as far back as 19.28.1, probably
;;; farther.
;;;
;;; Revision 1.6.3.3  1999/03/16 03:14:07  reid
;;; Cleaned up post-select-signature-select-sig-from-file code.
;;;
;;; Revision 1.6.3.2  1999/03/16 03:05:12  reid
;;; Fixed alist updating.
;;;
;;; Revision 1.6.3.1  1999/03/13 02:23:48  reid
;;; Added defface to the list of things that get defined if customize
;;; hasn't already done it.  Thanks to Melissa Binde for the bug report.
;;;
;;; Modified post-body-says-attach to use a regexp,
;;; post-attachment-regexp, so that something like "\(attach\|anbringen\)"
;;; can be used by bilingual people like Roland.
;;;
;;; Revision 1.6.2.1  1999/03/12 10:16:11  roland
;;; Added missing () to post-insert-to-auto-mode-alist-on-load.
;;;
;;; Revision 1.6.2 1999/03/11 15:51 Dave Pearson
;;; header-position-on-value fixed to return (point), and
;;; defcustom macro provided for Emacs 19 users.
;;;
;;; Revision 1.6.1.2  1999/03/06 11:24:43  roland
;;; Added post-insert-to-auto-mode-alist-on-load.
;;;
;;; Revision 1.6.1.1  1999/03/06 11:02:27  roland
;;; Customized renaming of buffer.
;;; Removed different handling for mail, news, news-reply.
;;; Fixed problems with easy-menu under XEmacs.
;;;
;;; Revision 1.6.0 1999/03/04 18:04 Rob Reid
;;; Returned post-signature-pattern to using "--" instead of "-- "
;;; because some senders have broken MTAs (as Eric reminded me) and
;;; some users don't use procmail to compensate.  This time all of the
;;; functions dealing with signatures have been smartened up to avoid
;;; false matches.  Unfortunately that means they don't use
;;; post-signature-pattern in its raw form.
;;;
;;; Added post-backup-original so that Dave's post-copy-original can
;;; be used.
;;;
;;; Kevin Rodgers explained how to put this in .emacs to fix the
;;; server-process-filter's annoying message problem:
;;;
;;; Revision 1.1  1999/03/04 18:02:30  reid
;;; Initial revision
;;;
;;; %%%%%%%%%%%% Put in .emacs %%%%%%%%%%%
;;;
;;; ;;; Email
;;; (server-start)
;;; (load "/home/reid/.mutt/post")
;;; (defadvice server-process-filter (after post-mode-message first activate)
;;;    "If the buffer is in post mode, overwrite the server-edit
;;;    message with a post-save-current-buffer-and-exit message."
;;;    (if (eq major-mode 'post-mode)
;;;        (message
;;;         (substitute-command-keys "Type \\[describe-mode] for help composing; \\[post-save-current-buffer-and-exit] when done."))))
;;; ; This is also needed to see the magic message.  Set to a higher
;;; ; number if you have a faster computer or read slower than me.
;;; '(font-lock-verbose 1000)
;;; ; (setq server-temp-file-regexp "mutt-")
;;; (add-hook 'server-switch-hook
;;;         (function (lambda()
;;;                     (cond ((string-match "Post" mode-name)
;;;                            (post-goto-body))))))
;;;
;;; %%%%%%%%% We now return to our regular commentary %%%%%%%%%
;;;
;;; Eric Kidd asked that the name of Headers mode be changed so that
;;; it doesn't conflict with mutt-mode's Headers, so I changed it to
;;; just Header (no s).
;;;
;;; Revision 1.5? 1999/02/27 17:30 Rob Reid
;;; I had a go at combining Dave Pearson's post mode with Eric Kidd's
;;; Mutt mode.  Since Dave Pearson's post mode explicitly handles news as
;;; well as email, and this should be useful for more than just mutt,
;;; I'm calling it post mode.  I also added functions for picking
;;; random signatures, selecting a signature from a file, and
;;; intelligently (IMHO) prompting the user for an attachment when
;;; necessary.  Changed mutt-save-buffer-and-exit to work better with
;;; emacsclient, and some of the key bindings.  post-signature-pattern
;;; now defaults to use "-- " instead of "--", and I have far less
;;; trouble this way (I use procmail to clean up braindead "--"s.).  I
;;; don't know why Eric warned against trailing whitespace.
;;;
;;; Revision 1.4  1998/04/11 00:05:46  emk
;;; Fixed font-lock bug. Also made mutt-mode a little more careful about
;;; saving various bits of Emacs state when moving around the buffer.
;;;
;;; Revision 1.3  1998/03/25 00:37:36  emk
;;; Added support for menus and font-lock mode, plus a few bug fixes.
;;;
;;; Revision 1.2  1998/03/24 13:19:46  emk
;;; Major overhaul--more commands, a minor mode for header editing, and other
;;; desirable features. Attaching files seems to be broken, though.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Required Packages

(require 'cl)
(require 'derived)
(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customization Support
;;;
;;; Set up our customizable features. You can edit these (and lots of other
;;; fun stuff) by typing M-x customize RET. The Post preferences can be
;;; found under the [Applications] [Mail] category.

;; Make post mode a bit more compatible with older (i.e. <20) versions of emacs.
;;; Code:
(eval-and-compile
  ;; Dumb down read-string if necessary.
  ;; The number of optional arguments for read-string seems to increase
  ;; sharply with (emacs-version).  Since old versions of emacs are a large
  ;; source of bug reports it might be worth writing (or looking for)
  ;; (bug-report reid@astro.utoronto.ca) which emails me the result of
  ;; (emacs-version) along with a user supplied description of the problem.
  ;; GNU Emacs 19.28.1 only has INITIAL-STRING as an optional argument.
  ;; 19.34.1 has (read-string PROMPT &optional INITIAL-INPUT HISTORY).  20.2.1
  ;; has (read-string PROMPT &optional INITIAL-INPUT HISTORY DEFAULT-VALUE
  ;; INHERIT-INPUT-METHOD).
  ;; Since I haven't found a way of redefining read-string without causing an
  ;; infinite loop, please use (string-read prompt).
  (if (< (string-to-number (substring (emacs-version)
				      (string-match "[0-9]+\.[0-9]"
					 (emacs-version) 5))) 20)
      (defmacro string-read (prompt) (` (read-string (, prompt))))
      (defmacro string-read (prompt)
	(` (read-string (, prompt) nil nil nil t))))

  ;; XEmacs gnuserv uses slightly different functions than the GNU Emacs
  ;; server, and some people are still wasting time and CPU cycles by starting
  ;; up a new emacs each time.
  (fset 'post-finish (cond ((fboundp 'server-edit)
			    'server-edit)
			   ((fboundp 'gnuserv-kill-buffer-function)
			    'gnuserv-kill-buffer-function)
			   (t
			    'save-buffers-kill-emacs)))
;;   (cond ((fboundp 'server-edit)
;; 	 (fset 'post-finish 'server-edit))
;; 	((fboundp 'gnuserv-kill-buffer-function)
;; 	 (fset 'post-finish 'gnuserv-kill-buffer-function))
;; 	(t
;; 	 (fset 'post-finish 'save-buffers-kill-emacs)))
   
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      ; The "extra" braces and whitespace are for emacs < 19.29.
      (` (defvar (, symbol) (, init) (, docstring))))
    (defmacro defface (&rest args) nil))
  (unless (fboundp 'buffer-substring-no-properties)
    (fset 'buffer-substring-no-properties 'buffer-substring)))

(defgroup post nil
  "Composing e-mail messages with Post.
Emacs can run as an external editor for Mutt, the spiffy Unix mail reader
du jour, or slrn, the spiffy Unix news reader du jour.  You can get
Mutt from http://www.mutt.org/."
  :group 'mail)

(defcustom post-uses-fill-mode t
  "*Specifies whether Post should automatically wrap lines.
Set this to t to enable line wrapping, and nil to disable line
wrapping.  Note that if a paragraph gets messed up (the line wrapper
is very primitive), you can type \\[fill-paragraph] to rewrap the paragraph."
  :type 'boolean
  :group 'post)

(defcustom post-mail-message "mutt-[a-z0-9]+-[0-9]+-[0-9]+.*\\'"
  "*Regular expression which matches your mailer's temporary files."
  :type 'string
  :group 'post)

(defcustom post-news-posting "\\.\\(followup\\|letter\\|article\\)$"
  "*Regular expression which matches your news reader's composition files."
  :type 'string
  :group 'post)

(defcustom post-backup-original nil
  "*Controls whether a pristine backup of the original is kept for reference."
  :type 'boolean
  :group 'post)

(defcustom post-signature-pattern "\\(--\\|Cheers,\\|\\)"
  "*Pattern signifying the beginning of signatures.
It should not contain trailing whitespace unless you know what you're doing."
  :type 'regexp
  :group 'post)

(defcustom post-signature-sep-regexp "^\\(%\\|^L\\|--\\)?\n"
  "*Regular expression delimiting signatures in the signature file.
This allows the use of classic fortune files as signature files.
This should normally contain a newline."
  :type 'regexp
  :group 'post)

(defcustom post-signature-source-is-file t
  "*Toggles the signature source type between file and directory."
  :type 'boolean
  :group 'post)

(defcustom post-variable-signature-source "~/.mutt/sigs.fortune"
  "*Location of the variable part of your signature.
Post uses this to locate signatures.  It can be either a directory
with one item per file or a file with items separated by blank lines."
  :type 'string
  :group 'post)

(defcustom post-fixed-signature-source "~/.fixedsig"
  "*File with the fixed part of your signature."
  :type 'string
  :group 'post)

(defcustom post-signature-directory "~/.sigs/"
  "*The directory that contains your collection of signature files."
  :type 'string
  :group 'post)

(defcustom post-signature-wildcard "sig*"
  "*Wildcard for finding signature files in your signature directory."
  :type 'string
  :group 'post)

(defcustom post-random-signature-command "fortune ~/.mutt/sigs.fortune"
  "*Command to run to get a random signature.
Examples are available at http://astro.utoronto.ca/~reid/mutt/"
  :type 'string
  :group 'post)

(defcustom post-kill-quoted-sig t
  "Specifies whether `post-mode' should automatically kill quoted signatures."
  :type 'boolean
  :group 'post)

(defcustom post-jump-header t
  "Specifies wheather `post-mode' should jump to the body."
  :type 'boolean
  :group 'post)

(defcustom post-force-pwd-to-home t
  "Specifies whether `post-mode' should cd to your home directory."
  :type 'boolean
  :group 'post)

(defcustom post-email-address (concat (user-login-name) "@" mail-host-address)
  "*Your email address."
  :type 'string
  :group 'post)

(defcustom post-should-prompt-for-attachment 'Smart
  "*Controls whether an attachment will be prompted for before saving
the message and exiting.  'Smart' will prompt only if the body
contains post-attachment-regexp."
  :type '(choice (const Never)
		 (const Smart)
		 (const Always))
  :group 'post)

(defcustom post-attachment-regexp "attach"
  "*This is what post looks for in the body if
post-should-prompt-for-attachment is 'Smart'."
  :type 'regexp
  :group 'post)

(defcustom post-news-poster-regexp "^On .*<.*>.*wrote:$"
  "Regular expression used to locate the attribution line of a news posting."
  :type 'regexp
  :group 'post)

(defcustom post-rename-buffer t
  "Specify whether `post-mode' should rename the buffer to *Composing*."
  :type 'boolean
  :group 'post)

(defcustom post-insert-to-auto-mode-alist-on-load t
  "Automatically insert `post-mode' with `post-mail-message' to `auto-mode-alist'."
  :type 'boolean
  :group 'post)

(defcustom post-mode-hook nil
  "List of hooks to be executed on entry to `post-mode'."
  :group 'post)

(defcustom post-quote-start "> "
  "Pattern which is added (or removed) at the beginning of the line by
comment-region"
  :group 'post)

(defcustom post-email-address-pattern
  "[A-Za-z0-9_][-A-Za-z0-9._]*@[-A-Za-z0-9._]*[A-Za-z0-9]"
  "Pattern to detect email addresses."
  :type 'regexp
  :group 'post)

(defcustom post-url-pattern
  '("\\<\\(\\(https?\\|news\\|mailto\\|ftp\\|gopher\\):\\|\\(www\\|ftp\\)\\.\\)[-~A-Za-z0-9._/%$+?#]+[A-Za-z0-9/#]" "<URL:[^ ]+>")
  "Pattern to detect URL addresses."
  :type '(repeat regexp)
  :group 'post)

(defcustom post-bold-pattern '("\\*\\w+\\*")
  "*List of regular expressions that define bold text."
  :type '(repeat regexp)
  :group 'post)

(defcustom post-underline-pattern '("_\\w+_")
  "*List of regular expressions that define underlined text."
  :type '(repeat regexp)
  :group 'post)

(defcustom post-emoticon-pattern '("[0O(<{}]?[;:8B|][.,]?[-+^*o0O][{<>/\|]?[][)>(<|/\P][)>]?"
			"\\s [(<]?[][)>(<|/\][}<>|]?[-+^*oO0][,.]?[:8][0O>]?"
			"\\s [;:][][P)\/(]" "\\s [][)(P\/][:;]"
				   "<[Gg]>" "<[BbSs][Gg]>")
  "*List of regular expressions that define a emoticon."
  :type '(repeat regexp)
  :group 'post)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable Faces
;;; If you find a more attractive color scheme for dark backgrounds, please
;;; email it to reid@astro.utoronto.

(defgroup post-faces nil
  "Typefaces used for composing messages with Post."
  :group 'post
  :group 'faces)

(defface post-header-keyword-face
  '((((class color)
      (background light))
     (:foreground "Navy" :bold t))
    (((class color)
      (background dark))
     (:foreground "LightBlue" :bold t))
    (t
     (:bold t)))
  "Face used for displaying keywords (e.g. \"From:\") in header."
  :group 'post-faces)

(defface post-header-value-face
  '((((class color)
      (background light))
     (:foreground "MidnightBlue"))
    (((class color)
      (background dark))
     (:foreground "LightSteelBlue")))
  "Face used for displaying the values of header."
  :group 'post-faces)

(defface post-quoted-text-face
  '((((class color)
      (background light))
     (:foreground "Sienna" :italic t))
    (((class color)
      (background dark))
     (:foreground "Wheat" :italic t))
    (t
     (:bold t :italic t)))
  "Face used for displaying text which has been quoted (e.g. \">foo\")."
  :group 'post-faces)

(defface post-double-quoted-text-face
  '((((class color)
      (background light))
     (:foreground "Firebrick" :italic t))
    (((class color)
      (background dark))
     (:foreground "Tan" :italic t))
    (t
     (:italic t)))
  "Face used for text which has been quoted twice (e.g. \">>foo\")."
  :group 'post-faces)

(defface post-multiply-quoted-text-face
  '((((class color)
      (background light))
     (:foreground "goldenrod" :italic t))
    (((class color)
      (background dark))
     (:foreground "tan3" :italic t))
    (t
     (:italic t)))
  "Face used for text which has been quoted more than twice (e.g. \">>>foo\")."
  :group 'post-faces)

(defface post-signature-text-face
  '((((class color)
      (background light))
     (:foreground "red3"))
    (((class color)
      (background dark))
     (:foreground "red1"))
    (t
     (:bold t)))
  "Face used for text that is part of a signature"
  :group 'post-faces)

(defface post-email-address-text-face
  '((((class color)
      (background light))
     (:foreground "green3"))
    (((class color)
      (background dark))
     (:foreground "green1"))
    (t
     (:italic t)))
  "Face used for email addresses"
  :group 'post-faces)

(defface post-url-face
  '((((class color)
      (background light))
     (:foreground "green3" :bold t))
    (((class color)
      (background dark))
     (:foreground "green1" :bold t))
    (t
     (:italic t)))
  "Face used for URL addresses"
  :group 'post-faces)

(defface post-emoticon-face
  '((((class color)
      (background light))
     (:foreground "black" :background "yellow" :bold t))
    (((class color)
      (background dark))
     (:foreground "black" :background "yellow" :bold t))
    (t
     (:bold t)))
  "Face used for text matched by post-emoticon-pattern."
  :group 'post-faces)

(defface post-bold-face
  '((((class color)
      (background light))
     (:bold t))
    (((class color)
      (background dark))
     (:bold t))
    (t
     (:bold t)))
  "Face used for text matching post-bold-pattern."
  :group 'post-faces)

(defface post-underline-face
  '((((class color)
      (background light))
     (:underline t))
    (((class color)
      (background dark))
     (:underline t))
    (t
     (:underline t)))
  "Face used for text matching post-underline-pattern."
  :group 'post-faces)

; Note: some faces are added later!
(defvar post-font-lock-keywords
  `(("^\\([A-Z][-A-Za-z0-9.]+:\\)\\(.*\\)$"
     (1 'post-header-keyword-face)
     (2 'post-header-value-face))
    ("^[ \t\f]*\\(>[ \t\f]*\\)\\([-a-zA-Z]*>[ \t\f]*\\)\\([-a-zA-Z]*>.*\\)$"
     (1 'post-quoted-text-face)
     (2 'post-double-quoted-text-face)
     (3 'post-multiply-quoted-text-face))
    ("^[ \t\f]*\\(>[ \t\f]*\\)\\([-a-zA-Z]*>.*\\)$"
     (1 'post-quoted-text-face)
     (2 'post-double-quoted-text-face))
    ("^[ \t\f]*\\(>[ \t\f]*[^ \t\f\n>].*\\)$"
     (1 'post-quoted-text-face))
    ("^[ \t\f]*\\(>[ \t\f]*\\)$"
     (1 'post-quoted-text-face))
	(,post-email-address-pattern
	 (0 'post-email-address-text-face)))
  "Highlighting rules for message mode.")

;;; Declare global mode variables.

(defconst post-font-lock-syntactic-keywords
  `((,(concat "^" post-signature-pattern "[ \t\f]*$") 0 '(11))))

(defun post-font-lock-syntactic-face-function (state)
  "Function for font locking syntactic faces.
Argument STATE ."
post-signature-text-face)

(defvar post-buf nil
  "Name of the composing buffer.")

(defvar post-select-signature-mode-map nil
  "Local keymap for the select-signature buffer.")

(defvar post-select-signature-last-buffer nil
  "Pointer to the calling buffer.")

(defvar post-select-signature-last-point nil
  "Where we were in the calling buffer.")

(defvar post-has-attachment nil
 "Whether the message has an attachment.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interactive Commands

(defun post-save-current-buffer-and-exit ()
  "Save the current buffer and exit Emacs."
  (interactive)

  ;; Should the user be prompted for an attachment?
  (cond (post-has-attachment)
	((equal post-should-prompt-for-attachment 'Never))
	((or (equal post-should-prompt-for-attachment 'Always)
	     (post-body-says-attach-p))
	 (post-prompt-for-attachment)))

  (basic-save-buffer)

  (if post-backup-original
      (kill-buffer "*Original*"))

  (post-finish)

  ;; Added by Rob Reid 10/13/1998 to prevent accumulating *Composing* buffers
  ;; when using (emacs|gnu)client.  Helped by Eric Marsden's Eliza example in
  ;; http://www.ssc.com/lg/issue29/marsden.html
;  (kill-buffer post-buf)
)

(defun post-goto-body ()
  "Go to the beginning of the message body."
  (interactive)
  (goto-char (point-min))
  ;; If the message has header, slide downward.
  (and header-mode (save-match-data (re-search-forward "^$" nil t))
       (next-line 1)))

(defun post-goto-signature ()
  "Go to the beginning of the message signature."
  (interactive)
  (goto-char (point-max))
  (and (save-match-data
	 (re-search-backward (concat "^" post-signature-pattern
				     "[ \t\f]*$")
			     nil t))))

(defun post-delete-quoted-signatures ()
  "Delete quoted signatures from buffer."
  (interactive)
  (goto-char (point-min))
  (flush-lines (concat "^\\([ \t\f]*>[ \t\f>]*\\)"
		       post-signature-pattern
		       "[ \t\f]*\\(\n\\1.*\\)+")))

(defun post-kill-signature ()
  "Kill the signature from the buffer.
Returns the point value for where the signature was or, if there isn't a
signature, the point value of the end of the buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
; The .=*+|#@!~$%&()_- is to compensate for people who put ASCII art on the
; same line as the sigdashes, and the $ at the end prevents this from deleting
; everything between mutt's standard forwarding lines.
    (cond ((search-forward-regexp (concat "^" post-signature-pattern
					  "[ \t\f.=*+|#@!~$%&()_-]*$") nil t)
	   (beginning-of-line)
	   (kill-region (point) (point-max)))
	  (t
	   (goto-char (point-max))))
    (point)))

(defun post-delete-old-citations ()
  "Delete citations more than one level deep from buffer."
  (interactive)
  (goto-char (point-min))
  (flush-lines "^[ \t\f]*>[ \t\f]*>[ \t\f>]*"))

;;; Functions for messing with the body

(defun post-make-region-bold (start end)
  "Apply mutt's nroff style bold to a region of text.
Argument START start of region.
Argument END end of region."
  (interactive "r")
  (while (< start end)
    (goto-char start)
    (insert (buffer-substring-no-properties start (1+ start)))
    (insert (char-to-string 8))
    (setq start (+ start 3))
    (setq end   (+ end   2))))

(defun post-make-region-underlined (start end)
  "Apply mutt's nroff style underline to a region of text.
Argument START start of region.
Argument END end of region."
  (interactive "r")
  (while (< start end)
    (goto-char start)
    (insert "_")
    (insert (char-to-string 8))
    (setq start (+ start 3))
    (setq end   (+ end   2))))

(defun post-quote-region (beg end)
  "Quote a region using the `post-quote-start' variable.
Argument BEG Beginning of region to be quoted.
Argument END End of region to be quoted."
  (interactive "r")
  (comment-region beg end))

(defun post-unquote-region (beg end)
  "Un-quote a region one level using the `post-quote-start' variable.
Argument BEG Beginning of region to be quoted.
Argument END End of region to be quoted."
  (interactive "r")
  (uncomment-region beg end))

; From Dave Pearson, July 15, 2000
(defun* split-quoted-paragraph (&optional (quote-string "> "))
  "Split a quoted paragraph at point, keeping the quote."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at (regexp-quote quote-string)))
      (progn
        (let ((spaces (- (point)
                         (save-excursion
                           (beginning-of-line)
                           (point))
                         (length quote-string))))
          (save-excursion
            (insert (format "\n\n%s%s" quote-string (make-string spaces ? ))))))
    (error "Can't see a quoted paragraph here")))

(defun post-random-signature ()
  "Randomize the signature.
Set it to whatever `post-random-signature-command' spits out followed by the
content of `post-fixed-signature-source', if available, or a nasty reminder if
it is not."
  (interactive)
  (save-excursion
    (goto-char (post-kill-signature))
    (insert "-- \n")
    (shell-command post-random-signature-command t)
    (goto-char (point-max))
    (if (file-readable-p post-fixed-signature-source)
	(insert-file-contents post-fixed-signature-source)
      (insert "I really need a `post-fixed-signature-source'!\n"))))

(defun post-el-random-signature ()
  "Choose a random signature from `post-variable-signature-source'.
the signatures in `post-variable-signature-source' must be separated by
`post-signature-sep-regexp'."
  (interactive)
  (let ((sig nil))
    (save-excursion
      (set-buffer (generate-new-buffer "*Post-Select-Signature*"))
      (insert-file post-variable-signature-source)
      (beginning-of-buffer)
      ;; we have 2 lists of marks since seperators are of arbitrary lenght
      (let ((marks-st (list (point-min)))
	    (marks-end (list))
	    (count 0))          ;nth counts from zero and random is [0,N)
	(while (search-forward-regexp post-signature-sep-regexp nil "a")
	  (setq marks-st (cons (match-end 0) marks-st)
		marks-end (cons (match-beginning 0) marks-end)
		count (1+ count)))
	(setq marks-end (cons (point-max) marks-end))
	(let ((r (random (1+ count))))
	  (setq sig (buffer-substring-no-properties
		     (nth r marks-st) (nth r marks-end))))
	(kill-buffer (current-buffer)))
      (goto-char (post-kill-signature))
      (insert-string "-- \n")
      (insert sig)
      (if (file-readable-p post-fixed-signature-source)
	  (insert-file-contents post-fixed-signature-source)
	(insert "I really need a `post-fixed-signature-source'!\n")))))

(defun post-select-signature-from-file ()
  "*Interactively select a signature from `post-variable-signature-source'."
  (interactive)
  (setq post-select-signature-last-buffer (current-buffer))
  (setq post-select-signature-last-point (point))
  (pop-to-buffer "*Post-Select-Signature*")
  (insert-file post-variable-signature-source)
  (use-local-map post-select-signature-mode-map))

(defun post-select-signature-select-sig-from-file ()
 "*Chooses the signature the cursor is in from `post-variable-signature-source'."
  (interactive)

  ;; These 2 lines select whatever siglet the cursor is sitting in,
  ;; making it nifty to C-s "word" then C-m (or whatever this is
  ;; bound to).
  (let ((sig-start (point))
        (sig-end (point)))

    (cond ((setq sig-start (search-backward-regexp post-signature-sep-regexp
						   nil "a"))
	   (forward-line 1)
	   (setq sig-start (point))))

    (if (search-forward-regexp post-signature-sep-regexp nil "a")
	(setq sig-end (match-beginning 0))
      (setq sig-end (point-max)))

    (let ((sig (buffer-substring-no-properties sig-start sig-end)))
      (switch-to-buffer post-select-signature-last-buffer)
      (goto-char (post-kill-signature))
      (insert-string "-- \n")
      (insert sig))
    (if (file-readable-p post-fixed-signature-source)
	(insert-file-contents post-fixed-signature-source))
    (post-select-signature-quit)))

(defun post-select-signature-from-dir ()
  "Select a new signature for an email/post in the current buffer."
  (interactive)
  (setq post-select-signature-last-buffer (current-buffer))
  (setq post-select-signature-last-point (point))
  (pop-to-buffer "*Post-Select-Signature*")
  (list-directory (concat post-signature-directory
                          post-signature-wildcard) t)
  (pop-to-buffer "*Directory*")
  (next-line 1)
  (copy-to-buffer "*Post-Select-Signature*" (point) (point-max))
  (kill-buffer "*Directory*")
  (pop-to-buffer "*Post-Select-Signature*")
  (use-local-map post-select-signature-mode-map)
  (toggle-read-only t))

(defun post-select-signature-select-sig-from-dir ()
  "Set the signature in the calling buffer to the one under the cursor."
  (interactive)
  (let ((sig-start   nil)
        (sig-to-load nil))
    (end-of-line)
    (search-backward " ")
    (forward-char)
    (setq sig-start (point))
    (end-of-line)
    (setq sig-to-load (buffer-substring-no-properties sig-start (point)))
    (switch-to-buffer post-select-signature-last-buffer)
    (goto-char (post-kill-signature))
    (insert-string "-- \n")
    (insert-file (concat post-signature-directory sig-to-load))
    (message "Signature set to %s%s" post-signature-directory sig-to-load)
    (post-select-signature-quit)))

(defun post-select-signature-quit ()
  "Kill the *Post-Select-Signature* frame."
  (interactive)
  (kill-buffer "*Post-Select-Signature*")
  (switch-to-buffer post-select-signature-last-buffer)
  (goto-char post-select-signature-last-point)
  (delete-other-windows))

(defun post-attach-file ()
  "Prompt for an attachment."
  (interactive)
  (let ((file (read-file-name "Attach file: " nil nil t nil))
	(description (string-read "Description: ")))
    (header-attach-file file description)))

;;; Non-interactive functions

(defun post-prompt-for-attachment ()
  "Prompt for an attachment."
   (if (y-or-n-p "Do you want to attach anything? ")
       (post-attach-file)))

(defun post-references-p ()
  "Is there a References header in this buffer?"
  (save-excursion
    (goto-char (point-min))
    (looking-at "^References: ")))

(defun post-body-says-attach-p ()
  "Check if attach appears in the body."
  (post-goto-body)
  
  ;; Aargh it's annoying that how-many returns a string,
  ;; "13 occurences" instead of a number, 13.
  ;; As of Emacs 22 how-many returns an integer number.  Consideration
  ;; must be taken for both newer and older versions.
  (let ((total-attach (how-many post-attachment-regexp)))
    (if (string= (type-of total-attach) 'string)
        (setq total-attach (string-to-number (how-many post-attachment-regexp))))
    ;; And this mess is just to catch the unlikely false alarm of
    ;; "attach" being in the signature, but not in the body.
    (if (> total-attach 0)
	    (progn (post-goto-signature)
		   (if (string= (type-of (how-many post-attachment-regexp)) 'string)
		       (> total-attach (string-to-number (how-many
						       post-attachment-regexp)))
		     (> total-attach (how-many
				      post-attachment-regexp)))))))

(defun post-ask-for-address-with-default (header)
  "Prompt for an email address, showing default.
Argument HEADER the header type."
  (let ((default (if (= (length (post-get-header-value header)) 0)
                     post-email-address
                   (post-get-header-value header))))
    (read-string (concat header ": ") default)))

; From davep@davep.org.  RR hasn't tested it.
(defun post-get-header-value (header)
  "Get the value of a specific mail HEADER."
  (save-excursion
    (let ((value          "")
          (start-of-value nil))
      (setf (point) (point-min))
      (when (post-find-header-line header)
        (setq start-of-value (point))
        (end-of-line)
        (setq value (buffer-substring-no-properties start-of-value (point))))
      value)))

;;; From davep@davep.org.  RR hasn't tested it.
(defun post-find-header-line (header)
  "Find a HEADER line in the header."
  (let ((old-point (point))
        (end-of-header nil)
        (found-point nil))
    (setf (point) (point-min))
    (search-forward-regexp "^$" nil t)
    (setq end-of-header (point))
    (setf (point) (point-min))
    (cond ((search-forward-regexp (concat "^" header ": ") nil t)
           (cond ((< (point) end-of-header)
                  (setq found-point (point)))
                 (t
                  (setf (point) old-point))))
          (t
           (setf (point) old-point)))
    found-point))

;;; Function to make a backup buffer for viewing the original.
(defun post-copy-original ()
  "Make a copy of the `post-mode' buffer before any editing by the user.
This way they can refer back to this buffer during a compose session."
  (copy-to-buffer (get-buffer-create "*Original*")
		  (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Heart of Darkness
;;;
;;; The old post mode (i.e. Dave Pearson's) derived from mail-mode.  I
;;; prefer deriving from text mode like mutt mode did. - RR
(define-derived-mode post-mode text-mode "Post"
  "Major mode for composing email or news with an external agent.
To customize it, type \\[customize] and select [Applications] [Mail] [Post].
When you finish editing this message, type \\[post-save-current-buffer-and-exit] to save and exit Emacs.

\\{post-mode-map}"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Neat things to do right off the bat.

  (auto-fill-mode (if post-uses-fill-mode 1 0))

  (if post-backup-original (post-copy-original))

  ;; Make Emacs smarter about wrapping citations and paragraphs.
  ;; We probably can't handle Supercited messages, though.
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start
	"\\([ \t\n\f]+[^ \t\n\f>]\\|[ \t\f>]*$\\)"
	paragraph-separate
	"[ \t\f>]*$")

  ;; XEmacs needs easy-menu-add, Emacs does not care
  (easy-menu-add post-mode-menu)

  ;; If headers were passed, activate the necessary commands.
  (when (looking-at "^[-A-Za-z0-9]+:")
    (header-mode 1))

  ;; Our temporary file lives in /tmp. Yuck! Compensate appropriately.
  (make-local-variable 'backup-inhibited)
  (setq backup-inhibited t)

  (if (boundp 'font-lock-defaults)
      (make-local-variable 'font-lock-defaults))
  (flet ((add-syntax-highlight (face regexps)
	    (set face face)
	    (nconc post-font-lock-keywords
		   (loop for regexp in regexps
			 collect (list regexp (list 0 face 't))))))
;			 collect (list regexp `(,0 ',face))))))
    (add-syntax-highlight 'post-emoticon-face post-emoticon-pattern)
    (add-syntax-highlight 'post-bold-face   post-bold-pattern)
    (add-syntax-highlight 'post-underline-face   post-underline-pattern)
    (add-syntax-highlight 'post-url-face    post-url-pattern))
  (setq font-lock-defaults
	'(post-font-lock-keywords nil nil nil nil
				  (font-lock-syntactic-keywords
				   . post-font-lock-syntactic-keywords)
				  (font-lock-comment-face
;				   . 'post-signature-text-face)))
				   . post-signature-text-face)))

  ;; Force pwd to home directory if so required.
  (cond (post-force-pwd-to-home
	 (cd "~")))

  ;; Kill quoted sig if so required.
  (cond (post-kill-quoted-sig
	 (post-delete-quoted-signatures)
         (not-modified)))

  ;; Remap signature selection functions according to whether the
  ;; signatures are stored in a file or directory.
  (if post-signature-source-is-file
      (progn
	(defalias 'post-select-signature 'post-select-signature-from-file)
	(defalias 'post-select-signature-select-sig
	  'post-select-signature-select-sig-from-file))
    (progn
      (defalias 'post-select-signature 'post-select-signature-from-dir)
      (defalias 'post-select-signature-select-sig
	'post-select-signature-select-sig-from-dir)))

  ;; Define mutt/slrn specific key bindings.
  (define-key (current-local-map) "\C-c\C-b"	 'post-make-region-bold)
  (define-key (current-local-map) "\C-c\C-u"	 'post-make-region-underlined)
  (define-key (current-local-map) "\C-c\C-q"	 'post-quote-region)
  (define-key (current-local-map) "\C-c\C-d\C-q" 'post-unquote-region)
  (define-key (current-local-map) "\C-c\C-a"	 'post-attach-file)
  (define-key (current-local-map) "\C-c\C-p"	 'post-set-return-receipt-to)

  ;; Give the buffer a handy name.
  (if post-rename-buffer
      (setq post-buf (rename-buffer "*Composing*" t)))
 
  ;; If this is a news posting, check the length of the References field.
  (if (post-references-p)
      (header-check-references))

  ;; Define the quote signs as comments to make comment-region usable.
  (make-local-variable 'comment-start)
  (setq comment-start post-quote-start)

  ;; Run any hooks.
  (run-hooks 'post-mode-hook)

  ;; Jump past header if so required.
  (cond (post-jump-header
         (post-goto-body)))

  (unless (fboundp 'server-process-filter)
    (message (substitute-command-keys
     "Type \\[describe-mode] for help composing; \\[post-save-current-buffer-and-exit] when done."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Post Header Mode

(defvar header-mode nil)

(defun header-mode (&optional arg)
  "Commands for editing the header of an e-mail or news message.

\\{header-mode-map}
Optional argument ARG ."

  (interactive "P")
  (make-local-variable 'header-mode)
  (setq header-mode
	(if (null arg)
	    (not header-mode)
	  (> (prefix-numeric-value arg) 0)))
  (setq post-has-attachment nil)

  ;; XEmacs needs easy-menu-add, Emacs does not care
  (easy-menu-add header-mode-menu)

  (force-mode-line-update))

(defvar header-mode-map (make-sparse-keymap)
  "Keymap used for editing RFC822 header.")

(defun header-position-on-value ()
  "Go to the start of the value part of a header."
  (beginning-of-line)
  (skip-chars-forward "-A-Za-z0-9:")
  ;; XXX - Should make sure we stay on line.
  (forward-char)
  (point))

(defun header-goto-field (field)
  "Go to FIELD of a header."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (save-match-data
      (when (re-search-forward (concat "^\\($\\|" field ": \\)"))
	(if (looking-at "^$")
	    (progn
	      (insert-string field ": \n")
	      (forward-char -1))
	  (header-position-on-value))))))

(defmacro define-header-goto (name header)
  "Define functions called NAME to go to HEADER."
  `(defun ,name ()
     ,(concat "Position the cursor on the " header ": header.")
     (interactive)
     (header-goto-field ,header)))

(define-header-goto header-goto-to "To")
(define-header-goto header-goto-cc "Cc")
(define-header-goto header-goto-fcc "Fcc")
(define-header-goto header-goto-summary "Summary")
(define-header-goto header-goto-keywords "Keywords")
(define-header-goto header-goto-subject "Subject")
(define-header-goto header-goto-bcc "Bcc")
(define-header-goto header-goto-reply-to "Reply-To")
(define-header-goto header-goto-from "From")
(define-header-goto header-goto-organization "Organization")

(defun header-attach-file (file description)
  "Attach a FILE to the current message (works with Mutt).
Argument DESCRIPTION MIME description."
  (interactive "fAttach file: \nsDescription: ")
  (when (> (length file) 0)
    (save-excursion
      (save-match-data
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (search-forward-regexp "^$")
	  (insert-string (concat "Attach: " (file-truename file) " "
				 description "\n"))
	  (message (concat "Attached '" file "'."))
	  (setq post-has-attachment t))))))

(or (assq 'header-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(header-mode " Header") minor-mode-alist)))

(or (assq 'header-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'header-mode header-mode-map)
		minor-mode-map-alist)))

(defun header-set-return-receipt-to (address)
  "Insert a Return-Receipt-To header into an email.
Argument ADDRESS email address return receipts should be sent to."
  (interactive (list (post-ask-for-address-with-default "Return-Receipt-To")))
  (save-excursion
    (header-set-value "Return-Receipt-To" address)))

(defun post-news-posting-p ()
  "Does the buffer look like a news posting?"
  (save-excursion
    (setf (point) (point-min))
    (looking-at "^Newsgroups: ")))

(defun header-set-followup-to (to)
  "Set the Followup-To: header.
Argument TO Where followups should go."
  (interactive (list (header-ask-for-value "Followup-To"
					   (header-ask-for-value
					    "Newsgroups"))))
  (cond ((post-news-posting-p)
	 (save-excursion
	   (header-set-value "Followup-To" to)))
	(t
	 (error
  "Followup-To is for Usenet.  Maybe you want Reply-To or Mail-Followup-To"))))

(defun header-set-organization (org)
  "Set the Organization: header.
Argument ORG Should be SMERSH."
  (interactive (list (header-ask-for-value "Organization")))
  (save-excursion
    (header-set-value "Organization" org)))

(defun header-check-references ()
  "Place the cursor at the start of the References: if they are too long."
  (interactive)
  (cond ((> (header-references-length) 500) ; 500 to be on the safe side.
         (beep)                              ; Catch my attention.
         (goto-char (point-min))
         (search-forward-regexp "^References: " nil t))))

(defun header-references-length (&optional show)
  "Get (and optionally display) the length of the references header.
Optional argument SHOW Whether or not to display the length."
  (interactive)
  (let* ((header "References")
         (refs (header-get-value header))
         (len (+ (length header) (length refs) 2)))
    (if (or (interactive-p) show)
        (message "References header is %d characters in length." len))
    len))

(defun header-delete-reference ()
  "Delete the first reference in the references header."
  (interactive)
  (save-excursion
    (let ((ref-location (header-goto-field "References")))
      (cond (ref-location
             (let ((ref-start (goto-char ref-location)))
               (cond ((search-forward ">" nil t)
                      (forward-char 1)
                      (delete-region ref-start (point))
                      (header-references-length t)))))))))

;; Noninteractive functions.

(defun header-ask-for-value (header &optional default)
  "Ask for a HEADER value, defaulting to the current value if one is present.
Optional argument DEFAULT ."
  (let ((new-value (post-get-header-value header)))
    (and (= (length new-value) 0)
         default
         (setq new-value default))
    (read-string (concat header ": ") new-value)))

(defun header-get-value (header)
  "Get the value of a specific mail HEADER."
  (save-excursion
    (let ((value          "")
          (start-of-value nil))
      (goto-char (point-min))
      (cond ((post-find-header-line header)
             (setq start-of-value (point))
             (end-of-line)
             (setq value (buffer-substring-no-properties
			  start-of-value (point)))))
      value)))

(defun header-set-value (header value)
  "Set VALUE of a HEADER (replacing any existing value)."
  (let ((kill-ring kill-ring))
    (setf (point) (point-min))
    (cond ((post-find-header-line header)
	   (beginning-of-line)
	   (kill-line)
	   (insert-string (concat header ": " value)))
	  (t
	   (header-append-value header value))))
  (message "%s set to %s" header value))

(defun header-append-value (header value)
  "Add a HEADER and set it's VALUE (if header exists, will add multiple headers)."
  (goto-char (point-min))
  (search-forward-regexp "^$" nil t)
  (insert-string (concat header ": " value "\n")))

;;; Setup the mode map for the select-signature buffer.
(if post-select-signature-mode-map nil
  (setq post-select-signature-mode-map (make-sparse-keymap))
  (define-key post-select-signature-mode-map "\C-m"
    'post-select-signature-select-sig)
  (define-key post-select-signature-mode-map " "
    'post-select-signature-select-sig)
  (define-key post-select-signature-mode-map "q" 'post-select-signature-quit)
  (define-key post-select-signature-mode-map "\C-g"
    'post-select-signature-quit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Key Bindings

(define-key post-mode-map "\C-c\C-c" 'post-save-current-buffer-and-exit)
(define-key post-mode-map "\C-c\C-d\C-s" 'post-delete-quoted-signatures)
(define-key post-mode-map "\C-c\C-d\C-c" 'post-delete-old-citations)
(define-key post-mode-map "\C-c\C-t" 'post-goto-body)
(define-key post-mode-map "\C-c\C-e" 'post-goto-signature)
(define-key post-mode-map "\C-c\C-r" 'post-random-signature)
(define-key post-mode-map "\C-c\C-b" 'post-make-region-bold)
(define-key post-mode-map "\C-c\C-u" 'post-make-region-underlined)
(define-key post-mode-map "\C-c\C-q" 'post-quote-region)
(define-key post-mode-map "\C-c\C-d\C-q" 'post-unquote-region)
(define-key post-mode-map "\C-c\C-s" 'post-select-signature)

(define-key header-mode-map "\C-c\C-f\C-t" 'header-goto-to)
(define-key header-mode-map "\C-c\C-f\C-c" 'header-goto-cc)
(define-key header-mode-map "\C-c\C-f\C-w" 'header-goto-fcc)
(define-key header-mode-map "\C-c\C-f\C-u" 'header-goto-summary)
(define-key header-mode-map "\C-c\C-f\C-k" 'header-goto-keywords)
(define-key header-mode-map "\C-c\C-f\C-s" 'header-goto-subject)
(define-key header-mode-map "\C-c\C-f\C-b" 'header-goto-bcc)
(define-key header-mode-map "\C-c\C-f\C-r" 'header-goto-reply-to)
(define-key header-mode-map "\C-c\C-f\C-f" 'header-goto-from)
(define-key header-mode-map "\C-c\C-f\C-o" 'header-goto-organization)
(define-key header-mode-map "\C-c\C-ff"    'header-set-followup-to)
(define-key header-mode-map "\C-c\C-a"     'header-attach-file)
(define-key header-mode-map "\C-c\C-fd"    'header-delete-reference)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Menus

(easy-menu-define
 post-mode-menu post-mode-map "Post Message Composition Commands."
 '("Post"
   ["Delete quoted signatures" post-delete-quoted-signatures t]
   ["Delete doubly quoted text" post-delete-old-citations t]
   "----"
   ["Go to body of message" post-goto-body t]
   ["Go to signature of message" post-goto-signature t]
   ["Get new random signature" post-random-signature t]
   ["Select new signature" post-select-signature t]
   "----"
   ["Embolden region" post-make-region-bold t]
   ["Underline region" post-make-region-underlined t]
   "----"
   ["Quote region" post-quote-region t]
   ["Unquote region" post-unquote-region t]
   "----"
   ["Save message and return from Post" post-save-current-buffer-and-exit t]))

(easy-menu-define
 header-mode-menu header-mode-map "Header Editing Commands."
 '("Header"
   ["Attach File..." header-attach-file t]
   "----"
   ["Edit From Header" header-goto-from t]
   ["Edit Subject Header" header-goto-subject t]
   ["Edit To Header" header-goto-to t]
   ["Edit Cc Header" header-goto-cc t]
   ["Edit Bcc Header" header-goto-bcc t]
   ["Edit Fcc Header" header-goto-fcc t]
   ["Edit Reply-To Header" header-goto-reply-to t]
   ["Edit Summary Header" header-goto-summary t]
   ["Edit Keywords Header" header-goto-keywords t]
   ["Edit Organization Header" header-goto-organization t]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finish Installing Post Mode

(when post-insert-to-auto-mode-alist-on-load
  (unless (assq post-mail-message auto-mode-alist)
    (setq auto-mode-alist
	  (cons (cons post-mail-message 'post-mode)
		auto-mode-alist)))
  (unless (assq post-news-posting auto-mode-alist)
    (setq auto-mode-alist
	  (cons (cons post-news-posting 'post-mode)
		auto-mode-alist))))

(provide 'post)

;;; post.el ends here
