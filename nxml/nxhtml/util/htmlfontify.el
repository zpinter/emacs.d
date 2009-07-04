;; htmlfontify.el  - htmlise a buffer/source tree with optional hyperlinks

;; Emacs Lisp Archive Entry
;; Package: htmlfontify
;; Filename: htmlfontify.el
;; Version: 0.20
;; Keywords: html, hypermedia, markup, etags
;; Author: Vivek Dasmohapatra <vivek@etla.org>
;; Maintainer: Vivek Dasmohapatra <vivek@etla.org>
;; Created: 2002-01-05
;; Description: htmlise a buffer/source tree with optional hyperlinks
;; URL: http://rtfm.etla.org/emacs/htmlfontify/
;; Compatibility: Emacs20, Emacs21
;; Incompatibility: Emacs19
;; Last Updated: Sun 2003-03-09 01:27:57 +0000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I have made some changes to make it work for Emacs 22.  A lot of
;; small bug fixes related to the format of text and overlay
;; properties (which might have changed since the beginning of 2003
;; when this file was originally written).
;;
;; The function `hfy-face-at' currently carries much of the burden of
;; my lacking understanding of the formats mentioned above and should
;; need some knowledgeable help.
;;
;; Another thing that maybe could be fixed is that overlay background
;; colors which are now only seen where there is text (in the XHTML
;; output). A bit of CSS tweaking is necessary there.
;;
;; The face 'default has a value :background "SystemWindow" for the
;; background color.  There is no explicit notion that this should be
;; considered transparent, but I have assumed that it could be handled
;; like if it was here. (I am unsure that background and foreground
;; priorities are handled ok, but it looks ok in my tests now.)
;;
;; Invisible text does not seem to be honored now.
;;
;; 2007-12-27 Lennart Borgman
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; Copyright (C) 2002,2003 Vivek Dasmohapatra <vivek@etla.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Here's some elisp code to html-pretty-print an emacs buffer, preserving
;; the emacs syntax/whatever highlighting. It also knows how to drive etags
;; (exuberant-ctags or emacs etags) and hyperlink the code according
;; to its (etags') output.

;; NOTE: Currently the hyperlinking code only knows how to drive GNU find
;; and the exuberant and GNU variants of etags : I do not know of any other
;; etags variants, but mechanisms have been provided to allow htmlfontify
;; to be taught how to drive them. As long as your version of find has
;; the -path test and is reasonably sane, you should be fine.

;; A sample of the htmlfontified / hyperlinked output of this module can be
;; found at http://rtfm.etla.org/sql/dbishell/src/ - it's not perfect, but
;; it's a hell of a lot faster and more thorough than I could hope to be
;; doing this by hand.

;; some user / horrified onlooker comments:
;; What? No! There's something deeply wrong here...   (R. Shufflebotham)
;; You're a freak.                                    (D. Silverstone)
;; Aren't we giving you enough to do?                 (J. Busuttil)
;; You're almost as messed up as Lexx is!             (N. Graves-Morris)

;; Changes: moved to changelog (CHANGES) file.

;; elisp-dep-block >>
;;(require 'faces)
;;  (`facep' `face-attr-construct' `x-color-values' `color-values' `face-name')
;;(require 'fast-lock)
;;  (`fast-lock-mode')
;;(require 'custom)
;;  (`defgroup' `defcustom')
;;(require 'font-lock)
;;  (`font-lock-fontify-region')
;; elisp-dep-block <<

(eval-and-compile

  ;; emacs 20 compatibility:
  (if (<= 21 emacs-major-version) nil ;; already post emacs21, NOOP
    (require 'hfy-emacs20))

  ;; non-x-windows emacs20 duct tape...
  ;; admittedly, we get no significant fontification here anyway
  ;; because emacs20 can't fontify on a tty
  (if window-system nil ;; NOOP
    (defun x-color-defined-p (colour &optional frame)
      (let ((window-system nil)) (if (hfy-colour-vals colour) t nil))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; I want these - can't be bothered requiring all of cl though.
  (if (not (fboundp 'caddr))
      (defun caddr (list)
        "Return the `car' of the `cddr' of LIST."
        (car (cddr list))))

  (if (not (fboundp 'cadddr))
      (defun cadddr (list)
        "Return the `cadr' of the `cddr' of LIST."
        (cadr (cddr list))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (autoload
    'htmlfontify-load-rgb-file
    "hfy-cmap"
    "Load an rgb.txt file for colour name -> rgb translation purposes."
    'interactive)

  (autoload
    'htmlfontify-unload-rgb-file
    "hfy-cmap"
    "Unload the current colour name -> rgb translation map."
    'interactive)

  (autoload
    'hfy-fallback-colour-values
    "hfy-cmap"
    "Use a fallback method for obtaining the rgb values for a colour."
    'interactive)
  )

(defconst htmlfontify-version 0.20)

(defconst hfy-meta-tags
  (format "<meta name=\"generator\" content=\"emacs %s; htmlfontify %0.2f\" />"
          emacs-version htmlfontify-version)
  "generator meta tag for this version of htmlfontify")

(defconst htmlfontify-manual "Htmlfontify Manual"
  "Copy and convert buffers and files to html, adding hyperlinks between
files \(driven by etags\) if requested.\n
Interactive functions:
  `htmlfontify-buffer'
  `htmlfontify-run-etags'
  `htmlfontify-copy-and-link-dir'
  `htmlfontify-load-rgb-file'
  `htmlfontify-unload-rgb-file'\n
In order to:\n
fontify a file you have open:           M-x htmlfontify-buffer
prepare the etags map for a directory:  M-x htmlfontify-run-etags
copy a directory, fontifying as you go: M-x htmlfontify-copy-and-link-dir\n
The following might be useful when running non-windowed or in batch mode:
\(note that they shouldn't be necessary - we have a built in map\)\n
load an X11 style rgb.txt file:         M-x htmlfontify-load-rgb-file
unload the current rgb.txt file:        M-x htmlfontify-unload-rgb-file\n
And here's a programmatic example:\n
\(defun rtfm-build-page-header \(file style\)
  \(format \"#define  TEMPLATE red+black.html
#define  DEBUG    1
#include <build/menu-dirlist|>\\n
html-css-url := /css/red+black.css
title        := rtfm.etla.org \( %s / src/%s \)
bodytag      :=
head         <=STYLESHEET;\\n
%s
STYLESHEET
main-title   := rtfm / %s / src/%s\\n
main-content <=MAIN_CONTENT;\\n\" rtfm-section file style rtfm-section file\)\)

\(defun rtfm-build-page-footer \(file\) \"\\nMAIN_CONTENT\\n\"\)

\(defun rtfm-build-source-docs \(section srcdir destdir\)
  \(interactive
   \"s section[eg- emacs / p4-blame]:\\nD source-dir: \\nD output-dir: \"\)
  \(require 'htmlfontify\)
  \(hfy-load-tags-cache srcdir\)
  \(let \(\(hfy-page-header  'rtfm-build-page-header\)
        \(hfy-page-footer  'rtfm-build-page-footer\)
        \(rtfm-section                     section\)
        \(hfy-index-file                   \"index\"\)\)
    \(htmlfontify-run-etags srcdir\)
    \(htmlfontify-copy-and-link-dir srcdir destdir \".src\" \".html\"\)\)\)")

(defgroup htmlfontify nil
  "Copy and convert buffers and files to html, adding hyperlinks between
files \(driven by etags\) if requested.\n
See: `htmlfontify-manual'"
  :group  'applications
  :prefix "hfy-")

(defcustom hfy-page-header 'hfy-default-header
  "*Function called with two arguments \(the filename relative to the top
level source directory being etag\'d and fontified), and a string containing
the <style>...</style> text to embed in the document- the string returned will
be used as the header for the htmlfontified version of the source file.\n
See also: `hfy-page-footer'"
  :group 'htmlfontify
  :tag   "page-header"
  :type  '(function))

(defcustom hfy-split-index nil
  "*Whether or not to split the index `hfy-index-file' alphabetically
on the first letter of each tag. Useful when the index would otherwise
be large and take a long time to render or be difficult to navigate."
  :group 'htmlfontify
  :tag   "split-index"
  :type  '(boolean))

(defcustom hfy-page-footer 'hfy-default-footer
  "*As `hfy-page-header', but generates the output footer
\(and takes only 1 argument, the filename\)."
  :group 'htmlfontify
  :tag   "page-footer"
  :type  '(function))

(defcustom hfy-extn        ".html"
  "*File extension used for output files"
  :group 'htmlfontify
  :tag   "extension"
  :type  '(string))

(defcustom hfy-src-doc-link-style "text-decoration: underline;"
  "*String to add to the \'<style> a\' variant of an htmlfontify css class."
  :group 'htmlfontify
  :tag   "src-doc-link-style"
  :type  '(string))

(defcustom hfy-src-doc-link-unstyle " text-decoration: none;"
  "*Regex to remove from the <style> a variant of an htmlfontify css class."
  :group 'htmlfontify
  :tag   "src-doc-link-unstyle"
  :type  '(string))

(defcustom hfy-link-extn nil
  "*File extension used for href links - Useful where the htmlfontify
output files are going to be processed again, with a resulting change
in file extension. If `nil', then any code using this should fall back
to `hfy-extn'."
  :group 'htmlfontify
  :tag   "link-extension"
  :type  '(choice string (const nil)))

(defcustom hfy-link-style-fun 'hfy-link-style-string
  "*Set this to a function, which will be called with one argument
\(a \"{ foo: bar; ...}\" css style-string\) - it should return a copy of
its argument, altered so as to make any changes you want made for text which
is a hyperlink, in addition to being in the class to which that style would
normally be applied."
  :group 'htmlfontify
  :tag   "link-style-function"
  :type  '(function))

(defcustom hfy-index-file  "hfy-index"
  "*Name \(sans extension\) of the tag definition index file produced during
fontification-and-hyperlinking."
  :group 'htmlfontify
  :tag   "index-file"
  :type  '(string))

(defcustom hfy-instance-file  "hfy-instance"
  "*Name \(sans extension\) of the tag usage index file produced during
fontification-and-hyperlinking."
  :group 'htmlfontify
  :tag   "instance-file"
  :type  '(string))

(defcustom hfy-html-quote-regex "\\(<\\|\"\\|&\\|>\\)"
  "*Regex to match \(with a single back-reference per match\) strings in HTML
which should be quoted with `hfy-html-quote' \(and `hfy-html-quote-map'\)
to make them safe."
  :group 'htmlfontify
  :tag   "html-quote-regex"
  :type  '(regexp))

(defcustom hfy-init-kludge-hooks '(hfy-kludge-cperl-mode)
  "*List of functions to call when starting htmlfontify-buffer to do any
kludging necessary to get highlighting modes to bahave as you want, even
when not running under a window system."
  :group 'htmlfontify
  :tag   "init-kludge-hooks"
  :type  '(hook))

(defcustom hfy-post-html-hooks nil
  "*List of functions to call after creating and filling the html buffer.
These functions will be called with the html buffer as the current buffer"
  :group   'htmlfontify
  :tag     "post-html-hooks"
  :options '(set-auto-mode)
  :type    '(hook))

(defcustom hfy-default-face-def nil
  "*Fallback `defface' specification for the face \'default, used when
`hfy-display-class' has been set \(the normal htmlfontify way of extracting
potentially non-current face information doesn\'t necessarily work for
\'default\).\n
Example: I customise this to:\n
\(\(t :background \"black\" :foreground \"white\" :family \"misc-fixed\"\)\)"
  :group   'htmlfontify
  :tag     "default-face-definition"
  :type    '(alist))

(defcustom hfy-etag-regex (concat ".*"
                                  "\x7f" "\\([^\x01\n]+\\)"
                                  "\x01" "\\([0-9]+\\)"
                                  ","    "\\([0-9]+\\)$"
                                  "\\|"  ".*\x7f[0-9]+,[0-9]+$")
  "*Regex used to parse an etags entry: must have 3 subexps, corresponding,
in order, to:\n
   1 - The tag
   2 - The line
   3 - The char \(point\) at which the tag occurs."
  :group 'htmlfontify
  :tag   "etag-regex"
  :type  '(regexp))

(defcustom hfy-html-quote-map '(("\"" "&quot;")
                                ("<"  "&lt;"  )
                                ("&"  "&amp;" )
                                (">"  "&gt;"  ))
  "*Alist of char -> entity mappings used to make the text html-safe."
  :group 'htmlfontify
  :tag   "html-quote-map"
  :type  '(alist :key-type (string)))

(defconst hfy-e2x-etags-cmd "for src in `find . -type f`;
do
  ETAGS=%s;
  case ${src} in
    *.ad[absm]|*.[CFHMSacfhlmpsty]|*.def|*.in[cs]|*.s[as]|*.src|*.cc|\\
    *.hh|*.[chy]++|*.[ch]pp|*.[chy]xx|*.pdb|*.[ch]s|*.[Cc][Oo][Bb]|\\
    *.[eh]rl|*.f90|*.for|*.java|*.[cem]l|*.clisp|*.lisp|*.[Ll][Ss][Pp]|\\
    [Mm]akefile*|*.pas|*.[Pp][LlMm]|*.psw|*.lm|*.pc|*.prolog|*.oak|\\
    *.p[sy]|*.sch|*.scheme|*.[Ss][Cc][Mm]|*.[Ss][Mm]|*.bib|*.cl[os]|\\
    *.ltx|*.sty|*.TeX|*.tex|*.texi|*.texinfo|*.txi|*.x[bp]m|*.yy|\\
    *.[Ss][Qq][Ll])
          ${ETAGS} -o- ${src};
          ;;
      *)
          FTYPE=`file ${src}`;
          case ${FTYPE} in
              *script*text*)
                  ${ETAGS} -o- ${src};
                  ;;
              *text*)
                  SHEBANG=`head -n1 ${src} | grep '#!' -c`;
                  if [ ${SHEBANG} -eq 1 ];
                  then
                      ${ETAGS} -o- ${src};
                  fi;
                  ;;
          esac;
          ;;
  esac;
done;")

(defcustom hfy-etags-cmd-alist
  `(("emacs etags"     . ,hfy-e2x-etags-cmd)
    ("exuberant ctags" . "%s -R -f -"   ))
  "*alist of possible shell commands that will generate etags output that
`htmlfontify' can use. \'%s\' will be replaced by `hfy-etags-bin'."
  :group 'htmlfontify
  :tag   "etags-cmd-alist"
  :type  '(alist :key-type (string) :value-type (string)) )

(defcustom hfy-etags-bin "etags"
  "*location of etags binary (we begin by assuming it\'s in your path).\n
Note that if etags is not in your path, you will need to alter the shell
commands in `hfy-etags-cmd-alist'."
  :group 'htmlfontify
  :tag   "etags-bin"
  :type  '(file))

(defcustom hfy-shell-file-name "/bin/sh"
  "*shell (bourne or compatible) to invoke for complex shell operations."
  :group 'htmlfontify
  :tag   "shell-file-name"
  :type  '(file))

(defun hfy-which-etags ()
  (let ((v (shell-command-to-string (concat hfy-etags-bin " --version"))))
    (cond ((string-match "exube" v) "exuberant ctags")
          ((string-match "GNU E" v) "emacs etags"    )) ))

(defcustom hfy-etags-cmd
  (eval-and-compile (cdr (assoc (hfy-which-etags) hfy-etags-cmd-alist)))
  "*etags equivalent command to run in a source directory to generate a tags
file for the whole source tree from there on down. The command should emit
the etags output on stdout.\n
Two canned commands are provided - they drive emacs\' etags and
exuberant-ctags\' etags respectively."
  :group 'htmlfontify
  :tag   "etags-command"
  :type (eval-and-compile
          (let ((clist (list '(string))))
            (mapc
             (lambda (C)
               (setq clist
                     (cons (list 'const :tag (car C) (cdr C)) clist)))
             hfy-etags-cmd-alist)
            (cons 'choice clist)) ))

(defcustom hfy-istext-command "file %s | sed -e 's@^[^:]*:[ \t]*@@'"
  "*command to run with the name of a file, to see whether it is a text file
or not. The command should emit a string containing the word \'text\' if
the file is a text file, and a string not containing \'text\' otherwise."
  :group 'htmlfontify
  :tag   "istext-command"
  :type  '(string))

(defcustom hfy-find-cmd
  "find . -type f \\! -name \\*~ \\! -name \\*.flc \\! -path \\*/CVS/\\*"
  "*find command used to harvest a list of files to attempt to fontify."
  :group 'htmlfontify
  :tag   "find-command"
  :type  '(string))

(defcustom hfy-display-class nil
  "*Display class to use to determine which display class to use when
calculating a face\'s attributes. This is useful when, for example, you
are running emacs on a tty or in batch mode, and want htmlfontify to have
access to the face spec you would use if you were connected to an X display.\n
Some valid class specification elements are:\n
  \'\(class      color\)
  \'\(class      grayscale\)
  \'\(background dark\)
  \'\(background light\)
  \'\(type       x-toolkit\)
  \'\(type       tty\)
  \'\(type       motif\)
  \'\(type       lucid\)
Multiple values for a tag may be combined, to indicate that any one or more
of these values in the specification key constitutes a match, eg:\n
\'\(\(class color grayscale\) \(type tty\)\) would match any of:\n
  \'\(\(class color\)\)
  \'\(\(class grayscale\)\)
  \'\(\(class color grayscale\)\)\)
  \'\(\(class color foo\)\)
  \'\(\(type  tty\)\)
  \'\(\(type  tty\) \(class color\)\)\n
and so on."
  :type    '(alist :key-type (symbol) :value-type (symbol))
  :group   'htmlfontify
  :tag     "display-class"
  :options '((type       (choice (const :tag "X11"           x-toolkit)
                                 (const :tag "Terminal"      tty      )
                                 (const :tag "Lucid Toolkit" lucid    )
                                 (const :tag "Motif Toolkit" motif    )))

             (class      (choice (const :tag "Colour"        color    )
                                 (const :tag "Greyscale"     grayscale)))

             (background (choice (const :tag "Dark"          dark     )
                                 (const :tag "Bright"        light    ))) ))

(defcustom hfy-optimisations (list 'keep-overlays)
  "*Optimisations to turn on: So far, the following have been implemented:\n
  merge-adjacent-tags: If two (or more) span tags are adjacent, identical and
                       separated by nothing more than whitespace, they will
                       be merged into one span.
  zap-comment-links  : Suppress hyperlinking of tags found in comments.
  zap-string-links   : Suppress hyperlinking of tags found in strings.
  div-wrapper        : Add <div class=\"default\"> </div> tags around the
                       output.
  keep-overlays      : More of a bell \(or possibly whistle\) than an
                       optimisation - If on, preserve overlay highlighting
                       \(cf ediff or goo-font-lock\) as well as basic faces\n
  And the following are planned but not yet available:\n
  kill-context-leak  : Suppress hyperlinking between files highlighted by
                       different modes.\n
Note: like compiler optimisations, these optimise the _output_ of the code,
not the processing of the source itself, and are therefore likely to slow
htmlfontify down, at least a little. Except for skip-refontification,
which can never slow you down, but may result in incomplete fontification."
  :type  '(set (const :tag "merge-adjacent-tags"  merge-adjacent-tags )
               (const :tag "zap-comment-links"    zap-comment-links   )
               (const :tag "zap-string-links"     zap-string-links    )
               (const :tag "skip-refontification" skip-refontification)
               (const :tag "kill-context-leak"    kill-context-leak   )
               (const :tag "div-wrapper"          div-wrapper         )
               (const :tag "keep-overlays"        keep-overlays       )
               )
  :group 'htmlfontify
  :tag   "optimisations")

;; (defcustom hfy-fast-lock-save nil
;;   "*Minimum size of a buffer for cached fontification.
;; This value is temporarily assigned to `fast-lock-minimum-size' during
;; html-fontification.\n
;; Only buffers more than this can have associated Font Lock cache files saved.\n
;; If nil, means cache files are never created.\n
;; If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
;; where MAJOR-MODE is a symbol or t (meaning the default).  For example:\n
;;  ((c-mode     . 25600  )\n  (c++-mode   . 25600  )\n  (rmail-mode . 1048576))\n
;; means that the minimum size is 25K for buffers in C or C++ modes, one megabyte
;; for buffers in Rmail mode, and size is irrelevant \(ie no saves\) otherwise.\n"
;;   :type '(choice
;;           (const   :tag "none" nil)
;;           (integer :tag "size")
;;           (repeat  :menu-tag "mode specific"
;;                    :tag      "mode specific"
;;                    :value ((t . nil))
;;                    (cons :tag "Instance"
;;                          (radio :tag "Mode"
;;                                 (const  :tag "all" t)
;;                                 (symbol :tag "name"))
;;                          (radio :tag "Size"
;;                                 (const   :tag "none" nil)
;;                                 (integer :tag "size")))))
;;   :group 'htmlfontify
;;   :tag   "fast-lock-minimum-size")

(defvar hfy-tags-cache  nil
  "Alist of the form\n
\(\(\"/src/dir/0\" . tag-hash0\) \(\"/src/dir/1\" tag-hash1\) ...\)\n
Each  tag hash entry then contains entries of the form:\n
\"tag_string\" => ((\"file/name.ext\" line char) ... )\n
ie an alist mapping \(relative\) file paths to line and character offsets.\n
See `hfy-load-tags-cache'.")

(defvar hfy-tags-sortl  nil
  "Alist of the form \(\(\"/src/dir\" . (tag0 tag1 tag2)\) ... \)\n
Where the tags are stored in descending order of length.\n
See `hfy-load-tags-cache'.")

(defvar hfy-tags-rmap   nil
  "Alist of the form \(\(\"/src/dir\" . tag-rmap-hash\)\)\n
Where tag-rmap-hash has entries of the form:
\"tag_string\" => ( \"file/name.ext\" line char )
Unlike `hfy-tags-cache' these are the locations of occurrences of
tagged items, not the locations of their definitions.")

(defvar hfy-style-assoc 'please-ignore-this-line
  "An assoc representing/describing an emacs face. Properties may be repeated,
In which case later properties should be treated as if they were inherited
from a \'parent\' font. \(For some properties, only the first encountered value
is of any importance, for others the values might be cumulative, and for
others they might be cumulative in a complex way).\n
Some examples:\n
\(hfy-face-to-style 'default\) =>
  \(\(\"background\"      . \"rgb\(0, 0, 0\)\"\)
   \(\"color\"           . \"rgb\(255, 255, 255\)\"\)
   \(\"font-style\"      . \"normal\"\)
   \(\"font-weight\"     . \"500\"\)
   \(\"font-stretch\"    . \"normal\"\)
   \(\"font-family\"     . \"misc-fixed\"\)
   \(\"font-size\"       . \"13pt\"\)
   \(\"text-decoration\" . \"none\"\)\)\n
\(hfy-face-to-style 'Info-title-3-face\) =>
  \(\(\"font-weight\"     . \"700\"\)
   \(\"font-family\"     . \"helv\"\)
   \(\"font-size\"       . \"120%\"\)
   \(\"text-decoration\" . \"none\"\)\)\n")

(defvar hfy-sheet-assoc 'please-ignore-this-line
  "An assoc with elements of the form (face-name style-name . stlye-string):\n
'\(\(default               \"default\" . \"{background: black;color: white}\"\)
  \(font-lock-string-face \"string\"  . \"{color: rgb\(64,224,208\)}\"\)\)" )

(defvar hfy-facemap-assoc 'please-ignore-this-line
  "An assoc of \(point . FACE-SYMBOL\) or \(point . DEFFACE-LIST\)
and (point . 'end) elements, in descending order of point value
\(ie from the file's end to its beginning\).\n
The map is in reverse order because inserting a <style> tag \(or any other
string) at POINT invalidates the map for all entries with a greater value of
point. By traversing the map from greatest to least POINT, we still invalidate
the map as we go, but only those points we have already dealt with \( and
therefore no longer care about \) will be invalid at any time.\n
'\(\(64820 . end\)
  \(64744 . font-lock-comment-face\)
  \(64736 . end\)
  \(64722 . font-lock-string-face\)
  \(64630 . end\)
  \(64623 . font-lock-string-face\)
  \(64449 . end\)
  \(64446 . font-lock-keyword-face\)
  \(64406 . end\)
  \(64395 . font-lock-constant-face\)
  \(64393 . end\)
  \(64386 . font-lock-keyword-face\)
  \(64379 . end\)
  ;; big similar section elided. You get the idea.
  \(4285 . font-lock-constant-face\)
  \(4285 . end\)
  \(4221 . font-lock-comment-face\)
  \(4221 . end\)
  \(4197 . font-lock-constant-face\)
  \(4197 . end\)
  \(1 . font-lock-comment-face\)\)")

(defvar hfy-tmpfont-stack nil
  "An alist of derived fonts resulting from overlays")

(defconst hfy-hex-regex "[0-9A-Fa-f]")

(defconst hfy-triplet-regex
  (concat
   "\\(" hfy-hex-regex hfy-hex-regex "\\)"
   "\\(" hfy-hex-regex hfy-hex-regex "\\)"
   "\\(" hfy-hex-regex hfy-hex-regex "\\)"))

(defun hfy-interq (set-a set-b)
  "Return the intersection \(using `eq'\) of 2 lists."
  (let ((sa set-a) (interq nil) (elt nil))
    (while sa
      (setq elt (car sa)
            sa  (cdr sa))
      (if (memq elt set-b) (setq interq (cons elt interq)))) interq))

(defun hfy-colour-vals (colour)
  "Where COLOUR is a colour name or #XXXXXX style triplet, return a
list of 3 (16 bit) rgb values for said colour.\n
If a window system is unavailable, calls `hfy-fallback-colour-values'."
  (if (string-match hfy-triplet-regex colour)
      (mapcar
       (lambda (x)
         (* (string-to-int (match-string x colour) 16) 257)) '(1 2 3))
    ;;(message ">> %s" colour)
    (if window-system
        (if (fboundp 'color-values)
            (color-values colour)
          ;;(message "[%S]" window-system)
          (x-color-values colour))
      ;; blarg - tty colours are no good - go fetch some X colours:
      (hfy-fallback-colour-values colour))))

(defvar hfy-cperl-mode-kludged-p nil)

(defun hfy-kludge-cperl-mode ()
  "cperl mode does its damndest not to do some of its fontification when not
in a windowing system - try to trick it..."
  (if (not hfy-cperl-mode-kludged-p)
      (progn (if (not window-system)
                 (let ((window-system 'htmlfontify))
                   (eval-and-compile (require 'cperl-mode))
                   (setq cperl-syntaxify-by-font-lock t)))
             (setq hfy-cperl-mode-kludged-p t))) )

(defun hfy-opt (symbol) (memq symbol hfy-optimisations))

(defun hfy-default-header (file style)
  "Default value for `hfy-page-header'"
;;   (format "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
;; <html>\n <head>\n  <title>%s</title>\n %s\n </head>\n  <body>\n" file style))
  (format "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title>%s</title>
%s
  </head>
  <body>\n"
          file style))

(defun hfy-default-footer (file)
  "Default value for `hfy-page-footer'"
  "\n </body>\n</html>\n")

(defun hfy-link-style-string (style-string)
  "Replace the end of a css style declaration STYLE-STRING with the contents
of the variable `hfy-src-doc-link-style', removing text matching the regex
`hfy-src-doc-link-unstyle' first, if necessary."
  ;;(message "hfy-colour-vals");;DBUG
  (if (string-match hfy-src-doc-link-unstyle style-string)
      (setq style-string (replace-match "" 'fixed-case 'literal style-string)))
  (if (and (not (string-match hfy-src-doc-link-style style-string))
           (string-match "} *$" style-string))
      (concat (replace-match hfy-src-doc-link-style
                             'fixed-case
                             'literal
                             style-string) " }") style-string))

;; utility functions - cast emacs style specification values into their
;; css2 equivalents:
(defun hfy-triplet (colour)
  "Takes a colour name \(string\) and return a css rgb(R, G, B) triplet string.
Uses the definition of \"white\" to map the numbers to the 0-255 range, so
if you\'ve redefined white, \(esp if you've redefined it to have a triplet
member lower than that of the colour you are processing, strange things
may happen\)."
  ;;(message "hfy-colour-vals");;DBUG
  (let ((white (mapcar (lambda (I) (float (1+ I))) (hfy-colour-vals "white")))
        (rgb16 (mapcar (lambda (I) (float (1+ I))) (hfy-colour-vals  colour))))
    (if rgb16
        ;;(apply 'format "rgb(%d, %d, %d)"
        ;; Use #rrggbb instead, it is smaller
        (apply 'format "#%02x%02x%02x"
               (mapcar (lambda (X)
                         (* (/ (nth X rgb16)
                               (nth X white)) 255)) '(0 1 2))))) )

(defun hfy-family (family) (list (cons "font-family"  family)))
(defun hfy-bgcol  (colour) (list (cons "background"   (hfy-triplet colour))))
(defun hfy-colour (colour) (list (cons "color"        (hfy-triplet colour))))
(defun hfy-width  (width)  (list (cons "font-stretch" (symbol-name  width))))

(defcustom hfy-font-zoom 1.05
  "Font scaling from Emacs to HTML"
  :type 'float
  :group 'htmlfontify)

(defun hfy-size   (height)
  "Derive a css font-size specifier from an emacs font :height attribute.
Does not cope with the case where height is a function to be applied to
the height of the underlying font."
  (list
   (cond
    ;;(t                 (cons "font-size" ": 1em"))
    ((floatp   height) (cons "font-size" (format "%d%%" (* (* hfy-font-zoom height) 100))))
    ((integerp height) (cons "font-size" (format "%dpt" (/ (* hfy-font-zoom height) 10 ))))
    )) )

(defun hfy-slant  (slant)
  "Derive a font-style css specifier from the emacs :slant attribute -
CSS does not define the reverse-* styles, so just maps those to the
regular specifiers."
  (list (cons "font-style" (cond ((eq 'italic          slant) "italic" )
                                 ((eq 'reverse-italic  slant) "italic" )
                                 ((eq 'oblique         slant) "oblique")
                                 ((eq 'reverse-oblique slant) "oblique")
                                 (t                           "normal" )))) )

(defun hfy-weight (weight)
  "Derive a font-weight css specifier from an emacs weight spec symbol."
  (list (cons "font-weight" (cond ((eq 'ultra-bold  weight) "900")
                                  ((eq 'extra-bold  weight) "800")
                                  ((eq 'bold        weight) "700")
                                  ((eq 'semi-bold   weight) "600")
                                  ((eq 'normal      weight) "500")
                                  ((eq 'semi-light  weight) "400")
                                  ((eq 'light       weight) "300")
                                  ((eq 'extra-light weight) "200")
                                  ((eq 'ultra-light weight) "100")))) )

(defun hfy-box-to-border-assoc (spec)
  (if spec
      (let ((tag (car  spec))
            (val (cadr spec)))
        (cons (cond ((eq tag :color) (cons "colour" val))
                    ((eq tag :width) (cons "width"  val))
                    ((eq tag :style) (cons "style"  val)))
              (hfy-box-to-border-assoc (cddr spec))))) )

(defun hfy-box-to-style (spec)
  (let* ((css (hfy-box-to-border-assoc  spec))
         (col (cdr      (assoc "colour" css)))
         (s   (cdr      (assoc "style"  css))))
    (list
     (if col (cons "border-color" (cdr (assoc "colour" css))))
     (cons "border-width" (format "%dpx" (or (cdr (assoc "width" css)) 1)))
     (cons "border-style" (cond ((eq s 'released-button) "outset")
                                ((eq s 'pressed-button ) "inset" )
                                (t                       "solid" ))))) )

(defun hfy-box (box)
  "Derive CSS border-* attributes from the emacs :box attribute."
  (if box
      (cond
       ((integerp box) (list (cons "border-width" (format "%dpx"   box))))
       ((stringp  box) (list (cons "border" (format "solid %s 1px" box))))
       ((listp    box) (hfy-box-to-style box)                            ))) )

(defun hfy-decor (tag val)
  "Derive CSS text-decoration specifiers from various emacs font attributes."
  (list
   (cond ((eq tag :underline     ) (cons "text-decoration" "underline"   ))
         ((eq tag :overline      ) (cons "text-decoration" "overline"    ))
         ((eq tag :strike-through) (cons "text-decoration" "line-through")))))

(defun hfy-combined-face-spec (face)
  "Return a `defface' style alist of possible specifications for FACE,
with any entries resulting from user customisation \(`custom-set-faces'\)
taking precedence."
  (let ((spec  nil))
    (setq spec (append (or (get face 'saved-face)        (list))
                       (or (get face 'face-defface-spec) (list))))
    (if (and hfy-display-class hfy-default-face-def (eq face 'default))
        (setq spec (append hfy-default-face-def spec))) spec))

(defun hfy-face-attr-for-class (face &optional class)
  "Return the face attributes for FACE. If CLASS is set, it must be a `defface'
alist key \[ see below \], in which case the first face specification
returned by `hfy-combined-face-spec' which *doesn\'t* clash with CLASS is
returned.\n
\(A specification with a class of `t' is considered to match any class you
specify - this matches emacs\' behaviour when deciding on which face attributes
to use, to the best of my understanding\).\n
If CLASS is nil, then you just get get whatever `face-attr-construct' returns,
ie the current specification in effect for FACE.\n
*NOTE* This function forces any face that is not \'default and which has
no :inherit property to inherit from \'default \( this is because \'default
is magical in that emacs fonts behave as if they inherit implicitly from
\'default, but no such behaviour exists in HTML/CSS \).\n
See `hfy-display-class' for details of valid values for CLASS."
  (let ((face-spec nil))
    (setq
     face-spec
     (if class
         (let ((face-props (hfy-combined-face-spec face))
               (face-specn nil)
               (face-class nil)
               (face-attrs nil)
               (face-score  -1)
               (face-match nil))
           (while face-props
             (setq face-specn (car face-props)
                   face-class (car face-specn)
                   face-attrs (cdr face-specn)
                   face-props (cdr face-props))
             ;; if the current element CEL of CLASS is t we match
             ;; if the current face-class is t, we match
             ;; if the cdr of CEL has a non-nil
             ;;   intersection with the cdr of the first member of
             ;;   the current face-class with the same car as CEL, we match
             ;; if we actually clash, then we can't match
             (let ((cbuf class)
                   (cel    nil)
                   (key    nil)
                   (val    nil)
                   (x      nil)
                   (next   nil)
                   (score    0))
               (while (and cbuf (not next))
                 (setq cel  (car cbuf)
                       cbuf (cdr cbuf)
                       key  (car  cel)
                       val  (cdr  cel)
                       val  (if (listp val) val (list val)))
                 (cond
                  ((or (eq t cel) (eq t face-class)) ;; default match
                   (setq score 0) (ignore "t match"))
                  ((not (cdr (assq key face-class))) ;; neither good nor bad
                   nil (ignore "non match, non collision"))
                  ((setq x (hfy-interq val (cdr (assq key face-class))))
                   (setq score (+ score (length x)))
                   (ignore "intersection"))
                  (t                                 ;; nope.
                   (setq next t score -10) (ignore "collision")) ))
               (if (> score face-score)
                   (progn
                     (setq face-match face-attrs
                           face-score score     )
                     (ignore "%d << %S/%S" score face-class class))
                 (ignore "--- %d ---- (insufficient)" score)) ))
           ;; matched ? last attrs : nil
           (if face-match
               (if (listp (car face-match)) (car face-match) face-match) nil))
       ;; Unfortunately the default face returns a
       ;; :background. Fortunately we can remove it, but how do we do
       ;; that in a non-system specific way?
       (let ((spec (face-attr-construct face))
             (new-spec nil))
         (if (not (memq :background spec))
             spec
           (while spec
             (let ((a (nth 0 spec))
                   (b (nth 1 spec)))
               (unless (and (eq a :background)
                            (stringp b)
                            (string= b "SystemWindow"))
                 (setq new-spec (cons a (cons b new-spec)))))
             (setq spec (cddr spec)))
           new-spec))
       ) )
    (if (or (memq :inherit face-spec) (eq 'default face))
        face-spec
      (nconc face-spec (list :inherit 'default)))
    )
  )

;; construct an assoc of (css-tag-name . css-tag-value) pairs
;; from a face or assoc of face attributes:

;; Some tests etc:
;;  (mumamo-message-with-face "testing face" 'highlight)
;;  (mumamo-message-with-face "testing face" '(:foreground "red" :background "yellow"))
;;  (hfy-face-to-style-i '(:inherit default foreground-color "red"))
;;  default face=(:stipple nil :background "SystemWindow" :foreground
;;    "SystemWindowText" :inverse-video nil :box nil :strike-through
;;    nil :overline nil :underline nil :slant normal :weight normal
;;    :height 98 :width normal :family "outline-courier new")
(defun hfy-face-to-style-i (fn)
  "The guts of `hfy-face-to-style': FN should be a `defface' font spec,
as returned by `face-attr-construct' or `hfy-face-attr-for-class'. Note
that this function does not get font-sizes right if they are based on
inherited modifiers \(via the :inherit\) attribute, and any other
modifiers that are cumulative if they appear multiple times need to be
merged by the user - `hfy-flatten-style' should do this."
  ;;(message "hfy-face-to-style-i");;DBUG

  ;; fn's value could be something like
  ;; (:inherit
  ;;  ((foreground-color . "blue"))
  ;;  (foreground-color . "blue")
  ;;  nil)

  (when fn
    (let ((key  (car  fn))
          (val  (cadr fn))
          (next (cddr fn))
          (that       nil)
          (this       nil)
          (parent     nil))
      (if (eq key :inherit)
        (let ((vs (if (listp val) val (list val))))
          ;; (let ((x '(a b))) (setq x (append '(c d) x)))
          ;; (let ((x '(a b))) (setq x (append '(c d) x)))
          (dolist (v vs)
            (setq parent
                  (append
                   parent
                   (hfy-face-to-style-i
                    (hfy-face-attr-for-class v hfy-display-class))
                   ))))
        (setq this
              (if val (cond
                       ((eq key :family        ) (hfy-family    val))
                       ((eq key :width         ) (hfy-width     val))
                       ((eq key :weight        ) (hfy-weight    val))
                       ((eq key :slant         ) (hfy-slant     val))
                       ((eq key :foreground    ) (hfy-colour    val))
                       ((eq key :background    ) (hfy-bgcol     val))
                       ((eq key :box           ) (hfy-box       val))
                       ((eq key :height        ) (hfy-size      val))
                       ((eq key :underline     ) (hfy-decor key val))
                       ((eq key :overline      ) (hfy-decor key val))
                       ((eq key :strike-through) (hfy-decor key val))
                       ((eq key :bold          ) (hfy-weight  'bold))
                       ((eq key :italic        ) (hfy-slant 'italic))))))
      (setq that (hfy-face-to-style-i next))
      ;;(lwarn t :warning "%S => %S" fn (nconc this that parent))
      (nconc this that parent))) )

(defun hfy-size-to-int (spec)
  "Convert SPEC, a css font-size specifier, back to an emacs :height attribute
value. Used while merging multiple font-size attributes."
  ;;(message "hfy-size-to-int");;DBUG
  (list
   (if (string-match "\\([0-9]+\\)\\(%\\|pt\\)" spec)
       (cond ((string= "%"  (match-string 2 spec))
              (/ (string-to-int (match-string 1 spec)) 100.0))
             ((string= "pt" (match-string 2 spec))
              (* (string-to-int (match-string 1 spec))    10)))
     (string-to-number spec))) )

;; size is different, in that in order to get it right at all,
;; we have to trawl the inheritance path, accumulating modifiers,
;; _until_ we get to an absolute (pt) specifier, then combine the lot
(defun hfy-flatten-style (style)
  "Take STYLE (see `hfy-face-to-style-i', `hfy-face-to-style') and merge
any multiple attributes appropriately. Currently only font-size is merged
down to a single occurrence - others may need special handling, but I
haven\'t encountered them yet. Returns a `hfy-style-assoc'."
  ;;(message "hfy-flatten-style");;DBUG
  (let ((n        0)
        (m (list 1))
        (x      nil)
        (r      nil))
    (mapcar
     (lambda (css)
       (if (string= (car css) "font-size")
           (progn
             (if (not x) (setq m (nconc m (hfy-size-to-int (cdr css)))))
             (if (string-match "pt" (cdr css)) (setq x t)))
         (setq r (nconc r (list css))))) style)
    (setq  n (apply '* m))
    (nconc r (hfy-size (if x (round n) (* n 1.0)))) r))

(defun hfy-face-to-style (fn)
  "Take FN, a font or `defface' style font specification,
\(as returned by `face-attr-construct' or `hfy-face-attr-for-class'\)
and return a `hfy-style-assoc'.\n
See also: `hfy-face-to-style-i', `hfy-flatten-style'."
  ;;(message "hfy-face-to-style");;DBUG
  (let ((face-def (if (facep fn)
                      (hfy-face-attr-for-class fn hfy-display-class) fn))
        (final-style nil))

    (setq final-style (hfy-flatten-style (hfy-face-to-style-i face-def)))
    ;;(message "%S" final-style)
    (if (not (assoc "text-decoration" final-style))
        (progn (setq final-style
                     ;; Fix-me: there is no need for this since
                     ;; text-decoration is not inherited.
                     (nconc final-style '(("text-decoration"."none"))))))
    final-style))

;; strip redundant bits from a name. Technically, this could result in
;; a collision, but it is pretty unlikely - will fix later...
;; also handle ephemeral fonts created by overlays, which don't actually
;; have names:
(defun hfy-face-or-def-to-name (fn)
  "Render a font symbol or `defface' font spec into a name \(string\)"
  (if (not (listp fn))
      (format "%s" fn)
    (let* ((key   (format       "%s"        fn))
           (entry (assoc key hfy-tmpfont-stack))
           (base  (cadr   (memq  :inherit  fn)))
           (tag   (cdr                   entry)))
      (if entry nil ;; noop
        (setq tag               (format "%04d" (length hfy-tmpfont-stack))
              entry             (cons key tag)
              hfy-tmpfont-stack (cons entry hfy-tmpfont-stack)))
      ;;(message "%S <- %S" base fn)
      (format "%s-%s" (or base 'default) tag)) ))

(defun hfy-css-name (fn)
  "Strip some of the boring bits from a font-name and return a css style name."
  ;;(message "hfy-css-name");;DBUG
  (let ((face-name (hfy-face-or-def-to-name fn)))
    (if (or (string-match "font-lock-\\(.*\\)" face-name)
            (string-match "cperl-\\(.*\\)"     face-name)
            (string-match "^[Ii]nfo-\\(.*\\)"   face-name))
        (progn
          (setq face-name (match-string 1 face-name))
          (if (string-match "\\(.*\\)-face$" face-name)
              (setq face-name (match-string 1 face-name))) face-name)
      face-name)) )

;; construct an assoc of (stripped-name . "{ css-stuff-here }") pairs
;; from a face:
(defun hfy-face-to-css (fn)
  "Take FN, a font or `defface' specification \(cf. `face-attr-construct'\)
and return a CSS style specification.

See also: `hfy-face-to-style'"
  ;;(message "hfy-face-to-css");;DBUG
  (let ((css-list nil)
        (css-text nil)
        (style    nil)
        (seen     nil))
    (setq css-list (hfy-face-to-style fn))
    (setq css-text
          (nconc
           (mapcar
            (lambda (E)
              (if (car E)
                    (if (not (member (car E) seen))
                        (progn
                          (setq seen (cons (car E) seen))
                          (format " %s: %s; " (car E) (cdr E)))))) css-list)))
    (cons (hfy-css-name fn) (format "{%s}" (apply 'concat css-text)))) )

;; extract a face from a list of char properties, if there is one:
(defun hfy-p-to-face-old (props)
  "Given PROPS, a list of text-properties, return the value of the face
property, or nil."
  (if props
      (if (string= (car props) "face")
          (if (listp (cadr props)) (car (cadr props)) (cadr props))
        (hfy-p-to-face (cddr props))) nil))

(defun hfy-p-to-face (props)
  "Given PROPS, a list of text-properties, return the value of the face
property, or nil."
  (when props
    (let ((face (plist-get props 'face))
          (font-lock-face (plist-get props 'font-lock-face))
          (button (plist-get props 'button))
          ;;(face-rec (memq 'face props))
          ;;(button-rec (memq 'button props)))
          )
      (if button
          (let* ((category (plist-get props 'category))
                 (face (when category (plist-get (symbol-plist category) 'face))))
            face)
        (if font-lock-face
            font-lock-face
          face)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hfy-get-face-at (pos)
;;   (let ((face (get-char-property-and-overlay pos 'face)))
;;     (when (and face (listp face)) (setq face (car face)))
;;     (unless (listp face)
;;       face)))
  ;;(get-char-property pos 'face)
  ;; Overlays are handled later
  (if (or (not show-trailing-whitespace)
          (not (get-text-property pos 'hfy-show-trailing-whitespace)))
      (get-text-property pos 'face)
    (list 'trailing-whitespace (get-text-property pos 'face)))
  )

;; Fix-me: This function needs some cleanup by someone who understand
;; all the formats that face properties can have.
;;
;; Fix-me: The handling of overlays is maybe not optimal. I have just
;; handled them as if they were text properties.
(defun hfy-face-at (p)
  "Find face in effect at point P. If overlays are to be considered
\(see `hfy-optimisations'\) then this may return a defface style
list of face properties instead of a face symbol"
  ;;(message "hfy-face-at");;DBUG
  ;; Fix-me: clean up, remove face-name etc
 (unless (and (not hfy-hide-invis)
             (hfy-line-move-invisible-p p))
  (let ((overlay-data nil)
        ;;(face-name   (hfy-p-to-face (text-properties-at p)))
        (face-name    (hfy-get-face-at p))
        (prop-seen    nil)
        (found-face   nil)
        (extra-props  nil)
        (text-props   (text-properties-at p))
        ;;(face-names   nil)
        )
    ;; Fix-me: hfy-get-face-at seems to return too much now, or?
    ;; Result: ((foreground-color . "red"))
    (when (listp face-name)
      ;;(setq text-props (cons 'face face-name))
      (dolist (f face-name)
        ;; Fix-me: chaged the test to "t" 2008-10-16 because in
        ;; list-colors-display you now have text properties like
        ;; (:background "snow").
        (if t ;(listp f) ;; for things like (variable-pitch (:foreground "red"))
            (setq extra-props (cons f extra-props))
          (setq extra-props (cons :inherit (cons f extra-props)))))
      (setq face-name nil))
    ;;(setq face-name nil)
    ;; text-properties-at format=(face (:foreground "red" :background "yellow"))
    ;;                      or  =(face (compilation-info underline)) ie a list of faces

    ;; overlay-properties format=(evaporate t face ((foreground-color . "red")))
    (if (and (or (not (hfy-opt                      'keep-overlays))
                 (not (setq overlay-data (hfy-overlay-props-at p))))
             nil
             (not text-props))
        face-name ;; overlays not wanted, or none to consider
      (when text-props
        (setq overlay-data (cons text-props overlay-data))
        ;;(setq overlay-data (reverse (cons text-props (reverse overlay-data))))
        )
      (when face-name
        (setq extra-props (list :inherit face-name)))
      (mapcar
       (lambda (P)
         (let ((fprops (cadr (or (memq 'face P)
                                 (memq 'font-lock-face P)))))
           (if (not (listp fprops))
;;                (if (not found-face)
;;                    (setq face-name (if (stringp fprops) (intern fprops) fprops)
;;                          found-face t))
               (setq extra-props
                     (cons :inherit
                           (cons (if (stringp fprops) (intern fprops) fprops)
                                 extra-props)))
             (while fprops
               (if (facep (car fprops))
                   (let ((face (car fprops)))
                     (when (stringp face) (setq face (intern fprops)))
                     (setq extra-props
                           (cons :inherit
                                 (cons face
                                     extra-props)))
                     (setq fprops (cdr fprops)))
                 (let (p v)
                   ;; Sigh.
                   (if (listp (car fprops))
                       (if (nlistp (cdr (car fprops)))
                           (progn
                             ;; ((prop . val))
                             (setq p (caar fprops))
                             (setq v (cdar fprops))
                             (setq fprops (cdr fprops)))
                         ;; ((prop val))
                         (setq p (caar fprops))
                         (setq v (cadar fprops))
                         (setq fprops (cdr fprops)))
                     (if (listp (cdr fprops))
                         (progn
                           ;; (:prop val :prop val ...)
                           (setq p (car fprops))
                           (setq v (cadr fprops))
                           (setq fprops (cddr fprops)))
                       (if (and (listp fprops)
                                (not (listp (cdr fprops))))
                           ;;(and (consp x) (cdr (last x)))
                           (progn
                             ;; (prop . val)
                             (setq p (car fprops))
                             (setq v (cdr fprops))
                             (setq fprops nil))
                         (error "Eh..., another format! fprops=%s" fprops)
                         )
                       ;;(if (listp (cdr fprops)) (setq fprops (cddr fprops)) (setq fprops nil))
                       ))
                   (setq p (case p
                             ;; These are all the properties handled
                             ;; in `hfy-face-to-style-i'.
                             ;;
                             ;; Fix-me: Are these translations right?
                             ('family           :family    )
                             ('width            :width     )
                             ('height           :height    )
                             ('weight           :weight    )
                             ('slant            :slant     )
                             ('underline        :underline )
                             ('overline         :overline  )
                             ('strike-through   :strike-through)
                             ('box              :box       )
                             ('foreground-color :foreground)
                             ('background-color :background)
                             ('bold             :bold      )
                             ('italic           :italic    )
                             (t                 p)))
                   (if (memq p prop-seen) nil ;; noop
                     (setq prop-seen   (cons p prop-seen)
                           found-face  t
                           extra-props (cons p (cons v extra-props)))))
                 )))))
       overlay-data)
      (if extra-props ;found-face
          ;;(cons :inherit (cons (or face-name 'default) extra-props))
;;           (let ((face-names (if (listp face-name)
;;                                 face-name
;;                               (list (if face-name face-name 'default))))
;;                 (ret extra-props))
;;             (mapc (lambda(f)
;;                     (setq ret (cons f ret))
;;                     (setq ret (cons :inherit ret)))
;;                   face-names)
;;             ret)
          extra-props
        face-name)
      )
    )
  )
)

(defun hfy-overlay-props-at (p)
  "Grab overlay properties at point P, plists are returned in
descending priority order"
  (sort (mapcar (lambda (O) (overlay-properties O)) (overlays-at p))
        (lambda (A B) (> (or (cadr (memq 'priority A)) 0)
                         (or (cadr (memq 'priority B)) 0)) ) ) )

;; construct an assoc of (face-name . (css-name . "{ css-style }")) elements:
(defun hfy-compile-stylesheet ()
  "Trawl the current buffer, construct and return a `hfy-sheet-assoc'."
  ;;(message "hfy-compile-stylesheet");;DBUG
  (let ((pt (point-min))
        ;; Make the font stack stay:
        ;;(hfy-tmpfont-stack nil)
        (fn         nil)
        (css        nil)
        (style      nil))
    (save-excursion
      (goto-char pt)
      (while (< pt (point-max))
        (if (and (setq fn (hfy-face-at pt)) (not (assoc fn style)))
            (setq style (cons (cons fn (hfy-face-to-css fn)) style)))
        (setq pt (next-char-property-change pt))) )
    (setq style (cons (cons 'default (hfy-face-to-css 'default)) style))) )

(defun hfy-fontified-p ()
  "`font-lock' doesn't like to say it\'s been fontified when in batch
mode, but we want to know if we should fontify or raw copy, so in batch
mode we check for non-default face properties. Otherwise we test
`font-lock-mode' and `font-lock-fontified' for truth."
  ;;(message "font-lock-fontified: %S" font-lock-fontified)
  ;;(message "noninteractive     : %S" noninteractive)
  ;;(message "font-lock-mode     : %S" font-lock-mode)
  (and font-lock-fontified
       (if noninteractive
           (let ((pt  (point-min))
                 (face-name   nil))
             (save-excursion
               (goto-char pt)
               (while (and (< pt (point-max)) (not face-name))
                 (setq face-name (hfy-face-at pt))
                 (setq pt (next-char-property-change pt)))) face-name)
         font-lock-mode)))

;; remember, the map is in reverse point order:
;; I wrote this while suffering the effects of a cold, and maybe a
;; mild fever - I think it's correct, but it might be a little warped
;; as my minfd keeps ... where was I? Oh yes, the bunnies...
(defun hfy-merge-adjacent-spans (face-map)
  "Where FACE-MAP is a `hfy-facemap-assoc' for the current buffer,
this function merges adjacent style blocks which are of the same value
and are separated by nothing more interesting than whitespace.\n
  <span class=\"foo\">narf</span> <span class=\"foo\">brain</span>\n
\(as interpreted from FACE-MAP\) would become:\n
  <span class=\"foo\">narf brain</span>\n
Returns a modified copy of FACE-MAP."
  (let ((tmp-map face-map)
        (map-buf      nil)
        (first-start  nil)
        (first-stop   nil)
        (last-start   nil)
        (last-stop    nil)
        (span-stop    nil)
        (span-start   nil)
        (reduced-map  nil))
    ;;(setq reduced-map (cons (car  tmp-map) reduced-map))
    ;;(setq reduced-map (cons (cadr tmp-map) reduced-map))
    (while tmp-map
      (setq first-start (cadddr tmp-map)
            first-stop  (caddr  tmp-map)
            last-start  (cadr   tmp-map)
            last-stop   (car    tmp-map)
            map-buf      tmp-map
            span-start   last-start
            span-stop    last-stop      )
      (while (and (equal (cdr first-start)
                         (cdr  last-start))
                  (save-excursion
                    (goto-char (car first-stop))
                    (not (re-search-forward "[^ \t\n\r]" (car last-start) t))))
        (setq map-buf     (cddr map-buf)
              span-start  first-start
              first-start (cadddr map-buf)
              first-stop  (caddr  map-buf)
              last-start  (cadr   map-buf)
              last-stop   (car    map-buf)))
      (setq reduced-map (cons span-stop  reduced-map))
      (setq reduced-map (cons span-start reduced-map))
      (setq tmp-map (memq last-start tmp-map))
      (setq tmp-map (cdr tmp-map)))
    (setq reduced-map (nreverse reduced-map))))

;; remember to generate 'synthetic' </span> entries -
;; emacs copes by just having a stack of styles in effect
;; and only using the top one: html has a more simplistic approach -
;; we have to explicitly end a style, there's no way of temporarily
;; overriding it w. another one... (afaik)
(defun hfy-compile-face-map ()
;; Fix-me: no need for special <a> version.
;; Fix-me: save table for multi-buffer
  "Compile and return a `hfy-facemap-assoc' for the current buffer."
  ;;(message "hfy-compile-face-map");;DBUG
  (let ((pt (point-min))
        (pt-narrow  1)
        (fn         nil)
        (map        nil)
        (prev-tag   nil)) ;; t   if the last tag-point was a span-start
                          ;; nil if it was a span-stop
    (save-excursion
      (goto-char pt)
      (while (< pt (point-max))
        (if (setq fn (hfy-face-at pt))
            (progn (if prev-tag (setq map (cons (cons pt-narrow 'end) map)))
                   (setq map (cons (cons pt-narrow fn) map))
                   (setq prev-tag t))
          (if prev-tag (setq map (cons (cons pt-narrow 'end) map)))
          (setq prev-tag nil))
        (setq pt (next-char-property-change pt))
        (setq pt-narrow (1+ (- pt (point-min))))
        )
      (if (and map (not (eq 'end (cdar map))))
          (setq map (cons (cons (- (point-max) (point-min)) 'end) map))))
    (if (hfy-opt 'merge-adjacent-tags) (hfy-merge-adjacent-spans map) map)))

(defun hfy-buffer ()
  "Generate a buffer to hold the html output. The filename of this buffer is
derived from the source \(current\) buffer\'s `buffer-file-name', if it is
set, plus `hfy-extn'. Otherwise a plausible filename is constructed from
`default-directory', `buffer-name' and `hfy-extn'."
  (let* ((name (concat (buffer-name) hfy-extn))
         (src               (buffer-file-name))
         (buf  (get-buffer-create        name)))
    (save-excursion
      (set-buffer buf)
      (if src (setq buffer-file-name (concat src hfy-extn))
        (if (string-match "^.*/\\([^/]*\\)$" name)
            (setq buffer-file-name
                  (concat default-directory "/" (match-string 1 name)))
          (setq buffer-file-name (concat default-directory "/" name) )))
      buf)))

;; get a css style name for a face from the style:
(defun hfy-lookup (face style) (cadr (assoc face style)))

(defun hfy-link-style (style-string)
  ;;(message "hfy-link-style");;DBUG
  (if (functionp hfy-link-style-fun)
      (funcall hfy-link-style-fun style-string)
    style-string))

;; barf up the inline css stylesheet
(defun hfy-sprintf-stylesheet (css file)
  ;;(message "hfy-sprintf-stylesheet");;DBUG
  (let ((stylesheet nil))
    (setq stylesheet
          (concat
           hfy-meta-tags
           "\n<style type=\"text/css\"><!-- \n"
           ;; Fix-me: Add handling of page breaks here + scan for ^L
           ;; where appropriate.
           (format "body %s\n" (cddr (assq 'default css)))
           (apply 'concat
                  (mapcar
                   (lambda (style)
                     (format
                      "span.%s   %s\nspan.%s a %s\n"
                      (cadr style) (cddr style)
                      (cadr style) (hfy-link-style (cddr style)))) css))
           " --></style>\n"))
    (funcall hfy-page-header file stylesheet)))

(defconst hfy-javascript "
    <script type=\"text/javascript\">
      // <![CDATA[

function getObj(name) {
    if (document.getElementById) {
        this.obj = document.getElementById(name);
        this.style = document.getElementById(name).style;
    }
}
function hfy_toggle_display(name) {
    var x = new getObj(\"hfy_invis_\" + name);
    var flag = x.style.display == 'inline';
    x.style.display = (flag) ? 'none' : 'inline'
}

      // ]]>
    </script>\n")

;; tag all the dangerous characters we want to escape
;; (ie any "<> chars we _didn't_ put there explicitly for css markup)
(defun hfy-html-enkludge-buffer ()
  "Mark dangerous [\"\<\>] characters with the \'hfy-quoteme property.\n
See also `hfy-html-dekludge-buffer'."
  ;;(message "hfy-html-enkludge-buffer");;DBUG
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward hfy-html-quote-regex nil t)
      (put-text-property (match-beginning 0) (point) 'hfy-quoteme t))) )

;; dangerous char -> &entity;
(defun hfy-html-quote (char-string)
  "Map a string (usu. 1 char long) to an html safe string (entity) if need be."
  ;;(message "hfy-html-quote");;DBUG
  (or (cadr (assoc char-string hfy-html-quote-map)) char-string) )

;; actually entity-ise dangerous chars.
;; note that we can't do this until _after_ we have inserted the css
;; markup, since we use a position-based map to insert this, and if we
;; enter any other text before we do this, we'd have to track another
;; map of offsets, which would be tedious...
(defun hfy-html-dekludge-buffer ()
  "Transform all dangerous characters marked with the \'hfy-quoteme property
using `hfy-html-quote'\n
See also `hfy-html-enkludge-buffer'."
  ;;(message "hfy-html-dekludge-buffer");;DBUG
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward hfy-html-quote-regex nil t)
      (if (get-text-property (match-beginning 0) 'hfy-quoteme)
          (replace-match (hfy-html-quote (match-string 1)))
        )
      )
    )
  )

;; (hfy-line-move-invisible-p (point))
(defun hfy-line-move-invisible-p (pos)
  "Return non-nil if the character after POS is currently invisible."
  (let ((prop
         (get-char-property pos 'invisible)))
    (if (eq buffer-invisibility-spec t)
        prop
      (if (listp prop)
          (catch 'invis
            (dolist (p prop)
              (when (or (memq p buffer-invisibility-spec)
                        (assq p buffer-invisibility-spec))
                (throw 'invis t))))
        (or (memq prop buffer-invisibility-spec)
            (assq prop buffer-invisibility-spec))))))

(defun hfy-line-move-invisible2-p (pos buffer-invisibility-spec)
  (hfy-line-move-invisible-p pos))

;; (setq hfy-hide-invis t)
(defcustom hfy-hide-invis nil
  "DON'T USE THIS!
If true then hide invisible text instead of deleting it.

NOTE: This was probably a bad idea. I can not see how I can get
this to work."
  :type 'boolean
  :group 'htmlfontify)

(defun hfy-insert-keeping-invisible (text)
  (let ((invis (get-text-property (max (point-min) (1- (point))) 'invisible)))
    (insert (propertize text 'invisible invis))))

;; Borrowed from font-lock.el
(defmacro hfy-save-buffer-state (varlist &rest body)
  "Bind variables according to VARLIST and eval BODY restoring buffer state.
Do not record undo information during evaluation of BODY."
  (declare (indent 1) (debug let))
  (let ((modified (make-symbol "modified")))
    `(let* ,(append varlist
                    `((,modified (buffer-modified-p))
                      (buffer-undo-list t)
                      (inhibit-read-only t)
                      (inhibit-point-motion-hooks t)
                      (inhibit-modification-hooks t)
                      deactivate-mark
                      buffer-file-name
                      buffer-file-truename))
       (progn
         ,@body)
       (unless ,modified
         (restore-buffer-modified-p nil)))))

(defun hfy-mark-trailing-whitespace ()
  (when show-trailing-whitespace
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (hfy-save-buffer-state nil
          (while (re-search-forward "[ \t]+$" nil t)
            (put-text-property (match-beginning 0) (match-end 0)
                                   'hfy-show-trailing-whitespace t)))))))

(defun hfy-unmark-trailing-whitespace ()
  (when show-trailing-whitespace
    (hfy-save-buffer-state nil
      (remove-text-properties (point-min) (point-max)
                              '(hfy-show-trailing-whitespace)))))

(defun hfy-fontify-buffer (&optional srcdir file)
  "Implement the guts of `htmlfontify-buffer'"
  (if srcdir (setq srcdir (directory-file-name srcdir)))
  (let* ((in-style                                   nil)
         ;;(fast-lock-minimum-size      hfy-fast-lock-save)
         (orig-buffer           (current-buffer))
         (orig-ovls             (overlays-in (point-min) (point-max)))
         (html-buffer           (hfy-buffer            ))
         (invis-spec            buffer-invisibility-spec)
         (css-sheet             nil) ;;(hfy-compile-stylesheet))
         (css-map               nil) ;;(hfy-compile-face-map  ))
         (rmin (when mark-active (region-beginning)))
         (rmax (when mark-active (region-end)))
         (rovl nil)
         )
    (when (and mark-active
               transient-mark-mode)
      (unless (and (= rmin (point-min))
                   (= rmax (point-max)))
        (setq rovl (make-overlay rmin rmax))
        (overlay-put rovl 'priority 1000)
        (overlay-put rovl 'face 'region)))
    ;; copy the buffer, including fontification, and switch to it:
    (hfy-mark-trailing-whitespace)
    (setq css-sheet             (hfy-compile-stylesheet))
    (setq css-map               (hfy-compile-face-map  ))
    (hfy-unmark-trailing-whitespace)
    (when rovl
      (delete-overlay rovl))
    (copy-to-buffer html-buffer (point-min) (point-max))
    (set-buffer     html-buffer)
    ;; Apply overlay invisible spec
    (setq orig-ovls
          (sort orig-ovls
                (lambda (A B) (> (or (cadr (memq 'priority (overlay-properties A))) 0)
                                 (or (cadr (memq 'priority (overlay-properties B))) 0)))))
    (dolist (ovl orig-ovls)
      (let ((invis (overlay-get ovl 'invisible)))
        (when invis
          (add-text-properties (overlay-start ovl) (overlay-end ovl) (list 'invisible invis)))))
    ;;(setq buffer-invisibility-spec invis-spec)
    (setq buffer-invisibility-spec nil)
    ;;(message "in new html-buffer")
    ;; at this point, html-buffer retains the fontification of the parent:
    ;; #####################################################################
    ;; if we are in etags mode, add properties to mark the anchors and links
    (if (and srcdir file)
        (progn
          (hfy-mark-tag-names srcdir file) ;; mark anchors
          (hfy-mark-tag-hrefs srcdir file))) ;; mark links
    ;; #####################################################################
    ;; mark the 'dangerous' characters
    ;;(message "marking dangerous characters")
    (hfy-html-enkludge-buffer)
    ;; trawl the position-based face-map, inserting span tags as we go
    ;; note that we cannot change any character positions before this point
    ;; or we will invalidate the map:
    ;; NB: This also means we have to trawl the map in descending file-offset
    ;; order, obviously.
    ;; ---------------------------------------------------------------------
    ;; Remember, inserting pushes properties to the right, which we don't
    ;; actually want to happen for link properties, so we have to flag
    ;; them and move them by hand - if you don't, you end up with
    ;;
    ;; <span class="foo"><a href="bar">texta</span><span class="bletch"></a>...
    ;;
    ;; instead of:
    ;;
    ;; <span class="foo"><a href="bar">texta</a></span><span class="bletch">...
    ;;
    ;; If my analysis of the problem is correct, we can detect link-ness by
    ;; either hfy-linkp or hfy-endl properties at the insertion point, but I
    ;; think we only need to relocate the hfy-endl property, as the hfy-linkp
    ;; property has already served its main purpose by this point.
    ;;(message "mapcar over the CSS-MAP")
    (mapcar
     (lambda (point-face)
       (let ((pt (car point-face))
             (fn (cdr point-face))
             (move-link       nil))
         (goto-char pt)
         (setq move-link
               (or (get-text-property pt 'hfy-linkp)
                   (get-text-property pt 'hfy-endl )))
         (if (eq 'end fn)
             (hfy-insert-keeping-invisible "</span>")
           (if (not (and srcdir file))
               nil
             ;;(insert "</span>")
             ;;(insert "</span>")
             ;; (message "move-link: %S, pt: %d, point: %d, PROPS: %S"
             ;;          move-link pt (point) PROPS )
             (if (not move-link) nil
               ;;(message "removing props @ %d" (point))
               (remove-text-properties (point) (1+ (point)) '(hfy-endl nil))
               (put-text-property pt (1+ pt) 'hfy-endl t)
               )
             )
           ;;(insert (format "<span class=\"%s\">"
           (hfy-insert-keeping-invisible (format "<span class=\"%s\">"
                                                 (hfy-lookup fn css-sheet)))
           (if (not move-link) nil
             ;;(message "removing prop2 @ %d" (point))
             (if (remove-text-properties (point) (1+ (point)) '(hfy-endl nil))
                 (put-text-property pt (1+ pt) 'hfy-endl t))) )))
     css-map)
    ;; #####################################################################
    ;; Invisibility
    ;; Fix-me: Maybe just make the text invisible in XHTML?
    (let ((pos (point-min))
          (next-pos nil)
          (next-invis nil)
          (invis-id-num 0)
          (done-invis nil)
          pending-ends
          prev-invis)
      (while (setq next-pos (next-single-property-change pos 'invisible))
        (if (not next-invis)
            (progn
             (while pending-ends
               (let ((pend-pos (car pending-ends)))
                 (goto-char pend-pos)
                 (insert "</span>"))
               (setq pending-ends (cdr pending-ends)))
             (setq prev-invis next-invis)
             (setq next-invis (hfy-line-move-invisible2-p next-pos invis-spec))
             (setq pos next-pos)
             )
          ;; (setq hfy-hide-invis nil)
          ;; (setq hfy-hide-invis t)
          (if (not hfy-hide-invis)
              (progn
                (delete-region pos next-pos)
                (setq next-pos pos)
                ;; How to show ellipsis?
                (when (listp next-invis)
                  (goto-char pos)
                  (insert " …")
                  (insert " ...")
                  )
                (setq prev-invis next-invis)
                (setq next-invis (hfy-line-move-invisible2-p next-pos invis-spec))
                (setq pos next-pos)
                )
            ;; fix-me: something is wrong with the logic here. The
            ;; nesting gets wrong. A recursive solution seems most
            ;; easy. Step down every time a invisible part is
            ;; found. However there is AFAICS no guarantee that this
            ;; will be correct either. The marking of the text and how
            ;; hiding is handled may be totally unrelated. So I
            ;; suspect I have better give up here, or?
            ;;
            ;; The best that could be done is probably collecting
            ;; successive chunks of invisible text into one chunk, but
            ;; ... - Maybe the diff in 'invisible prop can be used
            ;; too? (Even though there is no guarantee.)
            ;;
            ;; Seems to be useful at least for `org-mode', but only
            ;; gives one level of hiding so far. Add some hook for
            ;; org-mode to take care of?
            (if done-invis
                nil ;;(setq next-pos pos)
              ;;(setq done-invis t)
              (setq prev-invis next-invis)
              (setq next-invis (hfy-line-move-invisible2-p next-pos invis-spec))
              (goto-char next-pos)
              (setq next-pos (point-marker))
              (goto-char pos)
              (setq pos (point-marker))
              (set-marker-insertion-type pos nil)
              (insert (format "<a href=\"javascript:hfy_toggle_display(%s)\">...</a>" invis-id-num))
              (insert (format "<span id=\"hfy_invis_%s\" style=\"display:none\">" invis-id-num))
              (setq pos (point))
              (setq invis-id-num (1+ invis-id-num))
              ;;(goto-char next-pos) (insert "</span>")
              (setq pending-ends (cons next-pos pending-ends))
              ))
          )
        )
      (when next-invis
        (delete-region pos (point-max))))
    ;; #####################################################################
    ;; (message "checking to see whether we should link...")
    (if (and srcdir file)
        (let ((lp 'hfy-link)
              (pt  nil)
              (pr  nil)
              (rr  nil))
          ;; (message "  yes we should.")
            ;; translate 'hfy-anchor properties to anchors
            (setq pt (point-min))
            (while (setq pt (next-single-property-change pt 'hfy-anchor))
              (if (setq pr (get-text-property pt 'hfy-anchor))
                  (progn (goto-char pt)
                         (remove-text-properties pt (1+ pt) '(hfy-anchor nil))
                         (insert (concat "<a name=\"" pr "\"></a>")))))
            ;; translate alternate 'hfy-link and 'hfy-endl props to opening
            ;; and closing links. (this should avoid those spurious closes
            ;; we sometimes get by generating only paired tags)
            (setq pt (point-min))
            (while (setq pt (next-single-property-change pt lp))
              (if (not (setq pr (get-text-property pt lp))) nil
                (goto-char pt)
                (remove-text-properties pt (1+ pt) (list lp nil))
                (cond
                 ((eq lp 'hfy-link)
                  (if (setq rr (get-text-property pt 'hfy-inst))
                      (insert (format "<a name=\"%s\"></a>" rr)))
                  (insert (format "<a href=\"%s\">" pr))
                  (setq lp 'hfy-endl))
                 ((eq lp 'hfy-endl)
                  (insert "</a>") (setq lp 'hfy-link)) ))) ))

    ;; #####################################################################
    ;; transform the dangerous chars. This changes character positions
    ;; since entities have > char length.
    ;; note that this deletes the dangerous characters, and therefore
    ;; destroys any properties they may contain (such as 'hfy-endl),
    ;; so we have to do this after we use said properties:
    ;; (message "munging dangerous characters")
    (hfy-html-dekludge-buffer)
    ;; insert the stylesheet at the top:
    (goto-char (point-min))
    ;;(message "inserting stylesheet")
    (insert (hfy-sprintf-stylesheet css-sheet file))
    (insert hfy-javascript)
    (if (hfy-opt 'div-wrapper) (insert "<div class=\"default\">"))
    (insert "\n<pre>")
    (goto-char (point-max))
    (insert "</pre>\n")
    (if (hfy-opt 'div-wrapper) (insert "</div>"))
    ;;(message "inserting footer")
    (insert (funcall hfy-page-footer file))
    ;; call any post html-generation hooks:
    (run-hooks 'hfy-post-html-hooks)
    ;; return the html buffer
    (set-buffer-modified-p nil)
    html-buffer))

(defun hfy-force-fontification ()
  (mapcar (lambda (fun) (funcall fun)) hfy-init-kludge-hooks)
  (eval-and-compile (require 'font-lock))
  (if (boundp 'font-lock-cache-position)
      (or font-lock-cache-position
          (set 'font-lock-cache-position (make-marker))))
  (if (not noninteractive)
      (progn
        (message "hfy interactive mode (%S %S)" window-system major-mode)
        (when (and font-lock-defaults
                   font-lock-mode)
          (font-lock-fontify-region (point-min) (point-max) nil)))
    (message "hfy batch mode (%s:%S)"
             (or (buffer-file-name) (buffer-name)) major-mode)
    (when font-lock-defaults
      (font-lock-fontify-buffer))
    )
  )

;;;###autoload
(defun htmlfontify-buffer (&optional srcdir file)
  "Create a new buffer, named for the current buffer + a .html extension,
containing an inline css-stylesheet and formatted css-markup html
that reproduces the look of the current emacs buffer as closely
as possible.

Dangerous characters in the existing buffer are turned into html
entities, so you should even be able to do html-within-html
fontified display.

You should, however, note that random control or eight-bit
characters such as ^L (\x0c) or ¤ (\xa4) won't get mapped yet.

If the SRCDIR and FILE arguments are set, lookup etags derived
entries in the `hfy-tags-cache' and add html anchors and
hyperlinks as appropriate."
  (interactive)
  ;; pick up the file name in case we didn't receive it
  (if (not file)
      (progn (setq file (or (buffer-file-name) (buffer-name)))
             (if (string-match "/\\([^/]*\\)$" file)
                 (setq file (match-string 1 file)))) )

  (if (not (hfy-opt 'skip-refontification))
      (save-excursion ;; Keep region
        (hfy-force-fontification)))
  (if (interactive-p) ;; display the buffer in interactive mode:
      (switch-to-buffer (hfy-fontify-buffer srcdir file))
    (hfy-fontify-buffer srcdir file)))

;; recursive file listing
(defun hfy-list-files (directory)
  "Return a list of files under DIRECTORY.
Strips any leading \"./\" from each filename."
  ;;(message "hfy-list-files");;DBUG
  (cd directory)
  (mapcar (lambda (F) (if (string-match "^./\\(.*\\)" F) (match-string 1 F) F))
          (split-string (shell-command-to-string hfy-find-cmd))) )

;; strip the filename off, return a directiry name
;; not a particularly thorough implementaion, but it will be
;; fed pretty carefully, so it should be Ok:
(defun hfy-dirname (file)
  "Return everything preceding the last \"/\" from a relative filename,
on the assumption that this will produce a relative directory name. Hardly
bombproof, but good enough in the context in which it is being used."
  ;;(message "hfy-dirname");;DBUG
  (let ((f (directory-file-name file)))
    (and (string-match "^\\(.*\\)/" f) (match-string 1 f))))

;; create a directory, cf mkdir -p
(defun hfy-make-directory (dir)
  "Approx equivalent of mkdir -p DIR"
  ;;(message "hfy-make-directory");;DBUG
  (if (file-exists-p dir)
      (if (file-directory-p dir) t)
    (make-directory dir t)))

(defun hfy-text-p (srcdir file)
  "Is SRCDIR/FILE text? Uses `hfy-istext-command' to determine this."
  (let (cmd rsp)
    (setq cmd (format hfy-istext-command (concat srcdir "/" file))
          rsp (shell-command-to-string    cmd))
    (if (string-match "text" rsp) t nil)))

;; open a file, check fontification, if fontified, write a fontified copy
;; to the destination directory, otherwise just copy the file:
(defun hfy-copy-and-fontify-file (srcdir dstdir file)
  "open FILE in SRCDIR - if fontified, write a fontified copy to DSTDIR
adding an extension of `hfy-extn'. Fontification is actually done by
`htmlfontify-buffer'. If the buffer is not fontified, just copy it."
  ;;(message "hfy-copy-and-fontify-file");;DBUG
  (let (;;(fast-lock-minimum-size      hfy-fast-lock-save)
        ;;(font-lock-support-mode         'fast-lock-mode)
        ;;(window-system  (or window-system 'htmlfontify))
        (target nil)
        (source nil)
        (html   nil))
    (cd srcdir)
    (save-excursion
      (setq source (find-file-noselect file))
      (set-buffer   source)
      (setq target (concat dstdir "/" file))
      (hfy-make-directory (hfy-dirname target))
      (if (not (hfy-opt 'skip-refontification)) (hfy-force-fontification))
      (if (or (hfy-fontified-p) (hfy-text-p srcdir file))
          (progn (setq html  (hfy-fontify-buffer srcdir file))
                 (set-buffer  html)
                 (write-file (concat target hfy-extn))
                 (kill-buffer html))
        ;; #o0200 == 128, but emacs20 doesn't know that
        (if (and (file-exists-p target) (not (file-writable-p target)))
            (set-file-modes target (logior (file-modes target) 128)))
        (copy-file (buffer-file-name source) target 'overwrite))
      (kill-buffer source))
    )
  )

;; what line are we on?
(defun hfy-line-number ()
  ;;(message "hfy-line-number");;DBUG
  (let ((opoint (point)) start)
    (save-excursion
      (goto-char (point-min))
      (forward-line     0)
      (setq start (point))
      (goto-char   opoint)
      (forward-line 0)
      (1+ (count-lines 1 (point))))))

;; list of tags in file in srcdir
(defun hfy-tags-for-file (srcdir file)
  "List of etags tags that have definitions in this FILE. Looks up
the tags cache in `hfy-tags-cache' using SRCDIR as the key."
  ;;(message "hfy-tags-for-file");;DBUG
  (let ((cache-entry (assoc srcdir hfy-tags-cache))
        (cache-hash   nil)
        (tag-list     nil))
    (if (setq cache-hash (cadr cache-entry))
        (maphash
         (lambda (K V)
           (if (assoc file V)
               (setq tag-list (cons K tag-list)))) cache-hash))
    tag-list))

;; mark the tags native to this file for anchors
(defun hfy-mark-tag-names (srcdir file)
  "Mark tags in FILE (lookup SRCDIR in `hfy-tags-cache') with the \'hfy-anchor
property, with a value of \"tag.line-number\"."
  ;;(message "(hfy-mark-tag-names %s %s)" srcdir file);;DBUG
  (let ((cache-entry (assoc srcdir hfy-tags-cache))
        (cache-hash   nil))
    (if (setq cache-hash (cadr cache-entry))
        (mapcar
         (lambda (TAG)
           (mapcar
            (lambda (TLIST)
              (if (string= file (car TLIST))
                  (let* ((line              (cadr TLIST) )
                         (chr              (caddr TLIST) )
                         (link (format "%s.%d" TAG line) ))
                    (put-text-property (+ 1 chr)
                                       (+ 2 chr)
                                       'hfy-anchor link))))
            (gethash TAG cache-hash)))
         (hfy-tags-for-file srcdir file)))))

(defun hfy-relstub (file &optional start)
  "Return a \"../\" stub of the appropriate length for the current source
tree depth \(as determined from FILE\). iyswim."
  ;;(message "hfy-relstub");;DBUG
  (let ((c ""))
    (while (setq start (string-match "/" file start))
      (setq start (1+ start)) (setq c (concat c "../"))) c))

(defun hfy-href-stub (this-file def-files tag)
  "Return an href stub for a tag href: if DEF-FILES \(list of files containing
definitions for the tag in question\) contains only one entry, the href should
link straight to that file. Otherwise, the link should be to the index file.\n
We are not yet concerned with the file extensions/tag line number and so on at
this point.\n
If `hfy-split-index' is set, and the href wil be to an index file rather than
a source file, append a .X to `hfy-index-file', where X is the uppercased
first character of TAG.\n
See also: `hfy-relstub', `hfy-index-file'`'."
  ;;(message "hfy-href-stub");;DBUG
  (concat
   (hfy-relstub this-file)
   (if (= 1 (length def-files)) (car def-files)
     (if (not hfy-split-index) hfy-index-file
       (concat hfy-index-file "." (upcase (substring tag 0 1)))))) )

(defun hfy-href (this-file def-files tag tag-map)
  "Return a relative href to the tag in question, based on\n
THIS-FILE `hfy-link-extn' `hfy-extn' DEF-FILES TAG and TAG-MAP\n
THIS-FILE is the current source file
DEF-FILES is a list of file containing possible link endpoints for TAG
TAG is the TAG in question
TAG-MAP is the entry in `hfy-tags-cache'."
  ;;(message "hfy-href");;DBUG
  (concat
   (hfy-href-stub this-file def-files tag)
   (or hfy-link-extn hfy-extn) "#" tag ;;(.src -> .html)
   (if (= 1 (length def-files))
       (concat "." (format "%d" (cadr (assoc (car def-files) tag-map)))))) )

(defun hfy-word-regex (string)
  "Return a regex that matches STRING as the first `match-string', with non
word characters on either side \(vaguely emulating the perl \\b regex atom\)."
  (concat "[^$A-Za-z_0-9]\\(" (regexp-quote string) "\\)[^A-Za-z_0-9]"))

;; mark all tags for hyperlinking, except the tags at
;; their own points of definition, iyswim:
(defun hfy-mark-tag-hrefs (srcdir file)
  "Mark href start points with the \'hfy-link prop \(value: href string\)\n
Mark href end points with the \'hfy-endl prop \(value t\)\n
Avoid overlapping links, and mark links in descending length of
tag name in order to prevent subtags from usurping supertags,
\(eg \"term\" for \"terminal\"). "
  ;;(message "hfy-mark-tag-hrefs");;DBUG
  (let ((cache-entry (assoc srcdir hfy-tags-cache))
        (list-cache  (assoc srcdir hfy-tags-sortl))
        (rmap-cache  (assoc srcdir hfy-tags-rmap ))
        (no-comment  (hfy-opt  'zap-comment-links))
        (no-strings  (hfy-opt  'zap-string-links ))
        (cache-hash                            nil)
        (tags-list                             nil)
        (tags-rmap                             nil)
        (case-fold-search                      nil))
    ;; extract the tag mapping hashes (fwd and rev) and the tag list:
    (if (and (setq cache-hash (cadr cache-entry))
             (setq tags-rmap  (cadr rmap-cache ))
             (setq tags-list  (cadr list-cache )))
        (mapcar
         (lambda (TAG)
           (let* ((start            nil)
                  (stop             nil)
                  (href             nil)
                  (name             nil)
                  (case-fold-search nil)
                  (tmp-point        nil)
                  (maybe-start      nil)
                  (face-at          nil)
                  (rmap-entry       nil)
                  (rnew-elt         nil)
                  (rmap-line        nil)
                  (tag-regex       (hfy-word-regex TAG))
                  (tag-map         (gethash TAG cache-hash))
                  (tag-files       (mapcar (lambda (X) (car X))  tag-map)))
             ;; find instances of TAG and do what needs to be done:
             (goto-char (point-min))
             (while (search-forward TAG nil 'NOERROR)
               (setq tmp-point   (point)
                     maybe-start (- (match-beginning 0) 1))
               (goto-char maybe-start)
               (if (not (looking-at tag-regex))
                   nil
                 (setq start   (match-beginning 1))
                 (setq stop    (match-end 1))
                 (setq face-at
                       (and (or no-comment no-strings) (hfy-face-at start)))
                 (if (listp face-at)
                     (setq face-at (cadr (memq :inherit face-at))))
                 (if (or (text-property-any   start  (1+ stop)  'hfy-linkp  t)
                         (and no-comment (eq 'font-lock-comment-face face-at))
                         (and no-strings (eq 'font-lock-string-face  face-at)))
                     nil ;; already a link, NOOP

                   ;; set a reverse map entry:
                   (setq rmap-line  (hfy-line-number)
                         rmap-entry (gethash    TAG     tags-rmap)
                         rnew-elt   (list  file  rmap-line  start)
                         rmap-entry (cons   rnew-elt   rmap-entry)
                         name       (format "%s.%d" TAG rmap-line))
                   (put-text-property start (1+ start) 'hfy-inst name)
                   (puthash TAG rmap-entry tags-rmap)

                   ;; mark the link. link to index if the tag has > 1 def
                   ;; add the line number to the #name if it does not:
                   (setq href (hfy-href file tag-files TAG tag-map))
                   (put-text-property start (1+ start) 'hfy-link  href)
                   (put-text-property stop  (1+ stop ) 'hfy-endl  t   )
                   (put-text-property start (1+ stop ) 'hfy-linkp t   )))
               (goto-char tmp-point)) ))
         tags-list) )))

(defun hfy-shell ()
  (if (string-match "\\<bash\\>\\|\\<sh\\>" shell-file-name)
      shell-file-name
    (or hfy-shell-file-name "/bin/sh")))

;; cache the #(tag => file line point) entries for files under srcdir
;; and cache the descending sorted list of tags in the relevant alist,
;; also keyed by srcdir:
(defun hfy-load-tags-cache (srcdir)
  "Run `hfy-etags-cmd' on SRCDIR, then call `hfy-parse-tags-buffer'."
  ;;(message "hfy-load-tags-cache");;DBUG
  (let ((etags-buffer  (get-buffer-create     "*hfy-tags*"))
        (etags-command (format hfy-etags-cmd hfy-etags-bin))
        (shell-file-name                        (hfy-shell)))
    (cd srcdir)
    (shell-command  etags-command etags-buffer)
    (hfy-parse-tags-buffer srcdir etags-buffer)) )

;; break this out from `hfy-load-tags-cache' to make the tar file
;; functionality easier to implement.
;; ( tar file functionality not merged here because it requires a
;;   hacked copy of etags capable of tagging stdin: if Francesco
;;   Potorti accepts a patch, or otherwise implements stdin tagging,
;;   then I will provide a `htmlfontify-tar-file' defun )
(defun hfy-parse-tags-buffer (srcdir buffer)
  "Parse a BUFFER containing etags formatted output, loading the
`hfy-tags-cache'  and `hfy-tags-sortl' entries for SRCDIR."
  (let ((cache-entry     (assoc srcdir    hfy-tags-cache))
        (tlist-cache     (assoc srcdir    hfy-tags-sortl))
        (trmap-cache     (assoc srcdir    hfy-tags-rmap ))
        (cache-hash nil) (trmap-hash nil) (tags-list  nil)
        (hash-entry nil) (tag-string nil) (tag-line   nil)
        (tag-point  nil) (new-entry  nil) (etags-file nil))

    ;; (re)initialise the tag reverse map:
    (if trmap-cache (setq trmap-hash (cadr trmap-cache))
      (setq trmap-hash (make-hash-table :test 'equal))
      (setq hfy-tags-rmap (list (list srcdir trmap-hash) hfy-tags-rmap)))
    (clrhash trmap-hash)

    ;; (re)initialise the tag cache:
    (if cache-entry (setq cache-hash (cadr cache-entry))
      (setq cache-hash (make-hash-table :test 'equal))
      (setq hfy-tags-cache (list (list srcdir cache-hash) hfy-tags-cache)))
    (clrhash cache-hash)

    ;; cache the TAG => ((file line point) (file line point) ... ) entries:
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))

      (while (and (looking-at "^\x0c") (= 0 (forward-line 1)))
        ;;(message "^L boundary")
        (if (and (looking-at "^\\(.+\\),\\([0-9]+\\)$")
                 (= 0 (forward-line 1)))
            (progn
              (setq etags-file (match-string 1))
              ;;(message "TAGS for file: %s" etags-file)
              (while (and (looking-at hfy-etag-regex) (= 0 (forward-line 1)))
                (setq tag-string (match-string 1))
                (if (= 0 (length tag-string)) nil ;; noop
                  (setq tag-line   (string-to-int (match-string 2)))
                  (setq tag-point  (string-to-int (match-string 3)))
                  (setq hash-entry (gethash tag-string  cache-hash))
                  (setq new-entry  (list etags-file tag-line tag-point))
                  (setq hash-entry (cons new-entry hash-entry))
                  ;;(message "HASH-ENTRY %s %S" tag-string new-entry)
                  (puthash tag-string hash-entry cache-hash)
                  )
                )
              )
          )
        )
      )

    ;; cache a list of tags in descending length order:
    (maphash (lambda (K V) (setq tags-list (cons K tags-list))) cache-hash)
    (setq tags-list (sort tags-list (lambda (A B) (< (length B) (length A)))))

    ;; put the tag list into the cache:
    (if tlist-cache (setcar (cdr tlist-cache) tags-list)
      (setq hfy-tags-sortl (cons (list srcdir tags-list) hfy-tags-sortl)))

    ;; return the number of tags found:
    (length tags-list)
    )
  )

(defun hfy-prepare-index-i (srcdir dstdir filename &optional stub map)
  "Prepare a tags index buffer for SRCDIR.
`hfy-tags-cache' must already have an entry for SRCDIR for this to work.
`hfy-page-header', `hfy-page-footer', `hfy-link-extn' and `hfy-extn'
all play a part here.\n
If STUB is set, prepare an \(appropriately named\) index buffer
specifically for entries beginning with STUB.\n
If MAP is set, use that instead of `hfy-tags-cache'."
  ;;(message "hfy-write-index");;DBUG
  (let ((cache-entry  (assoc srcdir (or map hfy-tags-cache)))
        (cache-hash    nil)
        (tag-list      nil)
        (index-file
         (concat filename (if stub (concat "." stub) "") hfy-extn))
        (index-buf     nil))
    (if (not (and cache-entry
                  (setq cache-hash (cadr cache-entry))
                  (setq index-buf  (get-buffer-create index-file))))
        nil ;; noop
      (maphash (lambda (K V) (setq tag-list (cons K tag-list))) cache-hash)
      (setq tag-list (sort tag-list 'string<))
      (set-buffer index-buf)
      (erase-buffer)
      (insert (funcall hfy-page-header filename "<!-- CSS -->"))
      (insert "<table>\n")

      (mapcar
       (lambda (TAG)
         (let ((tag-started nil))
           (mapcar
            (lambda (DEF)
              (if (and stub (not (string-match (concat "^" stub) TAG)))
                  nil ;; we have a stub and it didn't match: NOOP
                (let ((file (car  DEF))
                      (line (cadr DEF)))
                  (insert
                   (format
                    (concat
                     "  <tr>                                   \n"
                     "   <td>%s</td>                           \n"
                     "   <td><a href=\"%s%s\">%s</a></td>      \n"
                     "   <td><a href=\"%s%s#%s.%d\">%d</a></td>\n"
                     "  </tr>                                  \n")
                        (if (string= TAG tag-started) "&nbsp;"
                          (format "<a name=\"%s\">%s</a>" TAG TAG))
                        file (or hfy-link-extn hfy-extn) file
                        file (or hfy-link-extn hfy-extn) TAG line line))
                  (setq tag-started TAG))))
            (gethash TAG cache-hash)))) tag-list)
      (insert "</table>\n")
      (insert (funcall hfy-page-footer filename))
      (and dstdir (cd dstdir))
      (set-visited-file-name index-file)
      index-buf)
    )
  )

(defun hfy-prepare-index (srcdir dstdir)
  "Return a list of index buffer\(s\), as determined by `hfy-split-index'."
  (if (not hfy-split-index)
      (list (hfy-prepare-index-i srcdir dstdir hfy-index-file nil))
    (let ((stub-list     nil)
          (cache-hash    nil)
          (index-list    nil)
          (cache-entry  (assoc srcdir hfy-tags-cache))
          )
      (if (and cache-entry (setq cache-hash (cadr cache-entry)))
          (maphash
           (lambda (K V)
             (let ((stub (upcase (substring K 0 1))))
               (if (member stub stub-list)
                   nil ;; seen this already: NOOP
                 (setq
                  stub-list  (cons stub stub-list)
                  index-list (cons (hfy-prepare-index-i srcdir
                                                        dstdir
                                                        hfy-index-file
                                                        stub)
                                   index-list)) ))) cache-hash) ) index-list)))

(defun hfy-prepare-tag-map (srcdir dstdir)
  "Prepare the counterpart\(s\) to the index buffer\(s\) - a list of buffers
with the same structure, but listing \( and linking to \) instances of tags
\( as opposed to their definitions \).\n
See: `hfy-prepare-index'
     `hfy-split-index'."
  (if (not hfy-split-index)
      (list (hfy-prepare-index-i srcdir
                                 dstdir
                                 hfy-instance-file
                                 nil
                                 hfy-tags-rmap))
    (let ((stub-list     nil)
          (cache-hash    nil)
          (index-list    nil)
          (cache-entry  (assoc srcdir hfy-tags-rmap))
          )
      (if (and cache-entry (setq cache-hash (cadr cache-entry)))
          (maphash
           (lambda (K V)
             (let ((stub (upcase (substring K 0 1))))
               (if (member stub stub-list)
                   nil ;; seen this already: NOOP
                 (setq
                  stub-list  (cons stub stub-list)
                  index-list (cons (hfy-prepare-index-i srcdir
                                                        dstdir
                                                        hfy-instance-file
                                                        stub
                                                        hfy-tags-rmap)
                                   index-list)) ))) cache-hash) ) index-list)))

(defun hfy-subtract-maps (srcdir)
  "Internal function - strips definitions of tags from the instance map.
See: `hfy-tags-cache' and `hfy-tags-rmap'"
  (let ((new-list nil)
        (old-list nil)
        (def-list nil)
        (exc-list nil)
        (fwd-map (cadr (assoc srcdir hfy-tags-cache)))
        (rev-map (cadr (assoc srcdir hfy-tags-rmap )))
        (taglist (cadr (assoc srcdir hfy-tags-sortl))))
    (mapc
     (lambda (TAG)
       (setq def-list (gethash TAG fwd-map)
             old-list (gethash TAG rev-map)
             new-list  nil
             exc-list  nil)
       (mapc
        (lambda (P)
          (setq exc-list (cons (list (car P) (cadr P)) exc-list))) def-list)
       (mapc
        (lambda (P)
          (or (member (list (car P) (cadr P)) exc-list)
              (setq new-list (cons P new-list)))) old-list)
       (puthash TAG new-list rev-map)) taglist)
    )
  )

(defun htmlfontify-run-etags (srcdir)
  "Load the etags cache for SRCDIR. See `hfy-load-tags-cache'."
  (interactive "D source directory: ")
  (setq srcdir (directory-file-name srcdir))
  (hfy-load-tags-cache srcdir))

;;(defun hfy-test-read-args (foo bar)
;;  (interactive "D source directory: \nD target directory: ")
;;  (message "foo: %S\nbar: %S" foo bar))

(defun hfy-save-kill-buffers (buffer-list &optional dstdir)
  (mapc (lambda (B)
          (set-buffer B)
          (and dstdir (file-directory-p dstdir) (cd dstdir))
          (save-buffer)
          (kill-buffer B)) buffer-list) )

(defun htmlfontify-copy-and-link-dir (srcdir dstdir &optional f-ext l-ext)
  "Trawl SRCDIR and write fontified-and-hyperlinked output in DSTDIR.\n
F-EXT and L-EXT specify values for `hfy-extn' and `hfy-link-extn'.\n
You may also want to set `hfy-page-header' and `hfy-page-footer'."
  (interactive "D source directory: \nD output directory: ")
  ;;(message "htmlfontify-copy-and-link-dir")
  (setq srcdir (directory-file-name srcdir))
  (setq dstdir (directory-file-name dstdir))
  (let ((source-files "SETME: list of source files, relative to srcdir")
        (tr-cache  (assoc srcdir hfy-tags-rmap))
        (hfy-extn            (or f-ext ".html"))
        (hfy-link-extn       (or l-ext ".html")))
    ;; oops, forgot to load etags for srcdir:
    (if tr-cache nil
      (message "autoload of tags cache")
      (hfy-load-tags-cache srcdir)
      (setq tr-cache (assoc srcdir hfy-tags-rmap)))
    ;; clear out the old cache:
    (clrhash   (cadr tr-cache))
    (hfy-make-directory dstdir)
    (setq source-files (hfy-list-files srcdir))
    (mapcar (lambda (file)
              (hfy-copy-and-fontify-file srcdir dstdir file)) source-files)
    (hfy-subtract-maps srcdir)
    (hfy-save-kill-buffers (hfy-prepare-index   srcdir dstdir) dstdir)
    (hfy-save-kill-buffers (hfy-prepare-tag-map srcdir dstdir) dstdir)
    )
  )

;; name of the init file we want:
(defun hfy-initfile ()
  (let* ((file (or (getenv "HFY_INITFILE") ".hfy.el")))
    (expand-file-name file "~") ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; incomplete as yet : transfer hook settings to hfy init file:
;; (defalias 'hfy-set-hooks 'custom-set-variables)

;; (defun hfy-pp-hook (H)
;;   (and (string-match "-hook$" (symbol-name H))
;;        (boundp H)
;;        (symbol-value H)
;;        (insert (format "\n '(%S %S)" H (symbol-value H)))
;;        )
;;   )

;; (defun hfy-save-hooks ()
;;   (let ((custom-file (hfy-initfile)))
;;     (custom-save-delete 'hfy-set-hooks)
;;     (let ((standard-output (current-buffer)))
;;       (princ "(hfy-set-hooks\n;;auto-generated, only one copy allowed\n")
;;       (mapatoms 'hfy-pp-hook)
;;       (insert "\n)")
;;       )
;;     )
;;   )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'hfy-init-progn 'progn)

(defun hfy-save-initvar (sym)
  (princ (format "(setq %s\n '" sym))
  (pp    (symbol-value sym))
  (princ ")\n")
  )

(defun htmlfontify-save-initfile ()
  (interactive)
  (let* ((start-pos       nil)
         (custom-file     (hfy-initfile))
         (standard-output (find-file-noselect custom-file 'nowarn)))
    (save-excursion
      (custom-save-delete 'hfy-init-progn)
      (setq start-pos (point))
      (princ "(hfy-init-progn\n;;auto-generated, only one copy allowed\n")
      (mapcar 'hfy-save-initvar
              (list 'auto-mode-alist 'interpreter-mode-alist))
      (princ ")\n")
      (indent-region start-pos (point) nil)
      )
    (custom-save-all)
    )
  )

(defun htmlfontify-load-initfile ()
  (interactive)
  (let ((file (hfy-initfile)))
    (load file 'NOERROR nil nil)
    )
  )

(provide 'htmlfontify)

;; TLF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is for part of the build system for rtfm.etla.org:
;; it's not really part of htmlfontify-buffer - but it's an example
;; of how to use it:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if t nil
  (defun rtfm-build-page-header (file style)
    (format "#define  TEMPLATE red+black.html
#define  DEBUG    1
#include <build/menu-dirlist|>\n
html-css-url := /css/red+black.css
title        := rtfm.etla.org ( %s / src/%s )
bodytag      :=
head         <=STYLESHEET;\n
%s
STYLESHEET
main-title   := rtfm / %s / src/%s\n
main-content <=MAIN_CONTENT;\n" rtfm-section file style rtfm-section file))

  (defun rtfm-build-page-footer (file) "\nMAIN_CONTENT\n")

  (defun rtfm-build-source-docs (section srcdir destdir)
    (interactive
     "s section[eg- emacs / p4-blame]:\nD source-dir: \nD output-dir: ")
    (require 'htmlfontify)
    (hfy-load-tags-cache srcdir)
    (let ((hfy-page-header  'rtfm-build-page-header)
          (hfy-page-footer  'rtfm-build-page-footer)
          (rtfm-section                     section)
          (hfy-index-file                   "index"))
      (htmlfontify-run-etags srcdir)
      (htmlfontify-copy-and-link-dir srcdir destdir ".src" ".html")))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
