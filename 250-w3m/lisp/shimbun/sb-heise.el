;;; sb-heise.el --- heise online shimbun backend

;; Copyright (C) 2004, 2005 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Keywords: news

;; This file is a part of shimbun.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-heise (shimbun) ())

(defvar shimbun-heise-url "http://www.heise.de")

(defvar shimbun-heise-group-path-alist
  '(("news" . "/newsticker/")
    ("telepolis" . "/tp/")))


(defvar shimbun-heise-content-start
  "\\(<!-- Meldung -->\\|<!-- INHALT -->\\|<HEISETEXT>\\)")
(defvar shimbun-heise-content-end
  "\\(<!-- untere News-Navigation -->\\|<!-- INHALT -->\\|</HEISETEXT>\\)")

(defvar shimbun-heise-x-face-alist
  '(("default" . "X-Face: #RVD(kjrS;RY\"2yH]w.1U,ZC_DbR,9{tQnhyYe|,\\J)\"\
C*o1{%`*]WwtAuo;reeq_koTr=oIKXFB4#bS'tSdz.Mc%t~-@873uYV>SMjL7D6K$M4L0Up{D\
_rBgD*Xj,t;iPKWh:!B}ijDOoCxs!}rs&(r-TLwU8=>@[w^H(>^u$wM*}\":9LANQs)1\"cZP\
6ftp?9>b&|rkGR+VWIlD:%?,Fvi8h?q2H+pVqq5#Z9*k2q7.P)0$x!A)T")))


(defvar shimbun-heise-groups
  (mapcar 'car shimbun-heise-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-heise))
  (concat shimbun-heise-url
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-heise-group-path-alist))))


(defun shimbun-heise-get-newsticker-headers (shimbun)
  (let ((regexp "<a href=\"meldung/\\([0-9]+\\)\">\\([^<]+\\)</a>")
	(from "Heise Online News <invalid@heise.de>")
	(date "") (id) (url) (subject) (headers))
    (catch 'stop
      (while (re-search-forward regexp nil t nil)
	(setq id (match-string 1))
	(setq url (shimbun-expand-url (concat "meldung/" id)
				      (shimbun-index-url shimbun)))
	(setq subject (match-string 2))
	(setq id (concat "<newsticker" id "@heise.de>"))
	(when (shimbun-search-id shimbun id)
	  (throw 'stop nil))
	(push (shimbun-make-header
	       0 (shimbun-mime-encode-string subject)
	       (shimbun-mime-encode-string from)
	       date id "" 0 0 url)
	      headers)))
    headers))


(defconst shimbun-heise-date-re "<td[^>]+class=\"date-cell\"\\s-*>\\sw*,\
\\s-*\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\s-*</td>")

(defconst shimbun-heise-author-re "<p\\s-+class=\"inhalt-autor\">\\(.*?\\)</p>")

(defconst shimbun-heise-url-re "<p\\s-+class=\"inhalt-head\">.*?\
<a\\s-+href=\"\\(/.*?/\\([0-9]+\\)/\\([0-9]+\\).html\\)\".*?\n*\
\\(.*?\\)</a></p>")

(defun shimbun-heise-get-telepolis-headers (shimbun)
  (let (headers (limit (re-search-forward shimbun-heise-date-re nil t)))
    (catch 'stop
      (while limit
        (goto-char limit)
        (let ((day (match-string 1))
              (month (match-string 2))
              (year (match-string 3)))
          (setq limit (save-excursion
                        (re-search-forward shimbun-heise-date-re nil t)))
          (save-match-data
            (while (re-search-forward shimbun-heise-url-re limit t)
              (let ((url (match-string 1))
                    (mid (concat "<" (match-string 2) "x"
                                 (match-string 3) "@heise.de>"))
                    (subj (match-string 4)))
                (when (shimbun-search-id shimbun mid)
                  (throw 'stop nil))
                (when (re-search-forward shimbun-heise-author-re limit t)
                  (let ((author (match-string 1)))
                    (push (shimbun-create-header
                           0 subj author
                           (shimbun-make-date-string
                            (string-to-number year)
                            (string-to-number month)
                            (string-to-number day)
                            "00:00"
                            ;; FIXME: timezone is always wrong, slightly better than
                            ;; the default "+0900"
                            "+0000")
                           mid "" 0 0 url) headers)))))))))
    headers))

(luna-define-method shimbun-get-headers
  ((shimbun shimbun-heise) &optional range)
  (if (equal (shimbun-current-group-internal shimbun) "news")
      (shimbun-heise-get-newsticker-headers shimbun)
    (shimbun-heise-get-telepolis-headers shimbun)))


(defun shimbun-heise-wash-newsticker-article (header)
  (save-excursion

    ;; get the real date
    (let ((regexp-date-begin "<!-- \\*\\*\\* tmpl \\*\\*\\* -->")
	  (regexp-date-end "<!-- obere News-Navigation -->")
	  (regexp-date (concat "\\([0-9]+\\)\\.\\([0-9]+\\)\\."
			       "\\([0-9]+\\)[ \t]+\\([0-9]+\\:[0-9]+\\)"))
	  (tmp-point) (bound-point))
      (when (setq tmp-point (re-search-forward regexp-date-begin nil t nil))
	(when (setq bound-point (re-search-forward regexp-date-end nil t nil))
	  (goto-char tmp-point)
	  (when (re-search-forward regexp-date bound-point t nil)
	    (shimbun-header-set-date
	     header
	     (shimbun-make-date-string
	      (string-to-number (match-string 3)) ; year
	      (string-to-number (match-string 2)) ; month
	      (string-to-number (match-string 1)) ; day
	      (match-string 4)		; time
	      ;; FIXME: timezone is always wrong, slightly better than the
	      ;; default "+0900"
	      "+0000"))))))

    ;; get the real from
    (let ((regexp-from-begin "<!-- Meldung -->\\|<HEISETEXT>")
	  (regexp-from-end "<!-- untere News-Navigation -->")
	  (regexp-from (concat "(<a href=\"mailto:\\([^@]+@ct.heise.de\\)\""
			       "[^>]*>\\([^<]+\\)</a>"))
	  (tmp-point) (bound-point))
      (when (setq tmp-point (re-search-forward regexp-from-begin nil t nil))
	(when (setq bound-point (re-search-forward regexp-from-end nil t nil))
	  (goto-char tmp-point)
	  (when (re-search-forward regexp-from bound-point t nil)
	    (shimbun-header-set-from
	     header
	     (shimbun-mime-encode-string
	      (concat "Heise Online News, "
		      (match-string 2)
		      " <"
		      (match-string 1)
		      ">")))))))

    ;; strip ads
    (goto-char (point-min))
    (let ((regexp-ad-begin "<!-- Meldung -->\\|<HEISETEXT>")
	  (regexp-ad-end "<!-- untere News-Navigation -->")
	  (regexp-ad "<!--OAS AD=\"Middle[0-9]*\"-->")
	  (tmp-point) (bound-min) (bound-max))
      (when (setq bound-min (re-search-forward regexp-ad-begin nil t nil))
	(when (setq bound-max (re-search-forward regexp-ad-end nil t nil))
	  (goto-char bound-min)
	  (while (re-search-forward regexp-ad bound-max t nil)
	    (let ((begin-region (re-search-backward "<table" bound-min t nil))
		  (end-region (re-search-forward "</table>" bound-max t nil)))
	      (when (and begin-region end-region)
		(delete-region begin-region end-region)))))))))


(defun shimbun-heise-wash-telepolis-article (header)
  (save-excursion
    ;; strip nasty "download" images
    (goto-char (point-min))
    (while (re-search-forward "<TP:BUT>" nil t nil)
      (delete-region (point) (re-search-forward "</TP:BUT>" nil t nil)))))

(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-heise) header)
  (if (equal (shimbun-current-group-internal shimbun) "news")
      (shimbun-heise-wash-newsticker-article header)
    (shimbun-heise-wash-telepolis-article header)))


(provide 'sb-heise)

;;; sb-heise.el ends here
