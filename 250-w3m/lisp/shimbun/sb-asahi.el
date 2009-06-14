;;; sb-asahi.el --- shimbun backend for asahi.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>,
;;         NOMIYA Masaru      <nomiya@ttmy.ne.jp>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-asahi
		   (shimbun-japanese-newspaper shimbun-text) ())

(defvar shimbun-asahi-top-level-domain "asahi.com"
  "Name of the top level domain for the Asahi shimbun.")

(defvar shimbun-asahi-url
  (concat "http://www." shimbun-asahi-top-level-domain "/")
  "Name of the parent url.")

(defun shimbun-asahi-make-regexp (name)
  "Return a list of a regexp and numbers for the kansai.NAME group.
Every `.' in NAME will be replaced with `/'."
  (list (let ((s0 "[\t\n $B!!(B]*")
	      (s1 "[\t\n ]+")
	      (no-nl "[^\n]+"))
	  (concat "<a" s1 "href=\"/"
		  ;; 1. url
		  "\\(" (shimbun-subst-char-in-string ?. ?/ name) "/"
		  ;; 2. serial number
		  "\\([a-z]*"
		  ;; 3. year
		  "\\(20[0-9][0-9]\\)"
		  ;; 4. month
		  "\\([01][0-9]\\)"
		  ;; 5. day
		  "\\([0-3][0-9]\\)"
		  "[0-9]+\\)"
		  "\\.html\\)"
		  "\">" s0
		  ;; 6. subject
		  "\\(" no-nl "\\)"
		  s0 "</a>"))
	1 nil 2 6 3 4 5))

(defvar shimbun-asahi-group-table
  (let* ((s0 "[\t\n $B!!(B]*")
	 (s1 "[\t\n ]+")
	 (no-nl "[^\n]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(%s/update/"
		    ;; 2. month
		    "\\([01][0-9]\\)"
		    ;; 3. day
		    "\\([0-3][0-9]\\)"
		    "/"
		    ;; 4. serial number
		    "\\([a-z]*[0-9]+\\)"
		    "\\.html\\)"
		    "\">" s0
		    ;; 5. subject
		    "\\(" no-nl "\\)"
		    s0 "</a>")
		   1 4 nil 5 nil 2 3))
	 (default2 (shimbun-asahi-make-regexp "%s"))
	 (default3 (list
		    (concat
		     "<a" s1 "href=\"/+"
		     ;; 1. url
		     "\\(.+/"
		     ;; 2. serial number
		     "\\([a-z]*"
		     ;; 3. year
		     "\\(20[0-9][0-9]\\)"
		     ;; 4. month
		     "\\([01][0-9]\\)"
		     ;; 5. day
		     "\\([0-3][0-9]\\)"
		     "[0-9]+\\)"
		     "\\.html\\)"
		     "\">" s0
		     ;; 6. subject
		     "\\(" no-nl "\\)"
		     s0 "</a>")
		    1 nil 2 6 3 4 5))
	 (edu (shimbun-asahi-make-regexp "edu.news"))
	 (health (shimbun-asahi-make-regexp "health.news")))
    `(("book" "$B=PHG%K%e!<%9(B" "book/news/"
       ,@(shimbun-asahi-make-regexp "book.news"))
      ("business" "$B%S%8%M%9(B" "%s/list.html" ,@default)
      ;; The url should be ended with "index.html".
      ("business.column" "$B7P:Q5$>]Bf(B" "business/column/index.html" ,@default2)
      ("car" "$B0&<V(B" "%s/news/" ,@(shimbun-asahi-make-regexp "car.news"))
      ("car.italycolumn" "$B%$%?%j%"H/%"%b!<%l!*%b%H!<%l!*(B" "car/italycolumn/"
       ,@default2)
      ("car.motorsports" "$B%b!<%?!<%9%]!<%D(B" "car/motorsports/" ,@default2)
      ("car.newcar" "$B?7<V>pJs(B" "car/newcar/" ,@default2)
      ("car.newcarbywebcg" "$B?7<VH/I=2q(B" "car/newcarbywebcg/" ,@default2)
      ("culture" "$BJ82=!&7]G=(B" "%s/list.html" ,@default)
      ("culture.column" "$B$b$d$7$N$R$2(B" "culture/list_moyashi.html"
       ,@(shimbun-asahi-make-regexp "culture.column.moyashi"))
      ("digital" "$B%G%8%?%k5!4o(B" "digital/av/"
       ,@(shimbun-asahi-make-regexp "digital.av"))
      ("digital.apc" "$B;(;o!V(BASAHI$B%Q%=%3%s!W%K%e!<%9(B" "digital/apc/" ,@default2)
      ("digital.bcnnews" "e$B%S%8%M%9>pJs(B ($BDs6!!'#B#C#N(B)" "digital/bcnnews/"
       ,@default2)
      ("digital.column01" "$B%G%8%?%k%3%i%`(B" "digital/column01/"
       ,@default2)
      ("digital.hotwired" "HotWired Japan" "digital/hotwired/" ,@default2)
      ("digital.internet" "$B%M%C%H!&%&%$%k%9(B" "digital/internet/" ,@default2)
      ("digital.mobile" "$B7HBSEEOC(B" "digital/mobile/" ,@default2)
      ("digital.nikkanko" "$BF|4)9)6H?7J9%K%e!<%9(B" "digital/nikkanko/"
       ,@default2)
      ("digital.pc" "$B%Q%=%3%s(B" "digital/pc/" ,@default2)
      ("editorial" "$B<R@b(B" "paper/editorial.html"
       ,(concat
	 "<a" s1 "href=\"\\./"
	 ;; 1. url
	 "\\(editorial"
	 ;; 2. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 3. month
	 "\\([01][0-9]\\)"
	 ;; 4. day
	 "\\([0-3][0-9]\\)"
	 "\\.html\\)"
	 "\"")
       1 nil nil nil 2 3 4)
      ("edu" "$B650i(B" "%s/news/index.html" ,@edu)
      ("edu.column" "$B650i%3%i%`(B" "edu/column/ikuji/"
       ,@(shimbun-asahi-make-regexp "edu.column.ikuji"))
      ("edu.it" "IT$B650i(B" "edu/news/it.html" ,@edu)
      ("edu.kosodate" "$B;R0i$F(B" "edu/news/kosodate.html" ,@edu)
      ("edu.nyushi" "$BBg3X!&F~;n(B" "edu/news/nyushi.html" ,@edu)
      ("edu.tamate" "$B$N$N$A$c$s$N$U$7$.6L<jH"(B" "edu/nie/tamate/"
       ,@(shimbun-asahi-make-regexp "edu.nie.tamate.kiji"))
      ("english" "ENGLISH" "%s/index.html"
       ,@(let ((rest (shimbun-asahi-make-regexp "english.Herald-asahi")))
	   (cons (concat (car rest)
			 "\\(" s0 "<[^>]+>\\)*" s0 "([01]?[0-9]/[0-3]?[0-9])")
		 (cdr rest))))
      ("health" "$B7r9/!&@83h(B" "%s/news/" ,@health)
      ("health.aged" "$BJ!;c!&9bNp(B" "health/news/aged.html" ,@health)
      ("health.alz" "$BG'CN>IFC=8(B" "health/news/alz.html" ,@health)
      ("health.medical" "$B0eNE!&IB5$(B" "health/news/medical.html" ,@health)
      ("housing" "$B=;$^$$(B" "%s/news/"
       ,@(shimbun-asahi-make-regexp "housing.news"))
      ("housing.amano" "$BE7Ln>4$N$$$$2H$$$$2HB2(B" "housing/amano/" ,@default2)
      ("housing.column" "$B=;$^$$$N$*LrN)$A%3%i%`(B" "housing/column/" ,@default2)
      ("housing.diary" "$B>.$5$J2H$N@83hF|5-(B" "housing/diary/" ,@default2)
      ("housing.world" "$B@$3&$N%&%A(B" "housing/world/" ,@default2)
      ("igo" "$B0O8k(B" "%s/news/" ,@(shimbun-asahi-make-regexp "igo.news"))
      ("international" "$B9q:](B" "%s/list.html" ,@default)
      ("international.jinmin" "$B?ML1F|Js(B" "international/jinmin/index.html"
       ,@default2)
      ("job" "$B="?&!&E>?&(B" "%s/news/"
       ,@(shimbun-asahi-make-regexp "job.news"))
      ("job.special" "$B=54)D+F|!&#A#E#R#A$+$i(B" "job/special/"
       ,(concat
	 (car default2)
	 "\\(" s0 "<[^>]+>\\)*" s0 "$B!J(B" s0
	 ;; 8. extra
	 "\\(" no-nl "\\)"
	 "$B!'(B")
       ,@(cdr default2) nil 8)
      ("kansai" "$B4X@>(B" "%s/news/" ,@(shimbun-asahi-make-regexp "kansai.news"))
      ("kansai.horiekenichi" "$BKY9>8,0l$N@$3&0l<~$R$H$j$\$C$A(B"
       "kansai/horiekenichi/" ,@default2)
      ("kansai.umaimon" "$B$&$^$$$b$s(B" "kansai/umaimon/" ,@default2)
      ("kansai.fuukei" "$BIw7J$rJb$/(B" "kansai/fuukei/" ,@default2)
      ("kansai.yotsuba" "$B$h$DMU$S$h$j(B" "kansai/yotsuba/" ,@default2)
      ("kansai.smile" "$B%9%^%$%k%9%?%$%k(B" "kansai/smile/" ,@default2)
      ("kansai.keiki" "$B$1!A$-$N!H$($(OC!I(B" "kansai/keiki/" ,@default2)
      ("kansai.okiniiri" "DJ$B$N$*5$$KF~$j(B" "kansai/okiniiri/" ,@default2)
      ("kansai.syun" "$B=\$N4i(B" "kansai/syun/" ,@default2)
      ("kansai.takara" "$B$?$+$i?^4U(B" "kansai/takara/" ,@default2)
      ("kansai.kansaiisan" "$B>!<j$K4X@>@$3&0d;:(B" "kansai/kansaiisan/"
       ,@default2)
      ("kansai.depa" "$B%G%QCO2<#N#E#W#S(B" "kansai/depa/" ,@default2)
      ("kansai.kaban" "$B$+$P$s$NCf?H(B" "kansai/kaban/" ,@default2)
      ("kansai.kyosho" "$B5p>"$K3X$Y(B" "kansai/kyosho/" ,@default2)
      ("kansai.okan" "$BJl$5$s$NCN7CB^(B" "kansai/okan/" ,@default2)
      ("kansai.densetsu" "$B$[$s$^!)4X@>EA@b(B" "kansai/densetsu/" ,@default2)
      ("kansai.onayami" "$B$_$&$i$8$e$s$N$*G:$_:W$j(B" "kansai/onayami/"
       ,@default2)
      ("kansai.sanshi" "$B;0;^$N>P%&%$%s%I%&(B" "kansai/sanshi/" ,@default2)
      ("life" "$BJk$i$7(B" "%s/list.html" ,@default)
      ("life.column" "$BJk$i$7%3%i%`(B" "life/column/"
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(life/column/"
	 ;; 2. serial number
	 "\\(.+/[a-z]*"
	 ;; 3. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "[0-9]*\\)"
	 "\\.html\\)"
	 "\">" s0
	 ;; 6. subject
	 "\\(" no-nl "\\)"
	 s0 "</a>")
       1 nil 2 6 3 4 5)
      ("life.food" "$B?)$HNAM}(B" "life/food/"
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(life/food/"
	 ;; 2. serial number
	 "\\(.+/[a-z]*"
	 ;; 3. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "[0-9]+\\)"
	 "\\.html\\)"
	 "\">" s0
	 ;; 6. subject
	 "\\(" no-nl "\\)"
	 s0 "</a>")
       1 nil 2 6 3 4 5)
      ("nankyoku" "$BFn6K%W%m%8%'%/%H(B" "%s/news/"
       ,@(shimbun-asahi-make-regexp "nankyoku.news"))
      ("nankyoku.borderless" "$B9q6-$N$J$$BgN&$+$i(B" "nankyoku/borderless/"
       ,@default2)
      ("nankyoku.people" "$B1[E_Bb$N?M$S$H(B" "nankyoku/people/" ,@default2)
      ("nankyoku.whitemail" "WhiteMail$B!wFn6K(B" "nankyoku/whitemail/" ,@default2)
      ("national" "$B<R2q(B" "%s/list.html" ,@default)
      ("national.calamity" "$B:R32!&8rDL>pJs(B" "national/calamity.html"
       ,@default3)
      ("national.etc" "$B$=$NB>!&OCBj(B" "national/etc.html" ,@default3)
      ("national.trial" "$B:[H=(B" "national/trial.html" ,@default3)
      ("obituaries" "$B$*$/$d$_(B" "obituaries" ,@default)
      ("politics" "$B@/<#(B" "%s/list.html" ,@default)
      ("rss" "RSS" "http://www3.asahi.com/rss/index.rdf"
       ,(concat
	 "<title>"
	 ;; 1. subject
	 "\\([^<]+\\)"
	 "</title>\n<link>"
	 ;; 2. url
	 "\\(http://www\\.asahi\\.com/"
	 ;; 3. extra keyword (en)
	 "\\([^/]+\\)"
	 "/update/"
	 ;; 4 and 5. serial number
	 "\\([0-9]+\\)/\\([0-9]+\\)"
	 "\\.html\\?ref=rss\\)"
	 "</link>\n<description/>\n<dc:subject>"
	 ;; 6. extra keyword (ja)
	 "\\([^<]+\\)"
	 "</dc:subject>\n<dc:date>20[0-9][0-9]-"
	 ;; 7. month
	 "\\([01][0-9]\\)"
	 "-"
	 ;; 8. day.
	 "\\([0-3][0-9]\\)"
	 "T"
	 ;; 9. hour:min:sec
	 "\\([012][0-9]:[0-5][0-9]:[0-5][0-9]\\)")
       2 4 5 1 nil 7 8 9 3 nil 6)
      ("science" "$B%5%$%(%s%9(B" "%s/list.html"
       ,@(shimbun-asahi-make-regexp "science.news"))
      ("shopping" "$B%7%g%C%T%s%0(B" "%s/news/"
       ,@(shimbun-asahi-make-regexp "shopping.news"))
      ("shopping.kishi" "$B4_D+;R$N5$$K$J$k$*<h$j4s$;(B12$B%+7n(B" "shopping/kishi/"
       ,@default2)
      ("shopping.ryouhin" "$B$/$i$7$NNIIJC5K,(B" "shopping/ryouhin/"
       ,@default2)
      ("shougi" "$B>-4}(B" "%s/news/" ,@(shimbun-asahi-make-regexp "shougi.news"))
      ("sports" "$B%9%]!<%D(B" "%s/list.html" ,@default)
      ("sports.baseball" "$BLn5e(B" "sports/bb/"
       ,@(shimbun-asahi-make-regexp "sports.bb"))
      ("sports.column" "$B%9%]!<%D%3%i%`(B" "sports/column/" ,@default2)
      ("sports.football" "$B%5%C%+!<(B" "sports/fb/"
       ,@(shimbun-asahi-make-regexp "sports.fb"))
      ("sports.spo" "$B0lHL%9%]!<%D(B" "sports/spo/" ,@default2)
      ("tenjin" "$BE7@<?M8l(B" "paper/column.html"
       ,(concat
	 "<a" s1 "href=\"\\./"
	 ;; 1. url
	 "\\(column"
	 ;; 2. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 3. month
	 "\\([01][0-9]\\)"
	 ;; 4. day
	 "\\([0-3][0-9]\\)"
	 "\\.html\\)"
	 "\"")
       1 nil nil nil 2 3 4)
      ("travel" "$B%H%i%Y%k(B" "%s/news/"
       ,@(shimbun-asahi-make-regexp "travel.news"))
      ("travel.kaido" "$B;JGONKB@O:!&39F;$r$f$/(B" "travel/kaido/" ,@default2)
      ("travel.matsuri" "$BF|K\$N:W$j(B" "travel/matsuri/"
       ,@default2)
      ("travel.zeitaku" "$BCO5e$NlT$?$/(B" "travel/zeitaku/"
       ,@default2)
      ("wakamiya" "$BIw9M7W(B ($BO@@b<g44!&<c5\7<J8(B)" "column/wakamiya/"
       ,@(shimbun-asahi-make-regexp "column.wakayama"))))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where index pages and regexps may contain the
\"%s\" token which is replaced with group names, numbers point to the
search result in order of [0]a url, [1,2]a serial number, [3]a subject,
\[4]a year, [5]a month, [6]a day, [7]an hour:minute and [8,9,10]an
extra keyword.")

(defvar shimbun-asahi-content-start
  "<!--[\t\n ]*Start of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE START NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-content-end
  "<!--[\t\n ]*End of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE END NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-x-face-alist
  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

(defvar shimbun-asahi-expiration-days 6)

(luna-define-method initialize-instance :after ((shimbun shimbun-asahi)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun "$BD+F|?7J9(B")
  (shimbun-set-from-address-internal shimbun "nobody@example.com")
  ;; To share class variables between `shimbun-asahi' and its
  ;; successor `shimbun-asahi-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-asahi-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun shimbun-asahi-expiration-days)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-asahi))
  (mapcar 'car shimbun-asahi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-asahi))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-asahi-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi))
  (let* ((group (shimbun-current-group-internal shimbun))
	 (index (nth 2 (assoc group shimbun-asahi-group-table))))
    (if (string-match "\\`http:" index)
	index
      (concat shimbun-asahi-url (format index group)))))

(defun shimbun-asahi-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	(case-fold-search t)
	regexp jname numbers cyear cmonth rss-p paper-p en-category
	hour-min month year day serial num extra rgroup id headers
	backnumbers)
    (setq regexp (assoc group shimbun-asahi-group-table)
	  jname (nth 1 regexp)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp)
			 (regexp-quote (shimbun-subst-char-in-string
					?. ?/ group)))
	  cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear)
	  rss-p (string-equal group "rss")
	  paper-p (member group '("editorial" "tenjin")))
    (catch 'stop
      ;; The loop for fetching all the articles in the whitemail group.
      (while t
	(while (re-search-forward regexp nil t)
	  (cond ((string-equal group "english")
		 (setq en-category
		       (save-excursion
			 (save-match-data
			   (if (re-search-backward "\
<h[0-9]\\([\n\t ]+[^>]+\\)?>[\t\n ]*\\([^&]+\\)[\t\n ]*&#[0-9]+"
						   nil t)
			       (downcase (match-string 2)))))))
		(t
		 (setq hour-min
		       (save-excursion
			 (save-match-data
			   (if (re-search-forward "\
<span[\t\n ]+[^>]+>[\t\n ]*(\\([01]?[0-9]/[0-3]?[0-9][\t\n ]+\\)?
\\([012]?[0-9]:[0-5][0-9]\\))[\t\n ]*</span>"
						  nil t)
			       (match-string 2)))))))
	  (setq month (string-to-number (match-string (nth 5 numbers)))
		year (if (setq num (nth 4 numbers))
			 (string-to-number (match-string num))
		       (cond ((>= (- month cmonth) 2)
			      (1- cyear))
			     ((and (= 1 month) (= 12 cmonth))
			      (1+ cyear))
			     (t
			      cyear)))
		day (string-to-number (match-string (nth 6 numbers)))
		serial (cond (rss-p
			      (format "%d%s.%s"
				      year
				      (match-string (nth 1 numbers))
				      (match-string (nth 2 numbers))))
			     (paper-p
			      (format "%d%02d%02d" year month day))
			     ((and (setq num (nth 1 numbers))
				   (match-beginning num))
			      (format "%d%02d%02d.%s"
				      year month day (match-string num)))
			     (t
			      (shimbun-subst-char-in-string
			       ?/ ?.
			       (downcase (match-string (nth 2 numbers))))))
		extra (or (and (setq num (nth 8 numbers))
			       (match-beginning num)
			       (match-string num))
			  (and (setq num (nth 9 numbers))
			       (match-beginning num)
			       (match-string num)))
		rgroup (mapconcat 'identity
				  (nreverse (save-match-data
					      (split-string group "\\.")))
				  ".")
		id (if (and extra
			    (not (member group '("job.special"))))
		       (concat "<" serial "%" extra "." rgroup "."
			       shimbun-asahi-top-level-domain ">")
		     (concat "<" serial "%" rgroup "."
			     shimbun-asahi-top-level-domain ">")))
	  (unless (and (shimbun-search-id shimbun id)
		       (if backnumbers
			   (throw 'stop nil)
			 ;; Don't stop it since there might be more new
			 ;; articles even if the same links are repeated.
			 t))
	    (push (shimbun-create-header
		   ;; number
		   0
		   ;; subject
		   (cond (rss-p
			  (match-string (nth 3 numbers)))
			 (en-category
			  (concat "[" en-category "] "
				  (match-string (nth 3 numbers))))
			 ((and (setq num (nth 8 numbers))
			       (match-beginning num))
			  (concat "[" (match-string num) "] "
				  (match-string (nth 3 numbers))))
			 ((and (setq num (nth 9 numbers))
			       (match-beginning num))
			  (concat "[" (match-string num) "] "
				  (match-string (nth 3 numbers))))
			 (paper-p
			  (concat jname (format " (%d/%d)" month day)))
			 (t
			  (match-string (nth 3 numbers))))
		   ;; from
		   (if (and rss-p
			    (setq num (nth 10 numbers))
			    (setq num (match-string num)))
		       (save-match-data
			 (shimbun-replace-in-string
			  from "(RSS" (concat "\\&:" num)))
		     from)
		   ;; date
		   (shimbun-make-date-string
		    year month day (cond ((and (setq num (nth 7 numbers))
					       (match-beginning num))
					  (match-string num))
					 (paper-p
					  "07:00")
					 (t
					  hour-min)))
		   ;; id
		   id
		   ;; references, chars, lines
		   "" 0 0
		   ;; xref
		   (shimbun-expand-url
		    (match-string (nth 0 numbers))
		    (if paper-p
			(concat shimbun-asahi-url "paper/")
		      shimbun-asahi-url)))
		  headers)))
	(if (string-equal group "nankyoku.whitemail")
	    (progn
	      (cond ((eq backnumbers 'stop)
		     (throw 'stop nil))
		    ((null backnumbers)
		     (while (re-search-forward "<a[\t\n ]+href=\"\
\\(http://www\\.asahi\\.com/nankyoku/whitemail/\
backnum0[345][01][0-9]\\.html\\)\">"
					       nil t)
		       (unless (member (setq id (match-string 1)) backnumbers)
			 (push id backnumbers)))))
	      (if backnumbers
		  (progn
		    (shimbun-retrieve-url
		     (prog1
			 (car backnumbers)
		       (erase-buffer)
		       (unless (setq backnumbers (cdr backnumbers))
			 (setq backnumbers 'stop)))))
		(throw 'stop nil)))
	  (throw 'stop nil))))
    (append (shimbun-sort-headers headers)
	    (shimbun-asahi-get-headers-for-today group jname from))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi)
					 &optional range)
  (shimbun-asahi-get-headers shimbun))

(defun shimbun-asahi-get-headers-for-today (group jname from)
  "Return a list of the header for today's article.
It works for only the groups `editorial' and `tenjin'."
  (goto-char (point-min))
  (let ((basename (cdr (assoc group '(("editorial" . "editorial")
				      ("tenjin" . "column")))))
	year month day url)
    (when (and basename
	       (re-search-forward
		(concat
		 ;; 1. year
		 "\\(20[0-9][0-9]\\)" "$BG/(B"
		 ;; 2. month
		 "\\([01]?[0-9]\\)" "$B7n(B"
		 ;; 3. day
		 "\\([0-3]?[0-9]\\)" "$BF|(B"
		 "$B!J(B.$BMKF|!KIU(B")
		nil t))
      (setq year (string-to-number (match-string 1))
	    month (string-to-number (match-string 2))
	    day (string-to-number (match-string 3))
	    url (format "paper/%s%d%02d%02d.html" basename year month day))
      (list
       (shimbun-make-header
	;; number
	0
	;; subject
	(shimbun-mime-encode-string (concat jname
					    (format " (%d/%d)" month day)))
	;; from
	from
	;; date
	(shimbun-make-date-string year month day "07:00")
	;; id
	(format "<%d%02d%02d%%%s.%s>"
		year month day group shimbun-asahi-top-level-domain)
	;; references, chars, lines
	"" 0 0
	;; xref
	(shimbun-expand-url url shimbun-asahi-url))))))

(defun shimbun-asahi-prepare-article (shimbun header)
  "Prepare an article.
Extract the article core on some groups or adjust a date header if
there is a correct information available.  For the groups editorial
and tenjin, it tries to fetch the article for that day if it failed."
  (let ((case-fold-search t)
	(group (shimbun-current-group-internal shimbun)))
    (cond
     ((string-equal group "editorial")
      (let ((regexp "\
<h[0-9]\\([\t\n ]+[^>]+\\)?>[\t\n ]*<a[\t\n ]+name=\"syasetu[0-9]+\">")
	    (retry 0)
	    index)
	(while (<= retry 1)
	  (if (re-search-forward regexp nil t)
	      (progn
		(goto-char (match-beginning 0))
		(insert "<!-- Start of Kiji -->")
		(when index
		  (insert "\
\n<p>($B;XDj$5$l$?(B&nbsp;url&nbsp$B$,(B&nbsp$B$^$@(B/$B$9$G$K(B&nbsp$BL5$$$N$G!"(B\
<a href=\"" index "\">$B%H%C%W%Z!<%8(B</a> $B$+$i5-;v$r<hF@$7$^$7$?(B)</p>\n"))
		(search-forward "</a>" nil t)
		(while (re-search-forward regexp nil t))
		(when (re-search-forward "[\n\t ]*</p>" nil t)
		  (insert "\n<!-- End of Kiji -->"))
		(setq retry 255))
	    (erase-buffer)
	    (if (zerop retry)
		(progn
		  (shimbun-retrieve-url (setq index
					      (shimbun-index-url shimbun)))
		  (goto-char (point-min)))
	      (insert "Couldn't retrieve the page.\n")))
	  (setq retry (1+ retry)))))
     ((string-equal group "tenjin")
      (let ((retry 0)
	    index)
	(while (<= retry 1)
	  (if (and (search-forward "$B!ZE7@<?M8l![(B" nil t)
		   (re-search-forward "<SPAN STYLE=[^>]+>[\t\n ]*" nil t))
	      (progn
		(insert "<!-- Start of Kiji -->")
		(when index
		  (insert "\
\n<p>($B;XDj$5$l$?(B&nbsp;url&nbsp$B$,(B&nbsp$B$^$@(B/$B$9$G$K(B&nbsp$BL5$$$N$G!"(B\
<a href=\"" index "\">$B%H%C%W%Z!<%8(B</a> $B$+$i5-;v$r<hF@$7$^$7$?(B)</p>\n"))
		(while (re-search-forward "[\t\n ]*<SPAN STYLE=[^>]+>[\t\n ]*"
					  nil t)
		  (delete-region (match-beginning 0) (match-end 0)))
		(when (re-search-forward "[\t\n ]*</SPAN>" nil t)
		  (goto-char (match-beginning 0))
		  (insert "\n<!-- End of Kiji -->"))
		(setq retry 255))
	    (erase-buffer)
	    (if (zerop retry)
		(progn
		  (shimbun-retrieve-url (setq index
					      (shimbun-index-url shimbun)))
		  (goto-char (point-min)))
	      (insert "Couldn't retrieve the page.\n")))
	  (setq retry (1+ retry)))))
     (t
      (when (re-search-forward
	     (eval-when-compile
	       (let ((s0 "[\t\n ]*")
		     (s1 "[\t\n ]+"))
		 (concat "<p" s1 "class" s0 "=" s0 "\"day\"" s0 ">" s0
			 ;; 1. year
			 "\\(20[0-9][0-9]\\)$BG/(B"
			 ;; 2. month
			 "\\([01]?[0-9]\\)$B7n(B"
			 ;; 3. day
			 "\\([0-3]?[0-9]\\)$BF|(B"
			 ;; 4. hour
			 "\\([012]?[0-9]\\)$B;~(B"
			 ;; 5. minute
			 "\\([0-5]?[0-9]\\)$BJ,(B"
			 s0 "</p>")))
	     nil t)
	(shimbun-header-set-date
	 header
	 (shimbun-make-date-string
	  (string-to-number (match-string 1))
	  (string-to-number (match-string 2))
	  (string-to-number (match-string 3))
	  (concat (match-string 4) ":" (match-string 5))
	  "+0900"))))))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-asahi)
						   header)
  (shimbun-asahi-prepare-article shimbun header))

(provide 'sb-asahi)

;;; sb-asahi.el ends here
