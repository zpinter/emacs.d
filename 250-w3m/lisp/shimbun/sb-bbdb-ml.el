;;; sb-bbdb-ml.el --- shimbun backend for bbdb-ml

;; Copyright (C) 2001, 2005 Akihiro Arisawa   <ari@mbf.sphere.ne.jp>
;; Copyright (C) 2001, 2005 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Akihiro Arisawa   <ari@mbf.sphere.ne.jp>,
;;         Yuuichi Teranishi <teranisi@gohome.org>
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
(require 'sb-fml)

(luna-define-class shimbun-bbdb-ml (shimbun-fml) ())

(defvar shimbun-bbdb-ml-url "http://heimat.jp/~nakaji/bbdb/")
(defvar shimbun-bbdb-ml-groups '("bbdb-ml"))

(provide 'sb-bbdb-ml)

;;; sb-bbdb-ml.el ends here
