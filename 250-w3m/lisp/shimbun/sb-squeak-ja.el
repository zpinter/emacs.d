;;; sb-squeak-ja.el --- shimbun backend for Squeak-ja's ML archive

;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-mailman)

(luna-define-class shimbun-squeak-ja (shimbun-mailman-ja) ())

(defvar shimbun-squeak-ja-url
  "http://www.smalltalk.jp/pipermail/squeak-ja/")

(defvar shimbun-squeak-ja-groups '("main"))

(provide 'sb-squeak-ja)
;;; sb-squeak-ja.el ends here
