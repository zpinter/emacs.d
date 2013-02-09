;;; json-pretty-print.el --- JSON pretty-printing functions

;; Copyright (C) 2012 Ryan Crum

;; Author: Ryan Crum
;; Git: git://github.com/thorstadt/json-pretty-print.git
;; Version: 1.0
;; Created: 2011-05-17
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library depends on the version of json.el with pretty-printing.
;; It can be found at http://github.com/thorstadt/json.el

(require 'json)

(defun json-pretty-print-buffer ()
  (interactive)
  (let ((json-encoding-pretty-print t))
      (let ((json-string (json-encode (json-read-from-string (buffer-string))))
            (buf (current-buffer)))
        (with-current-buffer buf
          (erase-buffer)
          (insert json-string)))))

(defun json-pretty-print ()
  (interactive)
  (unless mark-active
    (error "No region selected."))
  (let ((begin (region-beginning))
        (end (region-end)))
    (kill-region begin end)
    (let ((json-encoding-pretty-print t))
      (insert (json-encode (json-read-from-string (current-kill 0)))))))

(provide 'json-pretty-print)
