;;; mirah-complete.el --- Auto Java Completion fo GNU Emacs
;; This file is NOT part of GNU Emacs
;; plesase send Bug reports and suggestions to 'Joseph at <jixiuf@gmail.com>

;;{{{ License 

;;  License
        
;; Copyright (C) 2011  joseph <jixiuf@gmail.com> Limited

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Firstly I am not an English Speaker ,so forgive my bad English .
;;;this is "Auto Java Complete". 

;;}}}

;;{{{ Commentary

;;;this is "Auto Java Complete".

;; you can download a Video demo (2.8M)
;;http://screencast-repos.googlecode.com/files/auto-mirah-complete-demo-editing-jsp-2011-01-20.mp4
;;http://screencast-repos.googlecode.com/files/auto-mirah-complete-demo-2010-12-25.mp4.bz2
;;
;;1. it depends on auto complete ,so it would complete
;;   everything by dropdowning a menu.

;;2. it is tag based . so before you used it on emacs ,you
;;   should generate a tag file by using Tags.java .
;;   about how to use it ,see the Install section.

;;3. it depends on yasnippet . when completing method and
;;   constructor it would generate a templete dynamically
;;   so that you can jump from a paramter to another one .

;;4. when completing method ,it can show return type behind
;;   each candidate on dropdown menu.
;;   I want to show
;;       toString() :String         on menu ,but just insert
;;       toString()                 in buffer ,
;;   but there is a problem:
;;   auto complete 1.3 now doesn't support it .
;;   I patched  popup.el in auto-complete-1.3/  ,
;;   the patched file is mirah-complete/popup.el, please
;;   put this file into auto-complete-1.3/ ,or use
;;      mirah-complete/popup-patch.diff
;;      cp  mirah-complete/popup-patch.diff auto-complete-1.3/
;;      cd auto-complete-1.3/
;;      patch -p0 < popup-patch.diff
;;
;;     don't forget to byte-compile it 

;;}}} 

;;{{{ Features

;; 1. support importing.
;;    when you type in  import javax.s-|-
;;    it would drop down a menu like this
;;             import  javax.s
;;
;;                     javax.sql
;;                     javax.swing
;;                     javax.sound
                       
;; 2. support import class with keybindings (even in jsp file)
;;         auto import all Class in source file
;;    (local-set-key (kbd "C-c i") (quote amc-import-all-unimported-class))
;;         import Class where under point 
;;    (local-set-key (kbd "C-c m") (quote amc-import-class-under-point))

;; 3. support completing class name ,you just need  typing
;;    in a Word beginning with [A-Z] ,then it would auto find
;;    matched class and list it with dropdown menu.


;; 4. support complete method.
;;    for example
;;    List<Map<String,Object>> list = new ArrayList<Map<String,Object>>();
;;         list.

;;    it would list all method like this
;;         list.
;;              equals(Object)
;;              add(Object)
;;              clear()

;;    it can do more
;;         list.listIterator().next().
;;                                      toString()
;;                                      getClass()
;;                                      notify()
                                      
;; 5. support complete constructor
;;    after keyword 'new' it would try to complete constructor


;; 6. support completing in jsp files.
      
;;    if you want Auto Java Complete works  when you edit
;;    jsp file ,you just need to do something like this

;;    If your want to enable  mirah-complete-mode when openning
;;    a jsp file. you can
;;          (add-hook 'jsp-mode 'mirah-complete-mode)
;;    if you has a jsp-mode,
;;    if not ,you can do it like this
;;         (add-hook 'find-file-hook 'amc-4-jsp-find-file-hook)
      
;;    now it can complete class name,method ,constructor
;;    it also support complete importing ,but it isn't auto completed,
;;    you must trigger it by a key binding
;;    for example (define-key ac-mode-map (kbd "M-1") 'auto-complete)
;;    <%@ page language="java" import="java.io.File,java.util.Map,javax.sw-|-"%>
;;    now you can  press M-1 to show the menu.
;;    

;;}}} 

;;{{{ Install

;; 1. generate the tag file .
;;
;;     Tags.java use JDK reflection to generate a tag file by
;;     loading all class in classpath ,what you need to do is
;;     just add your jars to $CLASSPATH. don't drop it in
;;     JRE_HOME/lib/ext/ , the suggestion is
;;     export CLASSPATH=$CLASSPATH:your-jar-path
;;     it need about 3~10 min depending on your jars
;;     during it ,you may see some exceptions ,if it don't kill
;;     the program ,just ignore it .
;;     run 
;;                 javac Tags.java 
;;                 java  Tags
;;
;;     to generate the tag file ~/.java_base.tag 
;;     or
;;                 java  Tags com.yourcompanyname.*
;;
;;     it would only tag those class whose name is starts with
;;     com.yourcompanyname.
;;
;;     if it can't work on you computer ,use my tag file
;;     java_base.tag.bz2, just uncompress and rename it to
;;     .java_base.tag and put it in your home directory.
;;     of course you can change the name by customing
;;                 `amc-tag-file'


;;  2. you should have installed  auto-complete and yasnippet.
;;     about how to install and config them ,you should find
;;     it on the net.
;;     after installed auto-complete ,you should do some
;;       patch on auto-complete-1.3/popup.el
;;       tow choice :
;;        1. put the mirah-complete/popup.el into auto-complete-1.3/
;;           (recommand)
;;        2. cd auto-complete-1.3/
;;           patch -p0 <popup-patch.diff
;;        and don't forget to byte compile it. 

;;  3. then  add this lines  in .emacs

;;       (add-to-list 'load-path "~/.emacs.d/mirah-complete/")
;;       (require 'mirah-complete-config)
;;       (add-hook 'java-mode-hook 'mirah-complete-mode)
;;       (add-hook 'find-file-hook 'amc-4-jsp-find-file-hook)
;;       read mirah-complete-config.el  for more info .


;;     restart your emacs ,and enjoy.
;;}}}

;;{{{ History
;; tag  0.2.6
;;     no new feature ,just Bugfix,and improve Tags.java
;;     make Tags.java  more strong.
;;     and complete constructor gently.
;;     read comments in Tags.java
;;     
;;
;; tag  0.2.5
;;    define minor-mode (mirah-complete-mode).
;;    add function (amc-4-jsp-find-file-hook).
;;    to enable Auto Java Complete ,just need to add this minor-mode in
;;    your mode hook, for example
;;          (add-hook 'java-mode-hook 'mirah-complete-mode)
;;    If your want to enable  mirah-complete-mode when openning
;;    a jsp file. you can
;;          (add-hook 'jsp-mode 'mirah-complete-mode)
;;    if you has a jsp-mode,
;;    if not ,you can do it like this
;;         (add-hook 'find-file-hook 'amc-4-jsp-find-file-hook)


;; tag  0.2.4
;;      a litter change of tag file.
;;      replace toString`25:784`` to   toString`784`` in tag file
;;      package line 25 is not needn't now .
;;      so the old tag file doesn't work with this version.
;;      you need regenerate it by using Tags.java

;; tag 0.2.3
;;     support importing class under point ,and importing
;;     all class in buffer when editing jsp files 
;;
;; tag 0.2.2
;;     support completion in jsp files.
    
;;     if you want Auto Java Complete works  when you edit
;;     jsp file ,just need to do something like this
    
;;       (add-hook 'nxml-mode-hook 'mirah-complete-hook t)
      
;;     now it can complete class name,method ,constructor
;;     it also support complete importing ,but it isn't auto completed,
;;     you must trigger it by a key binding
;;     for example (define-key ac-mode-map (kbd "M-1") 'auto-complete)
;;     <%@ page language="java" import="java.io.File,java.util.Map,javax.sw-|-"%>
;;     now you can  press M-1 to show the menu.
    
;;     it does not support importing Class(importing Class under point
;;     ,and importing all Class in buffer) by keybinding ,I will try to make
;;     it works later.


;; tag 0.2.1  same to tag 0.2 ,just add comments in README .

;; tag 0.2
;;    add popup.el and popup-patch.diff
;;    support of showing return type behind each method candidate ,
;;    by make a patch on auto-complete-1.3/popup.el
   

;; tag 0.1.1
;;    add support of showing return type behind each method candidate,
;;    by define an advice on  (ac-expand-common) and (ac-selected-candidates)
;;    but if auto-complete.el is byte-compiled ,this advice doesn't work

;;}}}   
;;---------------------------------------------
;;; Code.


(defgroup auto-mirah-complete nil
  "Auto Java Completion."
  :group 'convenience
  :prefix "auto-mirah-complete")

(defcustom amc-use-short-class-name t
  "if it is not nil then ,when complete method and constructor,
  params and exceptions will use short class name ,
  instead of full class name"
  :type 'boolean
  :group 'auto-mirah-complete)

(defcustom amc-tag-file "~/.java_base.tag"
  "the tag file is  used for java complete ,it  is generate by a Tags.java ,
so before you use this tool ,try to compile Tags.java
          javac Tags.java
and  use this class like this 
         java  Tags  
 it will tag all jars in classpath to tag file , or
         java Tags   com.whatever.* 
 just tag class under com.whatever packages "
  :type 'string
  :group 'auto-mirah-complete)


(defcustom amc-default-length-of-class 36
  "the length of class name at dropdown-menu ,if the class
name is shorter than this value ,then empty string are append
.and return type are at position 37 "
  :type 'integer
  :group 'auto-mirah-complete)

(defcustom amc-return-type-char ":"
  "the char  before return type when
  completing methods."
  :type 'string
  :group 'auto-mirah-complete
  )
(defcustom amc-throws-char "   #"
  "the char  before Exceptions  when completing
  method"
  :type 'string
  :group 'auto-mirah-complete
  )

;;private variables
(defvar amc-is-running nil "after (amc-init) it will become true")
(defvar amc-all-sorted-class-items nil "this is a list,
all the element are sorted class-item
this variable should work with amc-two-char-list,
then search class is faster ")

(defvar amc-two-char-list nil 
  "in this list ,it looks like '((Ab 1 3 ) (Ac 4 15))that means
    all class those name starts with Ab are in the position of 
  0~2 (because index from 0 not 1) in amc-all-sorted-class-items " )

(defvar amc-tag-buffer nil "this is the buffer of .java_base.tag" )
(defvar amc-package-first-ln 0
  "the first line number of the package section in tag file")
(defvar amc-class-first-ln 0
"the first line number of the class section in tag file,
it is the end of package section line number too. " )
(defvar amc-member-first-ln 0
"the first line number of the member section in tag file ,
actually it is the end of package section line number too" )
(defvar amc-member-end-ln 0
  "the end line number of the member section in tag file ,
it is the last line number in tag file" )

(defvar amc-matched-class-items-cache nil
  "when search class-prefix without package-name ,
  it will search thoudsands of lines in tags files ,
  so this will cache for next match maybe  ")
(defvar amc-previous-class-prefix nil "cache last class-prefix ")

(defvar amc-matched-import-cache-list nil
  "when complete a import ,sometimes we can use
 the last completed items for next complete  ")
(defvar amc-previous-matched-import-prefix nil
 "previous matched prefix for import Class at head of source")

(defvar amc-current-class-prefix-4-complete-class nil
 "when (amc-is-available-4-complete-class-p ) return true,
 it will save current class-prefix in this variable ,so
 (amc-complete-class-candidates) can reuse it . ")

(defun amc-goto-line ( line-num &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when (numberp line-num) 
    (goto-char (point-min))
    (forward-line (1- line-num )))))

(defun amc-read-line(line-number  &optional buffer)
  "read a line  and return the string"
  (with-current-buffer (or buffer (current-buffer))
    (amc-goto-line line-number)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun amc-split-string-with-separator(str regexp &optional replacement OMIT-NULLS)
  "this function is a tool like split-string,
  but it treat separator as an element of returned list
  for example (amc-split-string-with-separator abc.def.g \\. .)
  will return '(abc . def . g )"
  (if (and str  ( > (length str ) 0))
      (let ((split-list)  (substr) (match-end ))
        (if  (string-match regexp str)
            (progn (while (string-match regexp  str  )
                     (setq match-end (match-end 0))
                     (setq  substr (substring-no-properties str 0 (- match-end 1)))
                     (if OMIT-NULLS 
                         (if (> (length substr ) 0)
                             (setq split-list (append split-list (list  substr))))
                       (setq split-list (append split-list (list  substr))))
                     (if replacement 
                         (setq split-list (append split-list (list replacement)))
                       (setq split-list (append split-list (list regexp))))
                     (setq str (substring-no-properties str  match-end)))
                   (if OMIT-NULLS 
                       (if (> (length str ) 0)
                           (setq split-list (append split-list (list str))))
                     (setq split-list (append split-list (list  str))))
                   (setq split-list split-list))
          (setq split-list (list str))))))

(defun amc-split-pkg-item ( pkg-line-string )
 "the format pkg-line-string is  str`num`num
  this function translate it to a list ,the num will be string2number
  give me  `java.lang`222`333 ,return '(\"java.lang\" 222 333 ) "
  (let (( pkg-item (split-string pkg-line-string "`" t))) 
    (setcar (nthcdr 1 pkg-item ) (string-to-number (nth 1 pkg-item )))
    (setcar (nthcdr 2 pkg-item ) (string-to-number (nth 2 pkg-item )))
     pkg-item))

(defun amc-split-pkg-item-by-pkg-ln ( pkg-line-number  &optional buffer )
  "the format pkg-line-string is str`num`num
   this function translate it to a list ,the num will be
   string2number return a list of pkg info of line-number "
  (amc-split-pkg-item
   (amc-read-line pkg-line-number
                  (or buffer (amc-reload-tag-buffer-maybe)))  ))

(defun amc-split-class-item ( class-line-string )
 "the format of class-line-string is
  classname`packageLineNum`memberStartLineNum`memberEndLineNum
  this function translate it to a list ,the num will be convert to number "
  (let (( class-item (split-string class-line-string "`" t))) 
    (setcar (nthcdr 1 class-item ) (string-to-number (nth 1 class-item )))
    (setcar (nthcdr 2 class-item) (string-to-number (nth 2 class-item )))
    (setcar (nthcdr 3 class-item) (string-to-number (nth 3 class-item )))
     class-item))

(defun amc-split-class-item-by-class-ln
  ( class-line-number  &optional buffer )
  (amc-split-class-item
   (amc-read-line class-line-number
                  (or buffer (amc-reload-tag-buffer-maybe)))))

(defun amc-split-constructor-by-line-num ( constructor-line-num )
   (amc-split-constructor
    (amc-read-line constructor-line-num
                   (amc-reload-tag-buffer-maybe)))) 

(defun amc-split-field (field-line-string)
  (let* ((field-item)
         (field-line-string (substring-no-properties field-line-string 1))
         (split-list (split-string  field-line-string "`"))
         (return-type (nth 1 split-list)))
    ;;handle field name
    (add-to-list  'field-item  (car split-list) t)
    (if (string-match  "^~" return-type )
        (add-to-list 'field-item (substring-no-properties  return-type 1) t)
      (add-to-list 'field-item (amc-split-class-item-by-class-ln 
                                (string-to-number return-type)) t))
    field-item ))

;(amc-field-to-string (amc-split-field " in`24:691" ))
(defun amc-field-to-string (field-item &optional with-return-type)
  (if  with-return-type 
      (let ((field-string  (car field-item)) (return-type (nth 1 field-item)))
        (let ((len (length field-string)));; insert whitespace between classname and return type
      (if (< len (- amc-default-length-of-class 3))
          (setq field-string
                (concat field-string
                        (make-string (- (- amc-default-length-of-class 3) len ) 32 )));;32 mean whitespace
         (setq field-string (concat field-string "     "))))
        (setq field-string (concat field-string amc-return-type-char))
        (when (stringp return-type)
          (setq field-string (concat field-string return-type )   ))
        (when (listp return-type)
          (if amc-use-short-class-name
              (setq field-string (concat field-string   (car return-type)))
            (setq field-string (concat field-string 
                                       (car (amc-split-pkg-item-by-pkg-ln (nth 1 return-type)))  "."  
                                       (car return-type)))))
        field-string)
    (car field-item)))


(defun amc-method-to-string (method-item &optional  with-return-type-and-throws )
  "this is a toString() like function .
   when param with-detail is not null, it will include
  return type and exceptions, default it only include method name
  and params"
  (let ((method-string  (car method-item))
       (return-type (nth 1 method-item)   )
       (params (nth 2 method-item)   )
       (exceptions (nth 3 method-item)))
    (if (stringp params )
        (setq method-string (concat method-string "()")) 
        (setq method-string (concat method-string "("))                
        (dolist (param  params )
          (when (stringp param )
            (setq method-string (concat method-string param " , " )))
          (when (listp param) 
            (if amc-use-short-class-name 
                (setq method-string (concat method-string  (car param)  " , " )) 
              (setq method-string
                    (concat method-string
                            (car (amc-split-pkg-item-by-pkg-ln (nth 1 param)))  "."  
                            (car param)  " , " )))))
        (setq method-string
              (replace-regexp-in-string  " , $" ")" method-string )))
    (when with-return-type-and-throws
      (let ((len (length method-string)));; insert whitespace between classname and return type
      (if (< len amc-default-length-of-class )
          (setq method-string
                (concat method-string
                        (make-string (- amc-default-length-of-class len ) 32 )));;32 mean whitespace
         (setq method-string (concat method-string "     "))))
      (if (stringp return-type)
          (setq method-string (concat method-string amc-return-type-char  return-type  ))
        (when (listp return-type)
          (if amc-use-short-class-name
              (setq method-string (concat method-string amc-return-type-char  (car return-type)))
            (setq method-string (concat method-string amc-return-type-char
                                        (car (amc-split-pkg-item-by-pkg-ln (nth 1 return-type)))  "."  
                                        (car return-type))))))
      (when (listp exceptions )  
        (setq method-string (concat method-string amc-throws-char))                
        (dolist (exception  exceptions )
          (when (stringp exception ) (setq method-string (concat method-string exception " , " )))
          (when (listp exception) 
            (if amc-use-short-class-name 
                (setq method-string (concat method-string  (car exception)  " , " )) 
              (setq method-string (concat method-string
                                          (car (amc-split-pkg-item-by-pkg-ln (nth 1 exception)))  "."  
                                          (car exception)  " , " )))))
        (setq method-string  (replace-regexp-in-string  ", $" "" method-string ))) 
      )
    method-string ))

(defun amc-class-to-string(class-item &optional  with-package-name-append)
  (let* ((class-string (car class-item)) (len (length class-string)))
    (when with-package-name-append
      (if (< len amc-default-length-of-class )
          (setq class-string
                (concat class-string
                        (make-string (- amc-default-length-of-class len ) 32 )));;32 mean whitespace
        (setq class-string (concat class-string "     ")))
      (setq class-string
            (concat class-string amc-return-type-char
                    (car (amc-split-pkg-item-by-pkg-ln (nth 1 class-item))))))
    class-string
    ))

;; (yas/expand-snippet(amc-method-to-yasnippet-templete    (car 
;; (amc-find-members (car  (amc-find-out-matched-class-item-without-package-prefix "FileWriter" t )) "write" ))))
;; (amc-method-to-string    (car 
;; (amc-find-members (car  (amc-find-out-matched-class-item-without-package-prefix "String" t )) "split" )))
;; "split(String , int)"
;(yas/expand-snippet "split(${1:String} , ${2:int})"
(defun amc-method-to-yasnippet-templete (method-item)
  (let ((method-string  (car method-item))
       (params (nth 2 method-item)   )
       (exceptions (nth 3 method-item)))
    (if (stringp params ) (setq method-string (concat method-string "()")) 
      (progn 
        (setq method-string (concat method-string "("))
        (let ((index 0) (length-of-params (length params))(param))
          (while (< index length-of-params)
            (setq param (nth index params ))
            (when (stringp param ) (setq method-string
                                      (concat  method-string "${" (number-to-string (+ index 1)) ":"
                                                param "} , " )))
            (when (listp param) 
              (if amc-use-short-class-name 
                  (setq method-string (concat method-string "${" (number-to-string (+ 1 index )) ":"
                                                (car param)  "} , " )) 
                (setq method-string (concat method-string "${" (number-to-string (+ 1 index)) ":"
                                            (car (amc-split-pkg-item-by-pkg-ln (nth 1 param)))  "."  
                                            (car param)  "} , " ))))
            (setq index (+ 1 index ))))
        (setq method-string  (replace-regexp-in-string  " , $" ")$0" method-string ))))
    (setq method-string method-string)))

(defun amc-split-method ( method-line-string )
  (let ((method-item) (split-list)(return-type))
    (setq split-list (split-string  method-line-string "`"))
    ;;handle method name
    (add-to-list  'method-item  (car split-list) t)
    (setq return-type (nth 1 split-list))
    (if (string-match  "^~" return-type )
        (add-to-list 'method-item (substring-no-properties  return-type 1) t)
      (add-to-list 'method-item  (amc-split-class-item-by-class-ln 
                                  (string-to-number return-type)) t))
    ;;handle params if exists
    (if (not  (string-equal "" (nth 2 split-list)))
        (let ((params)(param-split-list)) 
          (setq param-split-list (split-string (nth 2 split-list)  "," t))
          (dolist (param param-split-list)
            (if (string-match  "^~" param )
                (setq params  (append  params  (list (substring-no-properties param 1 ))))
                (setq params (append params (list (amc-split-class-item-by-class-ln 
                                                   (string-to-number param)))))))
          (setq method-item (append method-item (list params)))) 
      (setq method-item (append method-item  (list ""))))
    (if (not  (string-equal "" (nth 3 split-list)))
        (let ((exceptions)(exception-split-list)) 
          (setq exception-split-list (split-string (nth 3 split-list)  "," t))
          (dolist (exception exception-split-list)
            (if (string-match  "^~" exception )
                (setq exceptions  (append  exceptions  (list (substring-no-properties exception 1 ))))
                (setq exceptions (append exceptions (list (amc-split-class-item-by-class-ln 
                                      (string-to-number exception)))))))
          (setq method-item (append method-item (list exceptions)))) 
      (setq method-item (append method-item  (list ""))))   
      method-item))

(defun amc-split-constructor(constructor-line-string)
  (let ((constructor-item) (split-list))
    (setq constructor-line-string (substring-no-properties constructor-line-string 2))
    (setq split-list (split-string  constructor-line-string "`"))
    ;;handle constructor name
    (add-to-list  'constructor-item  (car split-list) t)
    ;;handle params if exists
    (if (not  (string-equal "" (nth 1 split-list)))
        (let ((params)(param-split-list)) 
          (setq param-split-list (split-string (nth 1 split-list)  "," t))
          (dolist (param param-split-list)
            (if (string-match  "^~" param )
                (setq params  (append  params  (list (substring-no-properties param 1 ))))
                (setq params (append params (list (amc-split-class-item-by-class-ln 
                                                   (string-to-number param)))))))
          (setq constructor-item (append constructor-item (list params)))) 
      (setq constructor-item (append constructor-item  (list ""))))
    (if (not  (string-equal "" (nth 2 split-list)))
        (let ((exceptions)(exception-split-list)) 
          (setq exception-split-list (split-string (nth 2 split-list)  "," t))
          (dolist (exception exception-split-list)
            (if (string-match  "^~" exception )
                (setq exceptions  (append  exceptions  (list (substring-no-properties exception 1 ))))
              (progn 
                (setq exceptions (append exceptions (list (amc-split-class-item-by-class-ln 
                                               (string-to-number exception)))))))) 
          (setq constructor-item (append constructor-item (list exceptions)))) 
      (setq constructor-item (append constructor-item  (list ""))))
    constructor-item))

(defun amc-constructor-to-string (constructor-item &optional is-with-exceptions)
  (let((constructor-string  (car constructor-item))
       (params (nth 1 constructor-item)   )
       (exceptions (nth 2 constructor-item)))
    (if (stringp params ) (setq constructor-string (concat constructor-string "()")) 
      (progn 
        (setq constructor-string (concat constructor-string "("))                
        (dolist (param  params )
          (when (stringp param ) (setq constructor-string (concat constructor-string param " , " )))
          (when (listp param) 
            (if amc-use-short-class-name 
                (setq constructor-string (concat constructor-string  (car param)  " , " )) 
              (setq constructor-string (concat constructor-string
                                               (car (amc-split-pkg-item-by-pkg-ln (nth 1 param)))  "."  
                                               (car param)  " , " )))))
        (setq constructor-string  (replace-regexp-in-string  " , $" ")" constructor-string ))))
    (when is-with-exceptions
    (when (listp exceptions )  
      (setq constructor-string (concat constructor-string amc-throws-char))
      (dolist (exception  exceptions )
        (when (stringp exception ) (setq constructor-string (concat constructor-string exception " , " )))
        (when (listp exception) 
          (if amc-use-short-class-name 
              (setq constructor-string (concat constructor-string  (car exception)  " , " )) 
            (setq constructor-string (concat constructor-string
                                             (car (amc-split-pkg-item-by-pkg-ln (nth 1 exception)))  "."  
                                             (car exception)  " , " )))))
      (setq constructor-string  (replace-regexp-in-string  ", $" "" constructor-string )))) 
    constructor-string))

(defun amc-constructor-to-yasnippet-templete (constructor-item)
  (let((constructor-string  (car constructor-item))
       (params (nth 1 constructor-item)   )
       (exceptions (nth 2 constructor-item)))
    (if (stringp params ) (setq constructor-string (concat constructor-string "()")) 
      (progn 
        (setq constructor-string (concat constructor-string "("))
        (let ((index 0) (length-of-params (length params))(param))
          (while (< index length-of-params)
            (setq param (nth index params ))
            (when (stringp param ) (setq constructor-string
                                          (concat  constructor-string "${" (number-to-string (+ index 1)) ":"
                                                    param "} , " )))
            (when (listp param) 
              (if amc-use-short-class-name 
                  (setq constructor-string (concat constructor-string "${" (number-to-string (+ 1 index )) ":"
                                                (car param)  "} , " )) 
                (setq constructor-string (concat constructor-string "${" (number-to-string (+ 1 index)) ":"
                                            (car (amc-split-pkg-item-by-pkg-ln (nth 1 param)))  "."  
                                            (car param)  "} , " ))))
            (setq index (+ 1 index ))
            ))
        (setq constructor-string  (replace-regexp-in-string  " , $" ")$0" constructor-string ))))
    (setq constructor-string constructor-string)))

;; find tag file 
(defun amc-init()
  "find java tag file and do some initial works, like  populate some variables "
  (unless amc-is-running
    (setq amc-tag-file (file-truename (expand-file-name amc-tag-file  )))
    (if (file-exists-p  amc-tag-file)
        (with-current-buffer (find-file-noselect amc-tag-file )
          ;; a buffer name starts with empth string,means hidden this buffer
          (rename-buffer " *java-base.tag*")
          (setq amc-tag-buffer " *java-base.tag*")
          (setq buffer-read-only t)
          (setq case-fold-search nil) 
          (setq amc-package-first-ln  (string-to-number (amc-read-line 3)))
          (setq amc-class-first-ln    (string-to-number  (amc-read-line 4)))
          (setq amc-member-first-ln   (string-to-number (amc-read-line 5)))
          (setq amc-member-end-ln     (string-to-number (amc-read-line 6)))
          (amc-load-all-sorted-class-items-to-memory))
      (message  ( concat amc-tag-file " doesn't exists !!!")))
    (setq amc-is-running t)
    ))

;; (defun amc-init-when-load-first-java-file() "just add in a hook "
;;   (if (not amc-all-sorted-class-items)
;;       (amc-load-all-sorted-class-items-to-memory)))

(defun amc-reload-tag-buffer-maybe( ) 
  "check if the amc-tag-buffer is still live ,if not reload it "
  (unless amc-tag-buffer
    (amc-init))
  amc-tag-buffer)

(defun amc-find-out-matched-pkg-item
  (pkg-prefix &optional exactly_match  &optional buffer)
  "this function is used to find out all matched packaged whose prefix is pkg-prefix 
  for example: support  pkg-prefix=javax.xm  then it will return
   '( '(\"javax.xml.bind\" 2741 2767 ) '(\"javax.xml.bind.attachment\" 2776 2778 )) 
if exactly_match is not nil then pkg-prefix will be seen as full package name , and
we will suppose you are searching package name = pkg-prefix , if exactly_match is set
in normal only 1 or 0 item will returned so we will try to
 convert '((packageName 12 33 )) to '(packageName 12 33 ) "
  (with-current-buffer (or buffer  (amc-reload-tag-buffer-maybe))
    (let ((line-num amc-package-first-ln) (matched-package)
          (regexp-pkg-prefix (concat "^" (regexp-quote pkg-prefix)))
          (current-pkg-line))
      (if exactly_match ;;I use ` char as the separator in tag file
          (setq regexp-pkg-prefix (concat "^"  (regexp-quote  pkg-prefix )  "`" )))
      (while (< line-num amc-class-first-ln)
        (setq current-pkg-line (amc-read-line line-num))
        (if (string-match regexp-pkg-prefix  current-pkg-line)
            (add-to-list 'matched-package (amc-split-pkg-item current-pkg-line)))
        (setq line-num  (+ line-num 1 )))
      (when exactly_match (setq matched-package (car matched-package)))
      matched-package )))

(defun amc-shrunk-matched-pkgs (pkg-prefix matched-pkg-items)
  "this function is used for list matched package.
when you import a package in head of your java file,
when you typed in 'jav-|-', then it will list 'java javax'
instead of 'java.lang java.lang.rel javax.xml javax.xml.ws'"
  (let ((index-of-first-dot 0) (return-list)
        (length-of-pkg-prefix (length pkg-prefix)))
    (dolist (current-item matched-pkg-items)
      (if (setq index-of-first-dot
                (string-match "\\." (car current-item) length-of-pkg-prefix))
          (add-to-list 'return-list (substring-no-properties
                                     (car current-item) 0 index-of-first-dot))
        (add-to-list 'return-list (car current-item))))
    return-list))

(defun amc-find-class-first-check-imported(class-name)
  "this function will find class from imported classes,
if doesn't exists,find from the tag file,
if more than one class item matched class-name in tag file,
then imported one of them first"
  (let* ((imported-class (amc-caculate-all-imported-class-items))
         (matched-class-item))
    (dolist (current-class-item imported-class)
      (when (string-equal class-name (car current-class-item))
        (setq matched-class-item current-class-item)))
    (unless matched-class-item;;if not found from imported section
      (let ((matched-class-items
             (amc-find-out-matched-class-item-without-package-prefix class-name t)))
        (if (< (length matched-class-items) 2) 
            (setq matched-class-item (car matched-class-items)) 
          (setq matched-class-item
                (car (amc-insert-import-at-head-of-source-file matched-class-items))))))
    matched-class-item))
(defun amc-find-out-matched-class-item
  (package-name class-prefix &optional exactly_match &optional buffer)
  "this function is use to find out all Class whose package name is
package-name and ClassName is start with class-prefix if package-name
is nil, then try to find out all Class whose ClassName is start with
class-prefix if class-prefix is nil or empty string ,it will try to
find out all Class in package package-name if both  package-name
and class-prefix are nil then  it will return all Class in all package
the param exactly_match ,means only class name exactly equals
 to class-prefix will be return"
  (let ((matched-pkg-item )(return-list)(regexp-class-prefix)
        (line-num )(end-line-num)(current-line-string))
    (with-current-buffer (or buffer  (amc-reload-tag-buffer-maybe))
      (if package-name
          (progn
            (setq matched-pkg-item (amc-find-out-matched-pkg-item package-name t))
            (if matched-pkg-item
                (setq line-num (nth 1 matched-pkg-item)
                      end-line-num (nth 2 matched-pkg-item))
              (setq line-num   1 end-line-num  1)))
        (setq line-num   amc-class-first-ln end-line-num  amc-member-first-ln))
      (unless class-prefix (setq class-prefix ""))
      (if exactly_match (setq regexp-class-prefix
                              (concat "^" (regexp-quote class-prefix) "`" ))
        (setq regexp-class-prefix (concat "^" (regexp-quote class-prefix))))
      (while (< line-num end-line-num)
        (setq current-line-string (amc-read-line line-num))
        (when (string-match regexp-class-prefix current-line-string)
          (add-to-list 'return-list (amc-split-class-item current-line-string)))
        (setq line-num (1+ line-num)))
      return-list)))

(defun amc-find-out-matched-class-item-without-package-prefix
  (class-prefix &optional exactly_match)
  "actully you can use amc-find-out-matched-class-item to do
the same thing ,just let package-prefix nil but it is very slowly ,
it need to search all the line in tag file just to find out one class item .
so this function use amc-load-all-sorted-clas-items-to-memory
to sort the class section and load it in memory and build a index for it,
limit : length of class-prefix must larger than 2"
  (let ((amc-two-char-list-length (length amc-two-char-list))(two-char-item)
           (not_found t) (index 0) (matched-class-items))
    (setq case-fold-search nil)
    (if (string-match "^[A-Z][a-zA-Z]" class-prefix);; actually we only index this 
          (while  (and not_found (< index amc-two-char-list-length ))
               (setq two-char-item (nth index amc-two-char-list))
               (when (string-equal (substring-no-properties class-prefix 0 2) (car two-char-item))
                   (let* ((start (1- (nth 1 two-char-item)))
                          (end  (nth 2 two-char-item)) 
                           (i start) (current-class-item) (regexp-class-prefix))
                     (if exactly_match (setq regexp-class-prefix (concat "^" class-prefix "$"))
                       (setq regexp-class-prefix (concat "^" class-prefix )))
                     (while (< i end) (setq current-class-item (nth i amc-all-sorted-class-items))
                           (if (string-match  regexp-class-prefix (car  current-class-item ))
                               (add-to-list 'matched-class-items current-class-item t))
                     (setq i (1+ i))))
                   (setq  not_found nil);; exit while
               )
               (setq index (1+ index)))
      ;;actually  I only index  those class whose class name match [A-Z][a-zA-Z].
      ;; other class like   _RMIConnection_Stub should be search line by line at the class section  in tag file 
      (let ((line-num amc-class-first-ln) (current-line-string) (regexp-class-prefix))
        (if exactly_match (setq regexp-class-prefix  (concat "^" class-prefix "`" ))              
                          (setq regexp-class-prefix  (concat "^" class-prefix  )))
        (with-current-buffer (amc-reload-tag-buffer-maybe)
        (while (< line-num amc-member-first-ln)
          (setq current-line-string (amc-read-line line-num))
          (if (string-match regexp-class-prefix current-line-string)
              (add-to-list 'matched-class-items (amc-split-class-item current-line-string)))
          (setq line-num (+ line-num 1))))))
      (setq matched-class-items matched-class-items)))



(defun amc-sort-class ()
  "sort class for search ,we build a table for example ((Ab 1 3) (Ac 4 6))
then we search AbstractC ,we just need to search line number from 1 3 "
     (let ((begin ) (end) (amc-tmp-sorted-class-buffer "**amc-tmp-sorted-class**"))
  (with-current-buffer (amc-reload-tag-buffer-maybe)
       (setq case-fold-search nil)
    (amc-goto-line amc-class-first-ln)(beginning-of-line) (setq begin (point))
    (amc-goto-line amc-member-first-ln) (beginning-of-line) (setq end (point))
    (if (get-buffer amc-tmp-sorted-class-buffer) (kill-buffer (get-buffer amc-tmp-sorted-class-buffer )))
    (append-to-buffer amc-tmp-sorted-class-buffer begin end ))
    (with-current-buffer amc-tmp-sorted-class-buffer 
      (setq case-fold-search nil)
     (sort-lines nil 1 (point-max))
     (let ((end ?Z) (index ?A) (index2 ?A)  (two-char)
           (return-two-list)(two-char-item)(next-start-search-line-num))
       (while  (<= index end) (setq index2 ?A)
         (while ( <= index2 ?z)
          (setq two-char (concat (char-to-string index) (char-to-string index2)))
          (if next-start-search-line-num
              (setq two-char-item
                   (amc-build-map-4-search-class
                    two-char amc-tmp-sorted-class-buffer next-start-search-line-num))
              (setq two-char-item
                   (amc-build-map-4-search-class two-char amc-tmp-sorted-class-buffer 1)))
          (if two-char-item 
          (add-to-list 'return-two-list  two-char-item  t) 
          (setq next-start-search-line-num (nth 2 two-char-item)))
           (if (= index2 ?Z) (setq index2 ?a) (setq index2 (+ index2 1))))
         (setq index (+ index 1)))
        (setq amc-two-char-list return-two-list)))))

(defun amc-build-map-4-search-class (two-char-prefix amc-tmp-sorted-class-buffer  start-search-line-num)
  "suppose two-char-prefix is 'Ab' and amc-tmp-sorted-class-buffer is the buffer
 ,all lines in it is the classname has been sorted by classname 
it is cut from tag file between amc-class-first-ln and amc-member-first-ln ,and sorted by (sort-lines)
then this function is try to find out className begin with two-char-prefix ,and got the start line-number 
and end-line-number ,record in a list ,when search class name begin with two-char-prefix ,we just need to
find it from the start line number to the end line number ,it is faster than directly searching the unsorted 
tag buffer file "
  (with-current-buffer  amc-tmp-sorted-class-buffer
    (amc-goto-line start-search-line-num)
    (let ((char1 ) (char2)(end-prefix-regexp )(end-line-num)
          (start nil) (end nil) (has-found-first nil) (return-item))
      (setq case-fold-search nil)
      (setq char1 (string-to-char (substring-no-properties two-char-prefix 0 1)))
      (setq char2 (string-to-char (substring-no-properties two-char-prefix 1 2)))
      (if (or  (= char1 ?Z)  (= char2 ?z) (= char2 ?Z))
          (setq end-line-num (line-number-at-pos (point-max)))
        (progn 
        (if (< char2 ?a) 
            (setq end-prefix-regexp (concat  "^" (char-to-string char1)
                         "[a-z" (char-to-string (+ 1 char2)) "-Z]\\|^" (char-to-string (+ char1 1)) "[a-zA-Z]"  ))
          (setq end-prefix-regexp (concat "^" (char-to-string char1)
                     "[" (char-to-string (+ 1 char2)) "-z]\\|^" (char-to-string (+ char1 1)) "[a-zA-Z]"  ))) 
        (amc-goto-line start-search-line-num)
        (if ( re-search-forward end-prefix-regexp  (point-max) t) 
            (setq end-line-num (point))
            (setq end-line-num (point-max)))))
        (amc-goto-line start-search-line-num)
      (add-to-list 'return-item two-char-prefix)
         (while (re-search-forward (concat "^" two-char-prefix ) (point-max) t )
                  (when (not has-found-first)    
                    (setq has-found-first t)
                    (setq start (line-number-at-pos (point))))
                  (setq end (line-number-at-pos (point))))
         (if (numberp start )
           (progn (setq return-item (append return-item (list start)))
             (setq return-item (append return-item (list end ))))
           (setq return-item nil))
         (setq return-item return-item))))


(defun amc-load-all-sorted-class-items-to-memory()
  (amc-sort-class);;first sort the class ,and populate amc-two-char-list variable
  (with-current-buffer "**amc-tmp-sorted-class**"
    (goto-char (point-min))
    (let ((max-line-num (line-number-at-pos (point-max)))(line-num 1))
      (while  (< line-num max-line-num)
        (add-to-list 'amc-all-sorted-class-items
                     (amc-split-class-item (amc-read-line line-num)) t)
        (setq line-num (+ line-num 1)))))
  (kill-buffer (get-buffer "**amc-tmp-sorted-class**")))

(defun amc-import-package-candidates()
  "this function is the candidates , so you can bind it with a key sequence 
  it will return a list, for example `( java.lang ,java.ref)"
  (save-excursion 
    (let ((prefix-string) (matched-pkg-strings))
      (setq case-fold-search nil)
      (when
          (re-search-backward ;;search import string in java file or jsp file ,now support jsp 
           "\\(?:\\(?:import=\"\\(?:.*[ \t\n]*,[ \t\n]*\\)*\\)\\|\\(?:import[ \t]+\\)\\)\\([a-zA-Z0-9_\\.]*\\)"
           nil t)
        (setq prefix-string (match-string-no-properties 1 ))
        (when (and amc-matched-import-cache-list  ;;first try completion from cache 
                   (string-match (concat "^"  amc-previous-matched-import-prefix   ) prefix-string ))
          (setq matched-pkg-strings (all-completions prefix-string amc-matched-import-cache-list)))
        (when (= (length matched-pkg-strings ) 0 ) ;;if there are 0 matched in cache then find it out from tag files 
          (setq matched-pkg-strings ;;add pkgs 
                (append matched-pkg-strings
                        (amc-shrunk-matched-pkgs prefix-string  (amc-find-out-matched-pkg-item prefix-string))))
          (let ((index_of_last_dot (string-match "\\.[a-zA-Z_0-9]*$" prefix-string));;add classes
                 (package-prefix)(class-prefix))
            (when index_of_last_dot
              (setq package-prefix (substring-no-properties prefix-string 0 index_of_last_dot))
              (setq class-prefix (substring-no-properties prefix-string (+ 1 index_of_last_dot)))
              (dolist (element (amc-find-out-matched-class-item package-prefix class-prefix))
                (add-to-list 'matched-pkg-strings (concat package-prefix "." (car element)))))))
;;        (setq amc-is-importing-packages-p t)
        (setq amc-previous-matched-import-prefix prefix-string) ;;
        (setq  amc-matched-import-cache-list matched-pkg-strings)))))

(defun amc-find-out-class-by-parse-source ()
  "find out class in current  java source file, then will import  them if they haven't been imported   "
  (save-excursion 
    (save-match-data
      (let ((matched-class-strings)
            (return-type-regexp  "\\(\\([a-zA-Z0-9_]\\| *\t*< *\t*\\| *\t*>\\| *\t*, *\t*\\| *\t*\\[ *\t*]\\)+\\)" )
            (split-char-regexp "\\(,\\|<\\|>\\|]\\|\\[\\| \\|\t\\|\n\\)"));;a list of split char like ", \t<>[]"
       (goto-char (point-min))  (setq case-fold-search nil)
        (while (search-forward-regexp (concat "\\bnew[ \t]+" return-type-regexp)(point-max) 't)
          (setq matched-class-strings (append matched-class-strings  (split-string (match-string-no-properties 1 ) split-char-regexp t))))
       (goto-char (point-min))
        (while (search-forward-regexp "\\b\\([A-Z][a-zA-Z0-9_]*\\)\\.[a-zA-Z0-9_]+[ \t]*(" (point-max) 't)
          (setq matched-class-strings (append matched-class-strings  (list (match-string-no-properties 1)))))
        (goto-char (point-min))
        (while (search-forward-regexp "\\([a-zA-Z0-9_]+\\)\\.getInstance[ \t]*(" (point-max) 't)
          (add-to-list 'matched-class-strings (match-string-no-properties 1)))
       (goto-char (point-min))
        ;;find out all statement of variable ,for example
        ;; String name;      Map<String,<String,Ojbect>>[] map=
        (while (search-forward-regexp "^[ \t]*\\(public\\|private\\|static\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\| \\|\t\\)*\\([A-Z]\\([a-zA-Z0-9_]\\| *\t*< *\t*\\| *\t*>\\| *\t*, *\t*\\| *\t*\\[ *\t*]\\)+\\)[ \t]+[a-zA-Z0-9_]+[ \t]*[;=]"  (point-max) 't)
          (setq matched-class-strings
                (append matched-class-strings  (split-string (match-string-no-properties 2 ) split-char-regexp  t))))
       (goto-char (point-min));; find ClassName after "catch" keywords  for example :catch(IOException e )
        (while   (search-forward-regexp "catch[ \t]*(\\([a-zA-Z0-9_]+\\)[ \t]+"  (point-max) 't)
          (add-to-list 'matched-class-strings (match-string-no-properties 1)))
        (goto-char (point-min)) ;;find method statement
        (while   (search-forward-regexp "^[ \t]*\\(public\\|private\\|static\\|final\\|native\\|synchronized\\|transient\\|volatile\\|strictfp\\| \\|\t\\)*[ \t]+\\(\\([a-zA-Z0-9_]\\|\\( *\t*< *\t*\\)\\|\\( *\t*> *\t*\\)\\|\\( *\t*, *\t*\\)\\|\\( *\t*\\[ *\t*\\)\\|\\(]\\)\\)+\\)[ \t]+[a-zA-Z0-9_]+[ \t]*(\\(.*\\))[ \t]*\\(throws[ \t]+\\([a-zA-Z0-9_, \t\n]*\\)\\)?[ \t\n]*{"  (point-max) 't)
          (let ((exception (match-string-no-properties 11))
                (returns (match-string-no-properties 2))
                (params (match-string-no-properties 9)))
            ;;handle return type
            (setq matched-class-strings (append matched-class-strings  (split-string  returns "\\(,\\|<\\|>\\|]\\|\\[\\| \\|\t\\)"  t)))
;;;;handle methods parameters  ;;find out 'Map String Ojbect User' from "Map<String,Object> map,User user"
            (while (and params (> (length params) 0))
              (if (string-match "\\([a-zA-Z0-9_]\\|\\( *\t*< *\t*\\)\\|\\( *\t*>\\)\\|\\( *\t*, *\t*\\)\\|\\( *\t*\\[ *\t*\\)\\|\\(]\\)\\)+" params)
                  (progn (setq matched-class-strings
                          (append matched-class-strings (split-string (match-string-no-properties 0 params ) split-char-regexp t)))
                    (string-match "[ \t]*[a-zA-Z0-9_]+[ \t,]?" params  (match-end 0 ))
                    (setq params (substring-no-properties params  (match-end 0 ))))
                (setq params nil)))
            ;;handle throws Exception1,Exception2, we will exatract Exception1 Exception2 from throws sentence
       (when exception 
            (setq matched-class-strings (append matched-class-strings (split-string  exception split-char-regexp t))))))
         ;;remove primitive type and remove duplicate item
        (delete-dups matched-class-strings) (delete "" matched-class-strings)
        (dolist (ele matched-class-strings)
          (if (string-match  "\\(int\\|float\\|double\\|long\\|short\\|char\\|byte\\|void\\|boolean\\|return\\|public\\|static\\|private\\|protected\\|abstract\\|final\\|native\\|package\\|new\\)" ele)
              (delete ele matched-class-strings)))
       matched-class-strings 
        ))))

(defun amc-caculate-all-unimported-class-items()
  "this function will find out all unimported Class itmes , it just do a subtration 
   (amc-find-out-class-by-parse-source) -(amc-caculate-all-imported-class-items) 
what you need to do next, is just import the unimported class  "
  (let ((imported-class-names (mapcar 'car (amc-caculate-all-imported-class-items)))
        (class-names-in-source (amc-find-out-class-by-parse-source))  
        (unimported-class-items))
    (print class-names-in-source)
    (dolist (ele class-names-in-source)
      (unless (member ele imported-class-names)
        (setq unimported-class-items
              (append unimported-class-items
                      (amc-find-out-matched-class-item-without-package-prefix ele t)))))
    unimported-class-items))

(defun amc-import-all-unimported-class()
  "import all unimported class ."
  (interactive)
    (amc-insert-import-at-head-of-source-file
     (amc-caculate-all-unimported-class-items)))

(defun amc-import-class-under-point ()
  "import class under point."
  (interactive)
  (let ((cur-word (current-word)))
    (when (and cur-word  (> (length cur-word) 0))
      (when (string-match "[^a-zA-Z0-9_]\\([a-zA-Z0-9_]+\\)$" cur-word)
        (setq cur-word (match-string-no-properties 1  cur-word )))
      (when (string-match "^\\([a-zA-Z0-9_]+\\)[^a-zA-Z0-9_]" cur-word)
        (setq cur-word (match-string-no-properties 1  cur-word)))
    (amc-insert-import-at-head-of-source-file
     (amc-find-out-matched-class-item-without-package-prefix cur-word t)))))

(defun amc-insert-import-at-head-of-source-file (import-class-items-list)
  "insert 'import sentence' at head of java source file,
before that it will use y-or-n-p ask user to confirm "
  (let ((import-class-buffer "*amc-import-java-class*")
        (import-class-window) (user-confirmed-class-items-list)
        (java-buffer (current-buffer))(java-window))
    (setq case-fold-search nil)
    (if (and import-class-items-list (> (length import-class-items-list) 0))
        (progn 
          (setq import-class-buffer (switch-to-buffer-other-window  import-class-buffer t))
          (setq java-window (get-buffer-window java-buffer))
          (setq import-class-window (get-buffer-window import-class-buffer))
          (with-current-buffer    import-class-buffer  ;;show maybe imported Class in a new buffer
            (delete-region (point-min)(point-max))
            (dolist (ele import-class-items-list)
              (insert (concat "[ ]  "  (car (amc-split-pkg-item-by-pkg-ln  (nth 1 ele ))) "." (car ele)  "\n")))
            (insert "  ");;insert empty line at end of buffer
            (goto-char (1+(point-min)))
            (dolist (ele import-class-items-list ) ;;ask user whether to import the Class
              (beginning-of-line)(forward-char 1)
              (when (y-or-n-p (concat "import " (car ele)  "? "))
                (add-to-list 'user-confirmed-class-items-list ele)
                (delete-char 1) (insert "*"))
              (forward-line 1)(forward-char 1)))
          ;;delete *import-java-class* buffer and window
          (delete-window import-class-window)(kill-buffer import-class-buffer)
          (with-current-buffer java-buffer
            (amc-insert-import-at-head-of-source-file-without-confirm user-confirmed-class-items-list))
          (message "Finished importing.")
          user-confirmed-class-items-list)
      (message "No class need import."))))


(defun amc-insert-import-at-head-of-source-file-without-confirm (class-items)
(save-match-data  ;;insert  at head of java source
      (setq case-fold-search nil)
  (save-excursion   (goto-char (point-min))
    (let* ((class-start (save-excursion
                (re-search-forward
                 "\\(\\b\\(class\\|interface\\)[ \t]+[a-zA-Z0-9_]+[ \t\n]*\\({\\|extends\\|implements\\)\\)"  nil 't))))
      ;; (if (not class-start);; then this is a jsp file
      ;;     (let ((all-class-strings ""))
      ;;       (dolist (class-item class-items)
      ;;         (setq all-class-strings
      ;;               (concat all-class-strings
      ;;                       (car (amc-split-pkg-item-by-pkg-ln  (nth 1 class-item ))) "." (car class-item)
      ;;                       ",")))
      ;;       (unless (string-equal "" all-class-strings);;delete last char ","
      ;;           (setq all-class-strings (substring all-class-strings 0 (1- (string-width all-class-strings)))))
      ;;       (goto-char (point-min))
      ;;       (insert (concat "<%@ page import=\"" all-class-strings "\" %>\n"  )))
        (if(re-search-forward "^[ \t]*import[ \t]+[a-zA-Z0-9_\\.\\*]+[ \t]*;" class-start 't) 
          ;;if find 'import' insert before it 
          (progn (beginning-of-line )(insert "\n")(forward-line -1)
             (dolist (ele class-items)(insert 
                                      (concat "import " 
                                             (car (amc-split-pkg-item-by-pkg-ln  (nth 1 ele )))
                                             "." (car  ele) ";\n"))))
        ;; if hasn't found 'import; then insert after 'package ' statement 
        (progn (goto-char (point-min))
               (if (re-search-forward "^[ \t]*package[ \t]+[a-z0-9_\\.]+[ \t]*;" class-start 't)
                   (progn (forward-line 1) (beginning-of-line)(newline)
                    (dolist (ele class-items)
                            (insert (concat "import "
                                             (car (amc-split-pkg-item-by-pkg-ln  (nth 1 ele )))
                                             "." (car  ele) ";\n"))))
                 (progn ;;if hasn't found 'import' and 'package' then insert at head of buffer
                   (goto-char (point-min))
                (dolist (ele class-items)
                         (insert (concat "import "
                                           (car (amc-split-pkg-item-by-pkg-ln  (nth 1 ele )))
                                            "." (car  ele) ";\n"))))))
           )))))

(defun amc-find-out-import-line ()
 "make a regex to match the packages in the import statements ,
return a list of each line string (exclude keyword 'import') "
  (let ((imported-lines))
    (save-match-data (save-excursion
        (goto-char (point-min))
        (setq case-fold-search nil)
        (let ((class-start (save-excursion
           (re-search-forward
            "\\(\\b\\(class\\|interface\\)[ \t]+[a-zA-Z0-9_]+[\n \t]*\\({\\|extends\\|implements\\)\\)" nil 't))))
          (if class-start ;;if found class or interface key words ,then this is a java file  ,if not  it is a jsp file 
              (while (re-search-forward "^[ \t]*import[ \t]+\\([a-zA-Z0-9_\\.\\*]+\\)[ \t]*;" class-start 't)
                (add-to-list 'imported-lines  (match-string-no-properties 1))
                (end-of-line))
            ;;may be this is a jsp file
            (while (re-search-forward "\\bimport=\"\\(.*?\\)[ \t]*\"[ \t]+"  (point-max) 't)
              (setq imported-lines (append imported-lines  (split-string (match-string-no-properties 1) "[ \t,]" t)))
              (end-of-line))))))
    imported-lines ))

(defun amc-caculate-all-imported-class-items (&optional exclude_java_lang)
  "find out all imported class  ,default include class in java.lang.*"
  (let ((imported-line (amc-find-out-import-line))(element)(index)  (return-class-items))
    (setq case-fold-search nil)
    (dolist ( element imported-line )
      (setq index   (string-match "\\.\\*$"  element))
      (if index   ;;import a package 
         (setq return-class-items (append return-class-items 
                     (amc-find-out-matched-class-item (substring-no-properties element 0 index) nil))) 
        (progn  ;;import a class 
          (string-match "^\\(.+\\)\\.\\([a-zA-Z0-9_]+\\)$" element)
          (setq return-class-items (append return-class-items  
                (amc-find-out-matched-class-item
                      (match-string-no-properties 1 element ) (match-string-no-properties 2 element )  t )))))) 
    (if exclude_java_lang 
        (setq return-class-items return-class-items )
      (setq return-class-items  (append return-class-items  (amc-find-out-matched-class-item "java.lang" nil )))
      )))
(defun amc-complete-constructor-candidates ()
  (let (andidates class-items);;if find keyword:new ,then do constructor complete ,if not do class complete
    (setq case-fold-search nil)
    (when (looking-back "\\bnew[ \t]+\\([A-Z][a-zA-Z0-9_]*\\)[ \t]*(?[ \t]*"  (line-beginning-position))
      (setq class-items (amc-complete-class-with-cache (match-string-no-properties 1)))
      (dolist (class-item class-items)
        (setq candidates (append candidates (amc-complete-constructor (car class-item))))
        ))
    candidates
    ))

(defun amc-complete-constructor (class-prefix)
  (let ((matched-class-items (amc-find-out-matched-class-item-without-package-prefix class-prefix t))
        (matched-constructor-items) (return-complete-list)) 
    ;;when matched class > 1 ,then ask user to import one of them ,
    ;;then we can got the imported class item , we complete its constructor
    (dolist (matched-class-item matched-class-items)
      (let ((line-num     (nth 2  matched-class-item))  (matching-constructor t)  
            (end-line-num (nth 3  matched-class-item)) (current-line))
        (while (and matching-constructor  (< line-num end-line-num ))
          (setq current-line (amc-read-line line-num (amc-reload-tag-buffer-maybe)))
          (if (string-match "^  "  current-line)   
              (add-to-list 'matched-constructor-items (amc-split-constructor current-line))
            (setq matching-constructor nil))
          (setq line-num (+ line-num 1)))
        (dolist (constructor matched-constructor-items)
          (let ((constructor-full-string (amc-constructor-to-string constructor t))
                (constructor-short-string (amc-constructor-to-string constructor nil)))
            (add-to-list 'return-complete-list  constructor-short-string t)
            (setplist 'props nil )
            (put 'props 'view constructor-full-string)
            (put 'props 'templete (amc-constructor-to-yasnippet-templete constructor))
            (add-text-properties 0 (length constructor-short-string)
                                 (symbol-plist  'props)  constructor-short-string)
            ))))
    return-complete-list))

(defun amc-is-available-4-complete-class-p ()
  "only when this function return t ,then amc-complete-class-candidates
    will try to find out candidates  "
  (let ((class-prefix (current-word)) (is-available))
    (setq case-fold-search nil)
        (when  (and class-prefix  (> (length class-prefix ) 0) (string-match "^[A-Z][a-zA-Z0-9_]*$" class-prefix))
           (setq amc-current-class-prefix-4-complete-class class-prefix) 
           (setq is-available t)) 
    is-available
    ))


(defun amc-complete-class-candidates () 
  "complete class name with (current-word) as class-prefix"
  (when (amc-is-available-4-complete-class-p)
    (let ((candidate)(candidates)
          (class-items (amc-complete-class-with-cache amc-current-class-prefix-4-complete-class)))
      (dolist (class-item class-items)
        (setq candidate  (car class-item))
        (setplist 'props nil )
        (put 'props 'view (amc-class-to-string class-item t))
        (add-text-properties 0 (length candidate) (symbol-plist  'props)  candidate)
        (add-to-list 'candidates candidate t)
     ) candidates
      )
    ))

(defun amc-complete-class-with-cache ( class-prefix )
  "find out class name starts with class-prefix ,before search tag file ,it first 
check out amc-matched-class-items-cache to find out if ant matched class exists "
  (let ((return-list))
    (setq case-fold-search nil)
    (when  (and class-prefix   (string-match "[A-Z][a-zA-Z0-9_]*" class-prefix))
      (if (and amc-previous-class-prefix   (string-match (concat "^" amc-previous-class-prefix ) class-prefix ))
          (dolist (class-item amc-matched-class-items-cache ) 
            (if (string-match (concat "^" class-prefix) (car class-item)) (add-to-list 'return-list class-item t))) 
        (setq return-list  (amc-find-out-matched-class-item-without-package-prefix class-prefix)))
      (when (> (length return-list) 0);; if find matched ,update cache ,or not
        (setq amc-previous-class-prefix class-prefix)
        (setq amc-matched-class-items-cache return-list )))
    return-list;; return 
))

(defun amc-build-list-with-nth-on-each-element (list index  )
  "if params : list= '((1 11 111) (2 22 222)) index=1 then return '(11 22 ) "
  (let ((return-list))
    (dolist (ele list) 
      (add-to-list 'return-list (nth index ele) t)) 
     return-list;;return
 ))

(defun amc-find-members (class-item  &optional member-prefix &optional exactly_match)
  "find members(field method) under class-item which member name match member-prefix ,
if member-prefix is nil or empty string it will return all members under class-item"
  (let ((line-num (nth 2 class-item))  (end-line-num (nth 3 class-item)) (return-member-items)
       (regexp-method-prefix)(regexp-field-prefix) (current-line-string )) 
       (if exactly_match  
            (setq regexp-method-prefix (concat "^" member-prefix "`") 
                  regexp-field-prefix (concat "^ " member-prefix "`"))  
         (if (or (not member-prefix)  (string-equal "" member-prefix))
            (setq regexp-method-prefix "^[a-zA-Z0-9_]" regexp-field-prefix "^ [^ ]" ) 
            (setq regexp-method-prefix (concat "^" member-prefix ) 
                  regexp-field-prefix (concat "^ " member-prefix ))))
       (with-current-buffer (amc-reload-tag-buffer-maybe)
         (while (< line-num  end-line-num)
           (setq current-line-string (amc-read-line line-num))
           (if (string-match regexp-method-prefix current-line-string)
               (add-to-list 'return-member-items (amc-split-method current-line-string ) t)
             (when (string-match regexp-field-prefix current-line-string) 
                 (add-to-list 'return-member-items (amc-split-field current-line-string ) t)))
           (setq line-num (+ line-num 1))))
        return-member-items))

(defun amc-caculate-class-name-by-variable(variable-name)
  "this function is used to find Class name depend on a varibale name ,for example
 the varibale-name is str ,then if exists 'String str' in source file , String will be returned "
  (let ((matched-class-name) (variable-line-string) (index-of-var-in-line) (var-stack))
    (setq case-fold-search nil)
    (save-excursion 
      (if (search-backward-regexp  (concat "\\b\\([a-zA-Z0-9_]\\| *\t*< *\t*\\| *\t*>\\| *\t*, *\t*\\)*[ \t]+"   variable-name "\\b")   (point-min) t )
          (setq variable-line-string (amc-read-line  ( line-number-at-pos (point)))) 
        (when (search-forward-regexp
               (concat "\\b\\([a-zA-Z0-9_]\\| *\t*< *\t*\\| *\t*>\\| *\t*, *\t*\\)*[ \t]+"   variable-name "\\b") 
               (point-max) t)
          (setq variable-line-string (amc-read-line  ( line-number-at-pos (point))))
    )))
    (when variable-line-string
      (setq index-of-var-in-line  (string-match  (concat "[ \t]+" variable-name "\\b")  variable-line-string))
      (setq variable-line-string (substring-no-properties  variable-line-string 0  index-of-var-in-line   ))
      (setq var-stack (split-string variable-line-string "[( \t]" t))
      (let ((tmp-list))
        (dolist (ele var-stack)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele "<"  "<"  t))))
        (setq var-stack tmp-list))
      (let ((tmp-list))
        (dolist (ele var-stack)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele ">"  ">"  t))))
        (setq var-stack tmp-list))
      (setq var-stack (nreverse var-stack ))
      (let ((top (pop var-stack)) (parse-finished ))
        (while  (and top (not parse-finished))
          (when (string-match "[A-Z][a-zA-Z0-9_]*" top ) 
            (setq matched-class-name top)   (setq parse-finished t));; parse finished ,exit the  loop
          (when (string-equal ">" top) 
            (let ((e)(right-stack)) 
              (push top  right-stack)
              (setq e (pop var-stack))
              (while (and e (  > (length right-stack) 0))
                (if (string-equal "<" e ) (pop right-stack))
                (if (string-equal ">" e ) (push e right-stack))
                (setq e (pop var-stack))) 
              (if e (push e var-stack)))) 
          (setq top (pop var-stack))))
      ) matched-class-name))

(defun amc-complete-method-candidates ()
  (let((stack-list  (amc-parse-splited-line-4-complete-method)) (is-dot-last)
      (top) (return-list)(return-string-list nil))
    ( if (and stack-list (> (length stack-list) 0))
        (when (amc-validate-splited-line-items-4-method-complete  stack-list)
          (if (= (% (length stack-list ) 2 ) 0) (setq is-dot-last t))
          (setq stack-list (remove "." stack-list ))
          (setq  top (pop stack-list))
          (let ((class-item ))
          (if (string-match "^[A-Z][a-zA-Z0-9_]*$" top)
            (setq class-item (amc-find-class-first-check-imported  top))
            (setq class-item (amc-find-class-first-check-imported (amc-caculate-class-name-by-variable top))))
                (while (> (length stack-list ) 1) 
                  (setq class-item  (nth 1 (car  (amc-find-members class-item (pop stack-list) t))))
                  )
                 (if is-dot-last (let ((member-string (pop stack-list)))
                                     (if member-string  
                                       (setq class-item  (nth 1 (car  (amc-find-members class-item member-string t)))))
                                       (setq return-list (amc-find-members class-item   )))
                  (setq return-list (amc-find-members class-item   (pop stack-list)))))
 ))
    (dolist (member return-list);; translate item to string
      (if (= 2   (length member ));; lenth of field is 2 (only field and returntype )
          (let ((field-full-string (amc-field-to-string member t))
                (field-short-string (amc-field-to-string member nil)))
            (add-to-list 'return-string-list     field-short-string t)
            (setplist 'props nil ) (put 'props 'view field-full-string)
             (add-text-properties 0 (length field-short-string)
                                 (symbol-plist  'props)  field-short-string)
            )
        (let((method-full-string  (amc-method-to-string member t))
              (method-short-string (amc-method-to-string member nil)))
          (add-to-list 'return-string-list   method-short-string t)
            (setplist 'props nil ) (put 'props 'view method-full-string)
            (put 'props 'templete (amc-method-to-yasnippet-templete member))
             (add-text-properties 0 (length method-short-string)
                                 (symbol-plist  'props)  method-short-string)
          
          )
        ))  
    return-string-list
    ))

(defun amc-validate-splited-line-items-4-method-complete (stack-list)
"('System' '.') return true, ('System' '.' 'ou' ) return true,
 ('System' '.' 'out' '.' 'pri') return  true"
  (let ((current-item (car stack-list)) (validate t) (index 1)(next-item))
    (if (and (> (length stack-list) 1)  (string-match "^[a-zA-Z0-9_]+$" current-item ))
        (while (and validate current-item  )
          (setq next-item (nth index stack-list))
          (if (string-match "^[a-zA-Z0-9_]+$" current-item )
              (if next-item (if (not (string-equal "." next-item)) (setq validate nil)   )))
          (if (string-equal "." current-item )
              (if next-item (if (not (string-match "^[a-zA-Z0-9_]+$" next-item)) (setq validate nil)   )))
          (setq current-item next-item)
          (setq index (+ 1 index))
          )    
      (setq validate nil))
    (setq validate validate)))

(defun amc-parse-splited-line-4-complete-method ()
  " parse current line  for complete method  ,suppose current line is
System.getProperty(str.substring(3)).to  
first amc-split-line-4-complete-method will split this line to 
'System' '.' 'getProperty' '(' 'str' '.' 'substring' '(' '3' ')' ')' '.' 'to'
amc-remove-unnecessary-items-4-complete-method will remove anything between ( and )  ,so only
'System'  '.' 'getProperty'  '.'  'to'  is left "  
  (let* ((line-string (buffer-substring-no-properties (line-beginning-position) (point)))
          (splited-line-items (amc-split-line-4-complete-method line-string)))
    (amc-remove-unnecessary-items-4-complete-method splited-line-items )))

(defun amc-remove-unnecessary-items-4-complete-method (splited-line-items) 
" System.getProperty(str.substring(3)).to  
first amc-split-line-4-complete-method will split this line to 
'System' '.' 'getProperty' '(' 'str' '.' 'substring' '(' '3' ')' ')' '.' 'to'
this function will remove anything between ( and )  ,so only
'System'  '.' 'getProperty'  '.'  'to'  is left "
  (let* (  (stack-list)(ele) (reverse-current-line-split-list  (reverse splited-line-items)) (parse-finished))
    (setq ele (pop reverse-current-line-split-list))
    (while  (and ele (not parse-finished))
      (if  (or (string-equal ";" ele) (string-equal "(" ele )) (setq parse-finished t);; parse finished ,exit the  loop
          (if (string-equal ")" ele) 
              (let ((e)(right-stack)) 
                (push ele  right-stack)
                (setq e (pop reverse-current-line-split-list))
                (while (and e (  > (length right-stack) 0))
                  (if (string-equal "(" e ) (pop right-stack))
                  (if (string-equal ")" e ) (push e right-stack))
                  (setq e (pop reverse-current-line-split-list))) 
                (if e    (push e reverse-current-line-split-list)))
              (push ele stack-list) 
            ))
      (setq ele (pop reverse-current-line-split-list)))
    (setq stack-list stack-list)
      ))
;; (defun amc-replace-keyword-with-its-class-name()
;;   (save-excursion 
;;     (let ((class-name)))
;;     (setq case-fold-search nil)
;;     (if (search-backward-regexp  "\\bclass[ \t]+\\([A-Z][a-zA-Z0-9_]*\\)\\b"   (point-min) t )
;;         (setq class-name (match-string-no-properties 1 ))
;;       ))
;;   )
(defun amc-split-line-4-complete-method(line-string  )
  "this function is used to complete method ,first this function will split line-string to small items 
for example : suppose line-string is 
System.getProperty(str.substring(3)).to  
then this function split it to
'System' '.' 'getProperty' '(' 'str' '.' 'substring' '(' '3' ')' ')' '.' 'to' "
  (save-excursion 
    (let* (  (stack-list nil)) 
      (setq case-fold-search nil)
        (setq line-string  (replace-regexp-in-string   "\\\\\"" "'"       line-string)) 
        (setq line-string  (replace-regexp-in-string   "\".*?\"" "String" line-string)) 
        (setq line-string  (replace-regexp-in-string   "\\bnew\\b"    ""  line-string)) 
        (setq line-string  (replace-regexp-in-string   "\\breturn\\b" ""  line-string)) 
        (setq line-string  (replace-regexp-in-string   "\\this\\b" ""  line-string)) 
        (while (string-match "=\\(.*\\)" line-string)
          (setq line-string (match-string-no-properties 1 line-string)))
       ;;split line-string with "." ,but add "." as an element at its position in list
      (setq stack-list (amc-split-string-with-separator  line-string "[ \t]*\\.[ \t]*"  "." t))
       ;;split each element  with "(" ,but add "(" as an element at its position in list 
      ;;and merge all the list in a list 
      (let ((ele)(tmp-list))
           (dolist (ele stack-list)
            (setq tmp-list (append tmp-list (amc-split-string-with-separator ele "("  "("  t))))
           (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele ")"  ")"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele "\\["  "("  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele "]"  ")"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele "{"  "("  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele "}"  ")"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele "<"  "("  t))))
        (setq stack-list tmp-list))
      (let((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele ">"  ")"  t))))
        (setq stack-list tmp-list))
            (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele ","  ";"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (amc-split-string-with-separator ele ";"  ";"  t))))
        (setq stack-list tmp-list))
      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (split-string ele "[ \t]+"  t))))
        (setq stack-list tmp-list))
      (setq stack-list stack-list )
      ))
  )

(defun amc-mirah-keywords-candidates ()
  (let ((keywords))
            (setq keywords (list "public" "protected"  "private" "native" "final" "synchronized" "transient" "abstract"  "static" "import" "this" "if" "else" "else if" "break" "case" "switch"  "continue" "class" "interface" "package" "new" "try" "catch" "finally" "super" "void"  "int" "float" "double" "short" "char" "byte" "long" "boolean" "enum" "intanceof"  "for" "while" "throw" "throws"  "extends" "implements" ))
    ))
(provide 'mirah-complete)

;; End.