;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          depends.lisp
;;;; Purpose:       Install all dependencies of Ovorost.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of Ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; Ovorost users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(require 'asdf)
(require 'asdf-install)

(push (merge-pathnames "src/ovorost/" (user-homedir-pathname))
      asdf:*central-registry*)

(defparameter *ovorost-dependencies*
  `((,(merge-pathnames "src/ovorost/depends/site/" (user-homedir-pathname))
     ,(merge-pathnames "src/ovorost/depends/systems/" (user-homedir-pathname))
     "OVOROST installation"))
  "OVOROST locations.")

(push (cadar *ovorost-dependencies*) asdf:*central-registry*)

(unless (find (caar *ovorost-dependencies*) asdf-install:*locations* :key #'car)
  (format t "~&LOCATION !!!")
  (push (car *ovorost-dependencies*) asdf-install:*locations*))


(loop for dep in '(:hunchentoot
                   :html-template
                   :cl-smtp
                   ;;:cl-i18n ;; patchee le 28/02/2008
                   :drakma
                   :cl-ppcre
                   :clsql
                   :cl-json
                   :closure-html
                   :cxml-stp)
      do (with-input-from-string (*standard-input* "1")
           (asdf-install:install dep)))
