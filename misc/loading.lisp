;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          loading.lisp
;;;; Purpose:       Load Ovorost..
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

(defparameter *ovorost-dependencies*
  `((,(merge-pathnames "src/ovorost/depends/site/" (user-homedir-pathname))
     ,(merge-pathnames "src/ovorost/depends/systems/" (user-homedir-pathname))
     "OVOROST installation"))
  "OVOROST locations.")

(setf asdf:*central-registry*
      (list (cadar *ovorost-dependencies*)))

(push (merge-pathnames "src/ovorost/" (user-homedir-pathname))
      asdf:*central-registry*)
            
;; asdf:*central-registry*)

(asdf:oos 'asdf:load-op :ovorost)


