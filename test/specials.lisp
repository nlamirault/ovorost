;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       ovorost-test specials informations.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ovorost users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :ovorost-test)


(defparameter *ovorost-source*
  (namestring (asdf:component-relative-pathname (asdf:find-system :ovorost)))
  "Directory with contains OVOROST source files.")


(defparameter *ovorost-path*
  (namestring
   (asdf:component-relative-pathname (asdf:find-system :ovorost)))
  "Directory with contains Ovorost source files.")


(unless (fboundp '+ut-configuration-ok+)
  (defconstant +ut-configuration-ok+ "etc/environments/ut/ovorost.conf"
    "A valid configuration file."))


(unless (fboundp '+ut-configuration-web-ko+)
  (defconstant +ut-configuration-web-ko+ "etc/environments/ut/ovorost.conf-web-ko"
    "A invalid web configuration file."))


(unless (fboundp '+ut-configuration-db-ko+)
  (defconstant +ut-configuration-db-ko+ "etc/environments/ut/ovorost.conf-db-ko"
    "An invalid database configuration file."))



