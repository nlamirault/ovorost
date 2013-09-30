;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Unit Test Ovorost package.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ovorost users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :ovorost-test
  (:use :common-lisp
        :ovorost-config
        :ovorost-tools
        :ovorost-dao
        :ovorost-web)
  (:export #:run-ovorost-test))

