;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          
;;;; Purpose:       
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of , is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;;  users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************



(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))
(asdf:oos 'asdf:load-op :ovorost-test)
(ovorost-test:run-ovorost-test)
(sb-cover:report "/home/nicolas/public_html/report/")
(declaim (optimize (sb-cover:store-coverage-data 0)))


