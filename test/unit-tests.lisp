;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          unit-tests.lisp
;;;; Purpose:       Unit Test suite for Ovorost.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-test)


(defun run-ovorost-test ()
  "Run the Ovorost unit tests.
CREATE-DATABASE-P if is T, create the SQL schema before running unit tests."
  (let ((config-file (concatenate 'string
                                  *ovorost-path*
                                  "etc/lift-standard.config")))
    (lift:run-tests :config config-file)))

