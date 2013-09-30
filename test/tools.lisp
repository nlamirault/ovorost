;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tools.lisp
;;;; Purpose:       Some tools for Ovorost Unit Tests.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-test)



(defun test-numbers-of-tokens (sequence token number)
  "Test how TOKEN is find in SEQUENCE. T if it is NUMBER."
  (let ((sum 0))
    (cl-ppcre:do-matches (s e token sequence)
      (incf sum))
    (lift:ensure (= sum number))))



