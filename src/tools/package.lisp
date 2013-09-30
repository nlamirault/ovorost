;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition file for Ovorost tools
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(defpackage :ovorost-tools
  (:use :cl)
  (:documentation "Some Ovorost tools.")
  (:export #:directory-p
           #:list-directory
           #:get-ovorost-directory
           #:trim
           #:print-date
           #:iso-time

           #:ovorost-error
           
           ))

