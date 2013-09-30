;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition file for Ovorost configuration package
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(defpackage :ovorost-config
  (:use :cl :ovorost-tools)
  (:documentation "Ovorost configuration package.")
  (:export
   
   ;; Configuration system
   
   #:configuration
   #:configuration-environment
   #:configuration-port
   #:configuration-key
   #:configuration-sql
   #:configuration-mod-lisp-p
   #:configuration-captcha

   ;; Tools

   #:load-configuration
   
   ;; Conditions

   #:configuration-error

   ))

