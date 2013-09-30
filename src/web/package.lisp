;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition file for Ovorost web package.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(defpackage :ovorost-web
  (:use :cl
        :ovorost-ajax
        :ovorost-tools
        :ovorost-config
        :ovorost-dao
        :ovorost-services)
  (:documentation "Ovorost Web site.")
  (:export

   ;; Factory

   #:make-ovorost
   #:ovorost
   #:config-of
   #:backend-of
   #:server-of
   
   ;; Ovorost Management

   #:setup
   #:start
   #:stop
   #:update

   #:reload-i18n
   #:reload-resorts


   ;; Conditions

   #:web-server-error
   
   ))

