;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          condition.lisp
;;;; Purpose:       Web Ovorost conditions.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)



(define-condition web-server-error (ovorost-error)
  ((message :initarg :message
            :accessor message-of))
  (:documentation "An error of type WEB-SERVER-ERROR is signaled if the web site of Ovorost
can't be started.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Can't start Ovorost: ~A." (message-of condition)))))


