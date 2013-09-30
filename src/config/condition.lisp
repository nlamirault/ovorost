;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          errors.lisp
;;;; Purpose:       Configuration errors of ovorost
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************



(in-package :ovorost-config)


(define-condition configuration-error (ovorost-error)
  ()
  (:documentation "Configuration problem.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Problem with configuration."))))

