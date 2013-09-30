;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          config.lisp
;;;; Purpose:       Unit Test suite for Ovorost configuration.
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


(lift:deftestsuite ovorost-config-test (ovorost-test)
  ()
  (:documentation "Unit Test suite for Ovorost configuration."))



(lift:addtest (ovorost-config-test)
  correct-config
  (:documentation "Test a correct configuration file.")
  (when cl-user::*config*
    (setf cl-user::*config* nil))
  (let ((config (load-configuration (concatenate 'string
                                                 *ovorost-source*
                                                 +ut-configuration-ok+))))
    (lift:ensure (eq (type-of config) 'configuration))))


(lift:addtest (ovorost-config-test)
  incorrect-database-config
  (:documentation "Test an incorrect database configuration.")
  (when cl-user::*config*
    (setf cl-user::*config* nil))
  (lift:ensure-condition 'configuration-error
    (load-configuration (concatenate 'string
                                     *ovorost-source*
                                     +ut-configuration-db-ko+))))


(lift:addtest (ovorost-config-test)
  incorrect-web-config
  (:documentation "Test an incorrect web configuration.")
  (when cl-user::*config*
    (setf cl-user::*config* nil))
  (lift:ensure-condition 'configuration-error
    (load-configuration (concatenate 'string
                                     *ovorost-source*
                                     +ut-configuration-web-ko+))))



(lift:addtest (ovorost-config-test)
  condition-instance
  (:documentation "Creates a 'configuration-error instance")
  (let ((condition (make-instance 'configuration-error)))
    (lift:ensure (eq (type-of condition)
                     'configuration-error))))