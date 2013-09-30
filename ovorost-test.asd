;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ovorost.asd
;;;; Purpose:       ASDF definition for Ovorost unit tests system.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; *************************************************************************



(in-package :asdf)


(defsystem ovorost-test
    :name "ovorost"
    :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :version "0.4-pre0"
    :licence "(c) Perave"
    :description "Unit Tests for Ovorost."
    :depends-on (:ovorost :lift :selenium)
    :components
    ((:module :test
              :components
              ((:file "package")
               (:file "specials" :depends-on ("package"))
               (:file "definitions" :depends-on ("package"))
               (:file "tools" :depends-on ("package"))
               (:file "config" :depends-on ("definitions" "specials"))
               (:file "dao" :depends-on ("definitions" "specials"))
               (:file "web" :depends-on ("definitions" "specials"))
               (:file "api" :depends-on ("definitions" "specials" "tools"))
               (:file "ajax" :depends-on ("definitions" "specials" "tools"))
               (:file "unit-tests" :depends-on ("config" "dao" "api" "ajax"))))))



