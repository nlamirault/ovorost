;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          debug.lisp
;;;; Purpose:       Tools for debuging Ovorost.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)


(defun debug-mode (active-p)
  "Active or not the debug mode."
  (setf hunchentoot:*show-access-log-messages* active-p
        hunchentoot:*show-lisp-backtraces-p* active-p
        hunchentoot:*show-lisp-errors-p* active-p
        hunchentoot:*log-lisp-backtraces-p* active-p
        hunchentoot:*log-lisp-errors-p* active-p
        hunchentoot:*log-lisp-warnings-p* active-p))




