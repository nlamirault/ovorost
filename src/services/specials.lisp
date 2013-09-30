;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       Some specials variables.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007 by Nicolas Lamirault
;;;;
;;;; ovorost users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :ovorost-services)



(defparameter *print-ovorost-services* nil
  "If T, modify the printed representation of Ovorost services objects.")


(defparameter *debug* nil "If T, active some debut logs.")


(unless (boundp '+SKIINFO-COUNTRIES+)
  (defconstant +SKIINFO-COUNTRIES+
    (list (cons "AT" "austria")
          (cons "FR" "france")
          (cons "DE" "germany")
          (cons "IT" "italy")
          (cons "NO" "norway")
          (cons "SE" "sweden")
          (cons "CH" "switzerland"))))


(unless (boundp '+SKIINFO+)
  (defconstant +SKIINFO+ "http://www.skiinfo.com/"))


(unless (boundp '+SKIINFO-SNOWREPORT+)
  (defconstant +SKIINFO-SNOWREPORT+ "http://www.skiinfo.com/snowreport/"))


(unless (boundp '+skiinfo-top-url+)
  (defconstant +skiinfo-top-country+
    "http://www.skiinfo.com/snowreport/~A.jsp?count=25"))

;; remplacer snowreport par destinations pour avoir des infos sur la station

;; (unless (boundp '+SKIINFO-CHANGE-COUNTRY+)
;;   (defconstant +SKIINFO-CHANGE-COUNTRY+
;;     "http://www.skiinfo.com/?product.skiinfo.CHANGE_USER_TLD="))


(unless (boundp '+SKIINFO-REGION-DETAIL+)
  (defconstant +SKIINFO-REGION-DETAIL+
    "<option value=\"/destinations/region_detail.jsp"))


(unless (boundp '+SKIINFO-DETAIL+)
  (defconstant +SKIINFO-DETAIL+
    "/Snowreport/"))


(unless (boundp '+skiinfo-top+)
  (defconstant +skiinfo-top+
    ;;"/destinations/detail.jsp\?product.skiinfo.DESTID"))
    "product.skiinfo.DESTID="))

(unless (boundp '+SKIINFO-TAG+)
  (defconstant +SKIINFO-TAG+ "<|>|value="))
    ;;"option value ="))



