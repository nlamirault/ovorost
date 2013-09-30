;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-recaptcha.lisp
;;;; Purpose:       ReCaptcha wrapper
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; ovorost users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :cl-recaptcha
  (:use :cl)
  (:documentation "Gravatar wrapper.")
  (:export 
   
   #:print-captcha
   #:verify-captcha

   ))


(in-package :cl-recaptcha)



(defparameter *recaptcha-server* "http://api.recaptcha.net"
  "The ReCaptcha URI")


(defparameter *recaptcha-verify* "http://api-verify.recaptcha.net/verify"
  "URI to verify captcha.")


(defun print-captcha (public-key &optional (stream *standard-output*))
  "Print the ReCaptcha client."
  (format stream "<script type=\"text/javascript\" src=\"~A/challenge?k=~A\"></script>"
          *recaptcha-server* public-key))


(defun verify-captcha (private-key remove-ip challenge response)
  "Verify recaptcha entered by user."
  (let ((params '()))
    (push (cons "privatekey" private-key) params)
    (push (cons "remoteip" remove-ip) params)
    (push (cons "challenge" challenge) params)
    (push (cons "response" response) params)
    (multiple-value-bind (body-stream status-code headers uri stream must-close)
        (drakma:http-request *recaptcha-verify*
                             :method :post
                             :parameters params)
      (declare (ignore headers uri stream must-close))
      ;;body-stream)))
      (if (and status-code (= status-code 200))
          (cl-ppcre:scan "true" body-stream)
          nil))))

