;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-gravatar.lisp
;;;; Purpose:       Gravatar wrapper
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; ovorost users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :cl-gravatar
  (:use :cl)
  (:documentation "Gravatar wrapper.")
  (:export #:get-gravatar
           ))


(in-package :cl-gravatar)



(defparameter *gravatar-uri*
  "http://www.gravatar.com/avatar.php?gravatar_id=~A&size=~A%"
  "URI to retrieve the gravatar.")



(defun md5sum-to-hexa (md5-hash)
  "Convert MD5-HACH to a string representation in Hexadecimal format."
  (let (hexa)
    (with-output-to-string (os)
      (loop for i across md5-hash
         do (format os "~2,'0x" i))
      (setf hexa (get-output-stream-string os)))
    (string-downcase hexa)))


(defun sequence-trim (sequence)
  "Trim SEQ."
  (string-trim '(#\Space #\Tab #\Newline) sequence))


(defun get-gravatar (email &key (size 40) default)
  "Get the Gravatar associated to user's EMAIL address.
SIZE specifies the desired width and height of the gravatar. Valid values are
from 1 to 80 inclusive.
DEFAULT specifies the URL of a GIF, JPEG, or PNG image that should be returned
if either the requested email address has no associated gravatar, or that gravatar
has a rating higher than is allowed by the 'rating' parameter."
  (let ((hash-mail (md5sum-to-hexa (md5:md5sum-sequence (sequence-trim email))))
        gravatar)
    (with-output-to-string (stream)
      (format stream *gravatar-uri* hash-mail size)
      (when default
        (format stream "&default=~A" (url-rewrite:url-encode default)))
      (setf gravatar (get-output-stream-string stream)))
    gravatar))


