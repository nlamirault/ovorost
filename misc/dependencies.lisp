;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          depends.lisp
;;;; Purpose:       Install all dependencies of Ovorost.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of Ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; Ovorost users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(require 'asdf)
(require 'asdf-install)


(defparameter *project-directory*
    (merge-pathnames "src/ovorost/" (user-homedir-pathname)))

(unless (find *project-directory* asdf:*central-registry*)
  (push *project-directory* asdf:*central-registry*))

(defparameter *ovorost-dependencies*
  `((,(merge-pathnames "depends/site/" *project-directory*)
     ,(merge-pathnames "depends/systems/" *project-directory*)
     "OVOROST dependencies installations"))
  "OVOROST dependencies installations.")


(push (cadar *ovorost-dependencies*) asdf:*central-registry*)

;; (when (null (find (cadar *ovorost-dependencies*) asdf:*central-registry*))
;;   (format t "~&=============")
;;   (push (cadar *ovorost-dependencies*) asdf:*central-registry*))

;; (format t "~&HOP: .... ~A"
;;         asdf-install:*locations*)

;; (let ((alreay (find (caar *ovorost-dependencies*)
;;                     asdf-install:*locations* :key #'car)))
;;   (format t "~&-----------nul .... ~A" alreay)
;;   (push (car *ovorost-dependencies*) asdf-install:*locations*))

(push (car *ovorost-dependencies*) asdf-install:*locations*)

(defparameter asdf-install-customize::*verify-gpg-signatures* nil)


(defun unsafe-install (package)
   (handler-bind
       ((simple-error
	#'(lambda (c)
	    (invoke-restart 'skip-gpg-check))))
     (asdf-install:install package)))



(loop for dep in '(:hunchentoot
                   :html-template
                   :cl-smtp
                   ;;:cl-i18n ;; patchee le 28/02/2008
                   :drakma
                   :cl-ppcre
                   :uffi
                   :clsql
                   :parenscript
                   :cl-json
                   :closure-html
                   :cxml-stp)
      do (with-input-from-string (*standard-input* "1")
           (asdf-install:install dep)))
;;(unsafe-install dep)))
