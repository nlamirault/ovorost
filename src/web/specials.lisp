;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          specials.lisp
;;;; Purpose:       Main parameters.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)


(defparameter *version*
  (asdf:component-version (asdf:find-system "ovorost")))

(defparameter *directory*
  (namestring (asdf:component-relative-pathname (asdf:find-system "ovorost"))))

(defparameter *author* "Nicolas Lamirault")

(defparameter *project* "Ovorost")


(defparameter *ovorost* nil "The Ovorost system.")

;;(defparameter *ovorost-server* nil "The Web Server for Ovorost.")

;;(defparameter *ovorost-config* nil "The Ovorost configuration.")

;;(defparameter *db* nil)



(defparameter *ovorost-directory* nil)

(defparameter *templates-directory* "html/"
  "Name of the directory which contains HTML templates.")

(defparameter *stylesheets-directory* "css/"
  "Name of the directory which contains CSS.")

(defparameter *javascript-directory* "js/"
  "Name of the directory which contains javascript scripts.")

(defparameter *images-directory* "img/"
  "Name of the directory which contains images.")

(defparameter *config-directory* "etc/environments/"
  "Name of the configuration directory.")

(defparameter *content-directory* "var/"
  "Name of the directory which contains data about ski resorts.")

(defparameter *page-prefix* "/ovorost/")

(defparameter *environments* '(local dev test prod)
  "Available environments.")

(defparameter *resorts* '())

(defparameter *countries*
  (list
   ;; Europe
   (list "france" "46.900" "2.393" "6")
   (list "austria" "47.521" "13.27" "7")
;;         (list "germany" "51.58" "9.9" "6")
         (list "italy" "42.537" "12.139" "6")
;;         (list "norway" "64.466" "12.305" "6")
;;         (list "sweden" "59.837" "13.145" "6")
;;         (list "switzerland" "46.612" "7.5" "7")))
   ;; Asie
;;    (list "armenie" "40" "44.69" "8")
;;    (list "australie" "-25" "130" "4")
;;    (list "inde" "20" "78" "5")
   ;; America
;;    (list "argentine" "-40" "-65" "4")
;;    (list "bolivie" "-16.43" "-68.15" "5")
   ))


;; i18n

(unless (boundp '+default-locale+)
    (defconstant +default-locale+ "en"))


(defparameter *translation* nil "Translation tokens.")


(defparameter *languages* '("en" "fr" "es")
  "A list of available languages.")


;; Mailing 

(defparameter *smtp-host* "smtp.free.fr")

(defparameter *from* "ovorost-admin@perave.org")

(defparameter *user-added-text*
  (cons "[Ovorost] Account creation" 
        "~&Welcome to Ovorost ~A ~A !~%
~&Your account will be available as soon as possible ...
~&Informations:
~&Login : ~A
~&Password : ~A
~&Regards~%~%
~&Ovorost (c) 2008 Perave - All rights reserved"))


;; API

(unless (fboundp '+xml-header+)
  (defconstant +xml-header+
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"))

