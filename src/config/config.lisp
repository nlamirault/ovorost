;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          configuration.lisp
;;;; Purpose:       Ovorost configuration tool
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-config)



(defclass configuration ()
  ((environment :initarg :environment
                :accessor configuration-environment
                :documentation "Environment of the project.")
   (port :initarg :port
         :accessor configuration-port
         :documentation "Listen port for the web server.")
   (key :initarg :key
        :accessor configuration-key
        :documentation "Google maps API Key.")
   (mod-lisp-p :initarg :mod-lisp-p
               :accessor configuration-mod-lisp-p
               :documentation "Use mod-lisp or not.")
   (captcha :initarg :captcha
            :accessor configuration-captcha
            :documentation "Keys for the Captcha system.")
   (sql :initarg :sql
        :accessor configuration-sql
        :documentation "List : Host, database name, login and password for
the SQL backend."))
  (:documentation "Ovorost configuration."))



(defun make-configuration (environment port key sql mod-lisp-p captcha)
  "Creates a new configuration."
  (make-instance 'configuration
                 :environment environment
                 :port port
                 :key key
                 :sql sql
                 :mod-lisp-p mod-lisp-p
                 :captcha captcha))


(defparameter cl-user::*config* nil "Ovorost configuration")


(defun load-configuration (filename)
  "Load configuration from a file.
Throws a CONFIGURATION-ERROR when if an error occurs during loading the
configuration file FILENAME."
  (load filename)
  (format t "Conf ~A~%" cl-user::*config*)
  (if cl-user::*config*
      (let* ((env (find :env cl-user::*config* :key #'car))
             (port (find :port cl-user::*config* :key #'car))
             (mod-lisp-p (find :mod-lisp cl-user::*config* :key #'car))
             (key (find :key cl-user::*config* :key #'car))
             (captcha (find :captcha cl-user::*config* :key #'car))
             (captcha-public (find :public (cdr captcha) :key #'car))
             (captcha-private (find :private (cdr captcha) :key #'car))
             (sql (find :sql cl-user::*config* :key #'car))
             (host (find :host (cdr sql) :key #'car))
             (database (find :database (cdr sql) :key #'car))
             (login (find :login (cdr sql) :key #'car))
             (password (find :password (cdr sql) :key #'car)))
        (if (or (null host) (null database) (null login) (null password)
                (null env) (null port) (null key))
            (error 'configuration-error)
            (make-configuration (cadr env) (cadr port) (cadr key)
                                (list (cadr host) (cadr database)
                                      (cadr login) (cadr password))
                                (cadr mod-lisp-p)
                                (cons (cadr captcha-public) (cadr captcha-private)))))
      (error 'configuration-error)))