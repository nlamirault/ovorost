;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          errors.lisp
;;;; Purpose:       Database errors of Ovorost
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault

;;;; *************************************************************************



(in-package :ovorost-dao)



(define-condition backend-error (ovorost-error)
  ()
  (:documentation "No backend defined.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "No backend defined."))))


(define-condition user-error (ovorost-error)
  ((email :reader user-email :initarg :email)))


(define-condition unknown-user-error (user-error)
  ()
  (:documentation "An unknown user error")
  (:report (lambda (condition stream)
             (format stream "Unknown user ~A"
                     (user-email condition)))))


(define-condition existing-user-error (user-error)
  ()
  (:documentation "An existing user error")
  (:report (lambda (condition stream)
             (format stream "user with email ~A already exists"
                     (user-email condition)))))


(define-condition entity-error (ovorost-error)
  ((name :reader name-of :initarg :name)))


(define-condition unknown-resort-error (entity-error)
  ()
  (:documentation "An unknown resort error")
  (:report (lambda (condition stream)
             (format stream "Unknown resort ~A"
                     (name-of condition)))))


(define-condition existing-resort-error (entity-error)
  ()
  (:documentation "An existing resort error")
  (:report (lambda (condition stream)
             (format stream "Ski resort ~A already exists."
                     (name-of condition)))))


(define-condition unknown-country-error (entity-error)
  ()
  (:documentation "An unknown country error")
  (:report (lambda (condition stream)
             (format stream "Unknown country ~A"
                     (name-of condition)))))


(define-condition existing-country-error (entity-error)
  ()
  (:documentation "An existing country error")
  (:report (lambda (condition stream)
             (format stream "Existing country ~A"
                     (name-of condition)))))


(define-condition unknown-region-error (entity-error)
  ()
  (:documentation "An unknown region error")
  (:report (lambda (condition stream)
             (format stream "Unknown region ~A"
                     (name-of condition)))))


(define-condition existing-region-error (entity-error)
  ()
  (:documentation "An existing region error")
  (:report (lambda (condition stream)
             (format stream "Existing region ~A"
                     (name-of condition)))))


(define-condition event-error (ovorost-error)
  ((email :reader event-email :initarg :email)
   (title :reader event-title :initarg :title)))


(define-condition unknown-event-error (event-error)
  ()
  (:documentation "An unknown event error")
  (:report (lambda (condition stream)
             (format stream "Unknown event ~A owned by ~A"
                     (event-title condition)
                     (event-email condition)))))


(define-condition existing-event-error (event-error)
  ()
  (:documentation "An existing event error")
  (:report (lambda (condition stream)
             (format stream "Existing event ~A owned by ~A"
                     (event-title condition)
                     (event-email condition)))))
