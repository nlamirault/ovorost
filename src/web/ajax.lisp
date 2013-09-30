;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ajax.lisp
;;;; Purpose:       Ajax functionalities of ovorost.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)


(defparameter *resort-xml*
  "~&<marker lat=\"~A\" lng=\"~A\" html=\"~A\" label=\"~A\" web=\"~A\" slopes=\"~A\" snow=\"~A\" avalanche=\"/img/avalanche_risk_~A.gif\" updated=\"~A\" />")

(defparameter *event-xml*
  "~&<marker title=\"~A\" begin_date=\"~A\" end_date=\"~A\" lat=\"~A\" lng=\"~A\" label=\"~A\" url=\"~A\" />")



(defun print-resorts (country region &optional (stream *standard-output*))
  "Parse database resorts and print to STREAM resorts informations 
find using COUNTRY and REGION in a XML representation."
  (let* ((country-name (string-downcase (symbol-name country)))
         (region-name (string-downcase (symbol-name region))))
    (format stream "~&<markers>~%")
    (let ((country-data (find country-name *resorts*
                              :test #'string-equal :key #'car)))
      (when country-data
        (let ((region-data (find region-name (cadr country-data)
                                 :test #'string-equal :key #'first)))
          (when region-data
            (loop for resort-data in (cadr region-data)
               do (destructuring-bind (lng lat name web slopes snow avalanche updated) resort-data
                    (format stream *resort-xml* lat lng name name web slopes snow avalanche updated)))))))
    (format stream "~&</markers>~%")))


(defexported get-ski-resorts (country region)
  ;; PB with sbcl 1.0.14 failed to defined function
  ;;&optional (stream *standard-output*))
   "Get all ski resorts for a specified COUNTRY REGION, and creates an XML document."
   (hunchentoot:log-message :error (format nil "Ski resorts ~A from ~A"
                                           country region))
   (let* ((stream (make-string-output-stream)))
     (print-resorts country region stream)
     (hunchentoot:log-message :error "Fin Output")
     (get-output-stream-string stream)))


(defun print-events (user &optional (stream *standard-output*))
  "Retrieve USER's events and print them to STREAM with a XML representation."
    (format stream "~&<markers>~%")
    (when user
      (let ((events (user-events user)))
        (loop for event in events
           as resort-infos = (event-resort-infos event)
           do (format stream *event-xml*
                      (trim (event-title event))
                      (trim (event-date-begin event))
                      (trim (event-date-end event))
                      (trim (resort-infos-latitude resort-infos))
                      (trim (resort-infos-longitude resort-infos))
                      (trim (resort-infos-name resort-infos))
                      (format nil "/show-event?id=~A" (event-id event))))))
    (format stream "~&</markers>~%"))


(defexported get-user-events ()
;; Pb to load with sbcl 1.0.14 (&optional (stream *standard-output*))
  "Get user's events."
  (let* ((user-data (hunchentoot:session-value 'user-id))
         (user (find-user (backend-of *ovorost*)
                          (car user-data) :password (cdr user-data)))
         (stream (make-string-output-stream)))
      (print-events user stream)
      (get-output-stream-string stream)))