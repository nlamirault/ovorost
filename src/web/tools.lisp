;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tools.lisp
;;;; Purpose:       Some tools for the web site.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)


(defun send-mail (from to subject body)
  "Sends a mail message."
  (cl-smtp:send-email *smtp-host* from to subject body))



(defun active-user (email password)
  (let ((user (find-user (backend-of *ovorost*) email :password password)))
    (when user
      (with-accessors ((active-p user-active-p)) user
        (setf active-p t)
        (update-user (backend-of *ovorost*) user)
        user))))



(defun get-resorts-statistics ()
  (loop for country in *resorts*
     collect (cons (car country)
                   (loop for region in (cadr country)
                         collect (cons (car region) (length (cadr region)))))))


(defun get-country-statistics (name)
  (let ((data (cadr (find name *resorts* :test #'string-equal :key #'car))))
    (when data
      (loop for region in data
         collect (cons (car region) (length (cadr region)))))))


(defun graph-country-resorts (name &optional (stream *standard-output*))
  "Creates a graph representing ski resorts repartition for a country.
Return the url to load the graph."
  (let* ((stats (get-country-statistics name))
         (data (list (mapcar #'cdr stats)))
         (legend (mapcar #'car stats))
         (google-chart (cl-google-chart::make-google-chart))
         (chart (cl-google-chart::make-pie-chart :p3
                                                 (cons 600 200)
                                                 data
                                                 :simple-encoding
                                                 :title (format nil "~A Ski resorts" name)
                                                 :legend legend
                                                 :colors '("000FFF"
                                                           "EFEFEF"
                                                           "AFAFAF"))))
           (cl-google-chart::print-url google-chart chart stream)))


(defun user-events-pie-chart (user &optional (stream *standard-output*))
  "Creates a pie chart representing USER's events repartition."
  (let* ((data (ovorost-tools::occurrences
               (mapcar #'event-resort-infos (user-events user))
               :test (lambda (resort name)
                       (string-equal (resort-infos-name resort) name))
               :key (lambda (resort)
                      (resort-infos-name (car resort)))))
         (google-chart (cl-google-chart::make-google-chart))
         (chart (cl-google-chart::make-pie-chart :p3
                                                 (cons 600 200)
                                                 (list (mapcar #'cdr data))
                                                 :simple-encoding
                                                 :title "Events resorts"
                                                 :legend (mapcar #'(lambda (item)
                                                                     (trim (resort-infos-name (car item))))
                                                                 data)
                                                 :colors '("000FFF" "EFEFEF" "AFAFAF"))))
    (cl-google-chart::print-url google-chart chart stream)))


(defun get-resorts-by-countries ()
  "Return a list of assoc lists, which count for each country the number
of ski resorts."
  (let ((countries-data (get-countries (backend-of *ovorost*)))
        (resorts (get-resorts-infos (backend-of *ovorost*))))
    (loop for country-data in *countries*
          as country = (find (first country-data) countries-data
                             :test #'string-equal
                             :key #'(lambda (token)
                                       (trim (country-name token))))
          collect (cons (trim (country-name country))
                        (count (ovorost-dao::country-id country)
                               resorts
                               :test #'=
                               :key #'ovorost-dao::resort-infos-countryid)))))


(defun countries-resorts-chart (&optional (stream *standard-output*))
  "Creates a bar chart of number of ski resorts for each country."
  (let* ((data (get-resorts-by-countries))
         (google-chart (cl-google-chart::make-google-chart))
         (chart (cl-google-chart::make-bar-chart :stacked
                                                 (cons 600 150)
                                                 (mapcar #'cdr data)
                                                 :vertical
                                                 :extended-encoding
                                                 :title "Resorts by countries"
                                                 :legend (mapcar #'(lambda (item)
                                                                      (format nil "~A:~A" (car item) (cdr item)))
                                                                 data))))
    (cl-google-chart::print-url google-chart chart stream)))
    