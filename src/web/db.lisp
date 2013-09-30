;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          db.lisp
;;;; Purpose:       Database interaction..
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)


(defun update-db-country (backend dir country-name)
  "Update informations about country's ski resorts."
;;;   (loop for region in (list-directory dir)
;;;      as region-name = (pathname-name region)
;;;      as data = (concatenate 'string (namestring dir) region-name ".csv")
;;;      do
;;;        (let ((infos (update-resort-informations data (get-infos backend) country-name)))
;;;          (loop for resort in infos 
;;;             do (handler-case
;;;                    (add-resort backend
;;;                                (first resort) country-name region-name
;;;                                (second resort) (third resort) (fourth resort)
;;;                                (fifth resort) (sixth resort) (seventh resort)
;;;                                (eighth resort) (ninth resort) (tenth resort))
;;;                  (existing-resort-error (msg)
;;;                    (warn (format nil "~A" msg))))))))
;;;   ))
  )


(defun update-database (backend directory)
  "Update ski database"
  ;;(let ((db-infos (get-infos backend)))
  (loop for dir in (list-directory (concatenate 'string
                                                directory
                                                "var/"))
     as country-name = (car (last (pathname-directory dir)))
     as dir-name = (namestring dir)
     do (format t "Country ~A ...~A ~%" country-name dir)
;;          (loop for region in (list-directory dir)
;;             as region-name = (pathname-name region)
;;             as data = (concatenate 'string dir-name region-name ".csv")
;;             do
;;               (let ((infos (update-resort-informations data db-infos country-name)))
;;                 (loop for resort in infos 
;;                    do (handler-case
;;                           (add-resort backend
;;                                       (first resort) country-name region-name
;;                                       (second resort) (third resort) (fourth resort)
;;                                       (fifth resort) (sixth resort) (seventh resort)
;;                                       (eighth resort) (ninth resort) (tenth resort))
;;                         (existing-resort-error (msg)
;;                           (warn (format nil "~A" msg)))))))
       (update-db-country backend directory country-name)
         ))



(defun load-resorts-from-file (filename)
  "Read FILENAME and get a list of all resorts."
  (with-open-file (in-stream filename :direction :input)
      (loop as line = (read-line in-stream nil)
         until (null line)
         as data = (cl-ppcre:split ";" line)
         collect data)))


(defun load-resorts ()
  "Load all resorts from definitions files."
  (let (data)
    (loop for country-data in *countries*
       as country-name = (first country-data)
       as region-directory = (concatenate 'string
                                          *directory*
                                          *content-directory* country-name "/")             
       do
         (format t "~&Load ~A data." country-name)
         (push (cons country-name
                     (loop for filename in (list-directory region-directory)
                        collect (cons (pathname-name filename)
                                      (load-resorts-from-file (namestring filename)))))
               data))
    ;;(setf *resorts* data)))
    data))


(defun set-up-resorts (backend)
  "Add resorts informations to the database."
  (let ((db (load-resorts)))
    (loop for country in *countries*
       do (let ((country-data
                 (find (first country) db :test #'string-equal :key #'car)))
            (when country-data
              (loop for region-data in (cdr country-data)
                 do (loop for resort-data in (cdr region-data)
                       do 
                         (format t "~&Resort data : ~A" resort-data)
                         (destructuring-bind (lng lat name url infos)
                             resort-data
                           (format t "~&~A : ~A : ~A ~A ~A ~A"
                                   (first country) (first region-data) lng lat name url)
                           (let ((resort
                                  (add-resort-infos backend
                                                    name (first country) (first region-data)
                                                    lng lat url))))))))))))


(defun update-ovorost (backend)
  "Update the database.
For each resorts, do a HTTP request to SkiInfo, parse HTML stream to find slopes, snow
and avalanche risk."
  (let ((db (load-resorts)))
    (loop for country in *countries*
       do
         (let ((country-data
                (find (first country) db :test #'string-equal :key #'car)))
           (when country-data
             (loop for region-data in (cdr country-data)
                do (loop for resort-data in (cdr region-data)
                      when resort-data
                      do 
                        (format t "~&Resorts data : ~A" resort-data)
                        (destructuring-bind (lng lat name url infos)
                            resort-data
                          (format t "~&Resort ~A : ~A : ~A ~A ~A ~A ~A"
                                  (first country) (first region-data) lng lat name url infos)
                          (when (cl-ppcre:scan "http" infos)
                            (handler-case
                                (destructuring-bind (slopes snow avalanche)
                                    (parse-ski-info infos)
                                  (format t "Slopes : ~A~%Snow: ~A~%Avalanche: ~A~%"
                                          slopes snow avalanche)
                                  (update-resort-infos backend
                                                       name
                                                       (format nil "~{~A~^-~}" slopes)
                                                       (format nil "~{~A~^, ~}" snow)
                                                       avalanche
                                                       (iso-time)))
                              (error (condition)
                                (warn "Ovorost update problem : ~A~%" condition))))))))))))
  


(defun resort-infos-to-list (resort-infos)
  "Tranform RESORT-INFOS to a list."
  (with-accessors ((lng resort-infos-longitude)
                   (lat resort-infos-latitude)
                   (name resort-infos-name)
                   (url resort-infos-url)
                   (slopes resort-infos-slopes)
                   (snow resort-infos-snow)
                   (avalanche resort-infos-avalanche)
                   (updated resort-infos-updated)) resort-infos
    (list (trim lng)
          (trim lat)
          (trim name)
          (trim url)
          (trim slopes)
          (trim snow)
          (trim avalanche)
          (trim updated))))


(defun region-to-list (region)
  "Transform REGION informations to a list."
  (list (trim (region-name region))
        (mapcar #'resort-infos-to-list (region-resorts region))))


(defun country-to-list (country)
  "Transform COUNTRY informations to a list."
  (list (trim (country-name country))
        (mapcar #'region-to-list (country-regions country))))


 (defun get-up-resorts (backend)
   "Load resorts informations from database and update *resorts*."
   (let ((countries (get-countries backend)))
     (loop for country in countries
        collect (country-to-list country))))


