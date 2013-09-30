;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.lisp
;;;; Purpose:       Ovorost web API.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)




(defmacro with-xml-ovorost (stream &body body)
  "Macro print to STREAM the xml headers and executes BODY."
  `(progn
     (format ,stream "~A" +xml-header+)
     (format ,stream "~&<ovorost version=\"~A\">" *version*)
     ,@body
     (format ,stream "~&</ovorost>")))


;; ----------
;; Publisher
;; ----------


(defclass publisher ()
  ((output-format :initform nil
                  :initarg :output-format
                  :accessor output-format-of))
  (:documentation "Publish data in a specified output format."))


(defgeneric publish-version (publisher &optional stream)
  (:documentation "Display Ovorost version."))


(defgeneric publish-news (publisher &optional stream)
  (:documentation "Display Ovorost news."))


(defgeneric publish-user-infos (publisher user &optional stream)
  (:documentation "Display user's informations."))


(defgeneric publish-resorts-infos (publisher resorts-infos &optional stream)
  (:documentation "Display ski resorts informations."))


(defgeneric publish-user-events (publisher events &optional stream)
  (:documentation "Display all user's events."))


;; XML publisher

(defclass xml-publisher (publisher)
  ()
  (:documentation "A XML publisher."))


(defmethod publish-version ((xml-publisher xml-publisher)
                            &optional (stream *standard-output*))
  (with-xml-ovorost stream))


(defmethod publish-news ((xml-publisher xml-publisher)
                         &optional (stream *standard-output*))
  (with-xml-ovorost stream
    (format stream "~&<news>")
    (format stream "~&</news>")))


(defmethod publish-user-infos ((xml-publisher xml-publisher) user
                               &optional (stream *standard-output*))
  (with-xml-ovorost stream
    (format stream "~&<user>")
    (format stream "~&<first-name>~A</first-name>"
            (trim (user-first-name user)))
    (format stream "~&<last-name>~A</last-name>"
            (trim (user-last-name user)))
    (format stream "~&<sex>~A</sex>"
            (trim (user-sex user)))
    (format stream "~&<longitude>~A</longitude>"
            (trim (user-longitude user)))
    (format stream "~&<latitude>~A</latitude>"
            (trim (user-latitude user)))
    (format stream "~&</user>")))


(defmethod publish-resorts-infos ((xml-publisher xml-publisher) resorts-infos
                                  &optional (stream *standard-output*))
  (with-xml-ovorost stream
    (loop for infos in resorts-infos
       do 
         (format stream "~&<resort>")
         (format stream "~&<name>~A</name>"
                 (trim (resort-infos-name infos)))
         (format stream "~&<longitude>~A</longitude>"
                 (trim (resort-infos-longitude infos)))
         (format stream "~&<latitude>~A</latitude>"
                 (trim (resort-infos-latitude infos)))
         (format stream "~&<url>~A</url>"
                 (trim (resort-infos-url infos)))
         (format stream "~&</resort>"))))


(defmethod publish-user-events ((xml-publisher xml-publisher) events
                                &optional (stream *standard-output*))
  (with-xml-ovorost stream
    (loop for event in events
       do
         (format stream "~&<event>")
         (format stream "~&<title>~A</title>" (trim (event-title event)))
         (format stream "~&<begin>~A</begin>" (trim (event-date-begin event)))
         (format stream "~&<end>~A</end>" (trim (event-date-end event)))
         (format stream "~&</event>"))))




;; JSON publisher

(defclass json-publisher (publisher)
  ()
  (:documentation "A JSON publisher."))


(defmethod publish-version ((json-publisher json-publisher)
                            &optional (stream *standard-output*))
  (json:encode-json ovorost-web::*version* stream))


(defmethod publish-news ((json-publisher json-publisher)
                         &optional (stream *standard-output*))
  (json:encode-json "news" stream))


(defmethod publish-user-infos ((json-publisher json-publisher) user
                               &optional (stream *standard-output*))
  (json:encode-json (trim (user-first-name user)) stream)
  (json:encode-json (trim (user-last-name user)) stream)
  (json:encode-json (trim (user-sex user)) stream)
  (json:encode-json (trim (user-longitude user)) stream)
  (json:encode-json (trim (user-latitude user)) stream))


(defmethod publish-resorts-infos ((json-publisher json-publisher) resorts-infos
                                  &optional (stream *standard-output*))
  (loop for infos in resorts-infos
     do
       (json:encode-json (trim (resort-infos-name infos)) stream)
       (json:encode-json (trim (resort-infos-longitude infos)) stream)
       (json:encode-json (trim (resort-infos-latitude infos)) stream)
       (json:encode-json (trim (resort-infos-url infos)) stream)))


(defmethod publish-user-events ((json-publisher json-publisher) events
                                &optional (stream *standard-output*))
  (loop for event in events
     do
       (json:encode-json (trim (event-title event)))
       (json:encode-json (trim (event-date-begin event)))
       (json:encode-json (trim (event-date-end event)))))


;; -----------
;; Client API
;; -----------



(defmacro with-output-format (publisher stream &body body)
  "Macro which defines a new PUBLISHER using output format defined as
a HTTP request parameter, initialize STREAM and executes BODY."
  `(let* ((format (if (equal :GET (hunchentoot:request-method))
                      (hunchentoot:get-parameter "format")
                      (hunchentoot:post-parameter "format")))
          (,stream (hunchentoot:send-headers))
          ,publisher)
     (setf ,publisher
           (cond ((string-equal format "json")
                  (make-instance 'json-publisher))
                 (t 
                  (make-instance 'xml-publisher))))
     (format t "~&Use publisher : ~A" (type-of publisher))
     ,@body))


(defun api-version ()
  "Get Ovorost informations."
  (with-output-format publisher stream
    (publish-version publisher stream)))

  
(defun api-news ()
  "Get news items."
  (with-output-format publisher stream
    (publish-news publisher stream)))


(defun api-resorts-infos ()
  "Get Resorts informations."
  (with-output-format publisher stream
    (publish-resorts-infos publisher
                           (get-resorts-infos (backend-of *ovorost*))
                           stream)))

;; User Informations 

(defun get-user-infos ()
  "Get user informations."
  (let* ((email (hunchentoot:get-parameter "email")))
    (hunchentoot:log-message :error "API USer Infos ~A" email)
    (let ((user (find-user (backend-of *ovorost*) email)))
      (hunchentoot:log-message :error "Find user ~A" (user-first-name user))
      (when user
        (with-output-format publisher stream
          (publish-user-infos publisher user stream))))))


(defun post-user-infos ()
  "Update user informations with a POST request."
;;   (let ((email (hunchentoot:post-parameter "email"))
;;         (password (hunchentoot:post-parameter "password"))
;;         (last-name (hunchentoot:post-parameter "last-name"))
;;         (first-name (hunchentoot:post-parameter "first-name"))
;;         (latitude (hunchentoot:post-parameter "latitude"))
;;         (longitude (hunchentoot:post-parameter "longitude"))
;;         (sex (hunchentoot:post-parameter "sex")))
;;     (hunchentoot:log-message :info
;;                              "Update user [~A ~A ~A ] ~A ~A ~A ~A / ~A"
;;                              email password last-name first-name sex  latitude longitude)
;;     (let ((user (find-user (ovorost-web:backend-of *ovorost*)
;;                            email :password password)))
;;       (setf (user-first-name user) first-name
;;             (user-last-name user) last-name
;;             (user-latitude user) latitude
;;             (user-longitude user) longitude
;;             (user-sex user) sex)
;;       (ovorost-dao:update-user (ovorost-web:backend-of *ovorost*) user))))
  )


(defun api-user-infos ()
  "Get user informations with GET method, updates informations with a POST
request, otherwise return an +http-not-found+ error."
  (cond ((equal :GET (hunchentoot:request-method))
         (get-user-infos))
        ((equal :POST (hunchentoot:request-method))
         (post-user-infos))
        (t (setf (hunchentoot:return-code) hunchentoot:+http-not-found+))))
                
  
;; User events


(defun get-user-events ()
  "Retrieve user's events."
  (with-output-format publisher stream
    (let* ((email (hunchentoot:get-parameter "email"))
           (password (hunchentoot:get-parameter "password"))
           (user (find-user (ovorost-web:backend-of *ovorost*)
                            email :password password)))
      (when user
        (publish-user-events publisher (user-events user) stream)))))


(defun post-user-event ()
  )


(defun api-user-events ()
  "Get user ski events with a GET method, add an event with a POST request,
otherwise return an +http-not-found+ error."
  (cond ((equal :GET (hunchentoot:request-method))
         (get-user-events))
        ((equal :POST (hunchentoot:request-method))
         (post-user-event))
        (t (setf (hunchentoot:return-code) hunchentoot:+http-not-found+))))