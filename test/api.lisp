;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.lisp
;;;; Purpose:       Unit Test suite for Ovorost API.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-test)


(lift:deftestsuite ovorost-api-test (ovorost-test)
  ((ovorost-ut (make-ovorost 'ut))
   (resort-infos '("resort" "france" "pyrenees" "0.47720" "42.7874" "http://www.ovorost.com"))
   (user-infos '("first-name" "last-name" "M" "-0.58" "44.83")))
  (:run-setup :once-per-suite)
  (:setup (progn
            ;;(ovorost-web:setup ovorost-ut)
            (open-backend (backend-of ovorost-ut))
            (create-sql-database)
            (close-backend (backend-of ovorost-ut))
            (ovorost-web:start ovorost-ut)
            (sleep 5)))
  (:teardown (ovorost-web:stop ovorost-ut))
  (:documentation "Unit Test suite for Ovorost API."))



(defmacro with-ovorost-api ((body-stream status-code) request &body body)
  `(multiple-value-bind (,body-stream ,status-code headers uri stream must-close)
      (drakma:http-request
       (format nil "http://localhost:~A/api/~A"
               (configuration-port (ovorost-web:config-of ovorost-ut))
               ,request))
     (declare (ignore headers uri stream must-close))
     (lift:ensure (= 200 ,status-code))
     ,@body))


(lift:addtest (ovorost-api-test)
  api-version-correct
  (with-ovorost-api (body-stream status-code) "version"
    ;;(lift:ensure (= 200 status-code))
    (lift:ensure (string-equal
                  (format nil
"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
<ovorost version=\"~A\">
</ovorost>" ovorost-web::*version*)
                  body-stream))))


(lift:addtest (ovorost-api-test)
  api-news-correct
  (with-ovorost-api (body-stream status-code) "news"
    ;;(lift:ensure (= 200 status-code))
    (test-numbers-of-tokens body-stream "<news>" 1)
    (test-numbers-of-tokens body-stream "</news>" 1)))


(lift:addtest (ovorost-api-test)
  api-user-correct
  (destructuring-bind (first-name last-name sex longitude latitude)
      user-infos
    (add-user (backend-of ovorost-ut)
              "test@ovorost.com" "test"
              first-name last-name
              longitude latitude sex :active-p t)
    (with-ovorost-api (body-stream status-code) "user?email=test@ovorost.com"
      ;;(lift:ensure (= 200 status-code))
      (test-numbers-of-tokens body-stream "<user>" 1)
      (lift:ensure (cl-ppcre:scan (format nil "<first-name>~A</first-name>" first-name)
                                  body-stream))
      (lift:ensure (cl-ppcre:scan (format nil "<last-name>~A</last-name>" last-name)
                                  body-stream))
      (lift:ensure (cl-ppcre:scan (format nil "<sex>~A</sex" sex)
                                  body-stream))
      (lift:ensure (cl-ppcre:scan (format nil "<longitude>~A</longitude>" longitude)
                                  body-stream))
      (lift:ensure (cl-ppcre:scan (format nil "<latitude>~A</latitude>" latitude)
                                  body-stream))
      (test-numbers-of-tokens body-stream "</user>" 1))))



(lift:addtest (ovorost-api-test)
  api-resorts-correct
  (destructuring-bind (name country region longitude latitude url)
      resort-infos
    (add-resort-infos (backend-of ovorost-ut)
                      name country region longitude latitude url)
    (with-ovorost-api (body-stream status-code) "resorts"
      (test-numbers-of-tokens body-stream "<resort>" 1)
      (lift:ensure (cl-ppcre:scan (format nil "<name>~A</name>" name)
                                  body-stream))
      (lift:ensure (cl-ppcre:scan (format nil "<longitude>~A</longitude>" longitude)
                                  body-stream))
      (lift:ensure (cl-ppcre:scan (format nil "<latitude>~A</latitude>" latitude)
                                  body-stream))
      (lift:ensure (cl-ppcre:scan (format nil "<url>~A</url>" url)
                                  body-stream))
      (test-numbers-of-tokens body-stream "</resort>" 1))))

