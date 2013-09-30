;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ajax.lisp
;;;; Purpose:       Unit Test suite for Ovorost Ajax module.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-test)



(lift:deftestsuite ovorost-ajax-test (ovorost-test)
  ((ovorost-ut (make-ovorost 'ut))
   (resorts-infos
    '(("first" "france" "pyrenees" "0.47720" "42.7874" "http://www.ovorost.com")
      ("second" "france" "pyrenees" "2.03" "42.504" "http://www.ovorost.com"))))
  (:run-setup :once-per-suite)
  (:setup (progn
            ;;(ovorost-web:setup ovorost-ut)
            (open-backend (backend-of ovorost-ut))
            (create-sql-database)
            (loop for data in resorts-infos
               do (destructuring-bind (name country region longitude latitude url)
                      data
                    (add-resort-infos (backend-of ovorost-ut)
                                      name country region longitude latitude url)))
            (close-backend (backend-of ovorost-ut))
            (ovorost-web:start ovorost-ut)
            (sleep 5)))
  (:teardown (ovorost-web:stop ovorost-ut))
  (:documentation "Unit Test suite for Ovorost Ajax module."))



(defmacro with-ovorost-ajax ((body-stream status-code) method parameters &body body)
  `(multiple-value-bind (,body-stream ,status-code headers uri stream must-close)
      (drakma:http-request
       (format nil "http://localhost:~A/ajax-function/?ajax-fun=~A&ajax-xml=true&~A"
               (configuration-port (ovorost-web:config-of ovorost-ut))
               (string-upcase ,method)
               ,parameters))
     (declare (ignore headers uri stream must-close))
     ,@body))

  
(lift:addtest (ovorost-ajax-test)
  get-ski-resorts-correct
  (with-ovorost-ajax (body-stream status-code) "get-ski-resorts" "ajax-1=france&ajax-2=pyrenees"
    (lift:ensure (= 200 status-code))
    (format t "Resorts : ~A~%" body-stream)
    (test-numbers-of-tokens body-stream "<markers>" 1)
    (test-numbers-of-tokens body-stream "<marker " 2)
    (test-numbers-of-tokens body-stream "</markers>" 1)))


;; How Test ?
;; (lift:addtest (ovorost-ajax-test)
;;   get-user-events-correct
;;   (let ((user (add-user (backend-of ovorost-ut)
;;                         "test@ovorost.com"
;;                         "test"
;;                         "first-name" "last-name" "12" "12" "M"
;;                         :active-p t)))
;;     (setf (hunchentoot:session-value 'user-id)
;;           (cons "test@ovorost.com" "test"))
;;     (with-ovorost-ajax (body-stream status-code) "get-user-events" ""
;;       (format t "Ajax user : ~A~%" body-stream))))


