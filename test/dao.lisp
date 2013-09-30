;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          dao.lisp
;;;; Purpose:       Unit Test suite for Ovorost database.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; ovorost users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :ovorost-test)


(lift:deftestsuite ovorost-dao-test (ovorost-test)
  ((ovorost-ut (make-ovorost 'ut)))
  (:run-setup :once-per-suite)
  (:setup (ovorost-web:setup ovorost-ut))
  (:documentation "Unit Test suite for Ovorost database system."))


(defmacro with-correct-config (backend &body body)
  "Macro which load configuration, creates a database backend,
open this backend, executes BODY and closes the backend."
  `(let ((config (load-configuration (concatenate 'string
                                                  *ovorost-source*
                                                  +ut-configuration-ok+))))
     (destructuring-bind (host base login password)
         (configuration-sql config)
       (let ((,backend (make-sql-backend host base login password :postgresql)))
         (open-backend ,backend)
         (unwind-protect
              (progn
                ,@body)
           (close-backend ,backend))))))


(lift:addtest (ovorost-dao-test)
  dao-valid-db
  (:documentation "Test connection to a valid database.")
  (with-correct-config backend
    (let ((db (car (clsql-sys:connected-databases))))
      (format t "~&Type : ~A" (type-of db))
      (lift:ensure (eq (type-of db)
                       'clsql-postgresql:postgresql-database))
      (format t "~&Name : ~A" (clsql:database-name db))
      (lift:ensure (string-equal (clsql:database-name db)
                                 (format nil "~A/~A/~A" host base login))))))


;; Pb UTF-8 with clsql
;; (lift:addtest (ovorost-dao-test)
;;   dao-unknown-db
;;   (:documentation "Test connection to an unknown database.")
;;   (when cl-user::*config*
;;     (setf cl-user::*config* nil))
;;   (let ((config (load-configuration (concatenate 'string
;;                                                  *ovorost-source*
;;                                                  +ut-configuration-ok+))))
;;     (lift:ensure (eq (type-of config) 'configuration))
;;     (destructuring-bind (host base login password)
;;         (configuration-sql config)
;;       (declare (ignore base))
;;       (let ((backend (make-sql-backend host (format nil "~A" (gensym)) login password :postgresql)))
;;         (open-backend backend)
;;         (close-backend backend)))))



;; Country

(lift:addtest (ovorost-dao-test)
  dao-add-country-test
  (:documentation "Test adding a new country.")
  (with-correct-config backend
    (let ((name (format nil "country-~A" (gensym))))
      (lift:ensure (null (find-country backend name)))
      (add-country backend name)
      (let ((country (find-country backend name)))
        (lift:ensure (eq (type-of country) 'country))
        (lift:ensure (string-equal (ovorost-tools:trim (country-name country))
                                   name))))))


(lift:addtest (ovorost-dao-test)
  dao-add-existing-country-test
  (:documentation "Test it's not possible to add an existing country.")
  (with-correct-config backend
    (let ((name (format nil "country-~A" (gensym))))
      (add-country backend name)
      (lift:ensure-condition 'existing-country-error
        (add-country backend name)))))


(lift:addtest (ovorost-dao-test)
  dao-list-countries-test
  (:documentation "Test retrieving all countries.")
  (with-correct-config backend
    (let* ((names (loop for i from 1 to 10
                     as name = (format nil "country-~A" i)
                     do (add-country backend name)
                     collect name))
           (countries (get-countries backend)))
      (loop for name in names
           as country = (find name countries
                              :test #'string-equal
                              :key #'(lambda (item)
                                       (ovorost-web::trim (country-name item))))
           do
           (lift:ensure (eq (type-of country) 'country))
           (lift:ensure (string-equal (ovorost-tools:trim (country-name country))
                                      name))))))


;; Region


(lift:addtest (ovorost-dao-test)
  dao-add-region-test
  (:documentation "Test adding a new region.")
  (with-correct-config backend
    (let ((region-name (format nil "region-~A" (gensym))))
      (add-region backend region-name (format nil "country-~A" (gensym)))
      (let ((region (find-region backend region-name)))
        (lift:ensure (eq (type-of region) 'region))))))


(lift:addtest (ovorost-dao-test)
  dao-add-existing-region-test
  (:documentation "Test adding a new region.")
  (with-correct-config backend
    (let ((country-name (format nil "country-~A" (gensym)))
          (region-name (format nil "region-~A" (gensym))))
      (add-region backend region-name country-name)
      (lift:ensure-condition 'existing-region-error
        (add-region backend region-name country-name)))))



;; User

(lift:addtest (ovorost-dao-test)
  dao-user-test
  (:documentation "Test user API : add and delete..")
  (with-correct-config backend
    (destructuring-bind (email password first-name last-name lng lat sex)
        (list (format nil "~A@ovorost.com" (gensym))
              (format nil "~A-unit-test" (gensym))
              "unit" "test" "40" "1" "M")
      (lift:ensure (null (find-user backend email :password password)))
      (let ((sql-user
             (add-user backend email password first-name last-name
                       lng lat sex)))
        (lift:ensure (eq (type-of sql-user) 'user))
        (lift:ensure (string-equal email (user-email sql-user)))
        (lift:ensure (string-equal password (user-password sql-user)))
        (lift:ensure (string-equal first-name (user-first-name sql-user)))
        (lift:ensure (string-equal last-name (user-last-name sql-user)))
        (lift:ensure (string-equal lng (user-longitude sql-user)))
        (lift:ensure (string-equal lat (user-latitude sql-user)))
        (lift:ensure (string-equal sex (user-sex sql-user)))
        (lift:ensure (null (user-active-p sql-user)))
        (lift:ensure (null (user-administrator-p sql-user))))
      (delete-user backend email password)
      (lift:ensure (null (find-user backend email :password password))))))



(lift:addtest (ovorost-dao-test)
  dao-add-user-invalid-test
  (:documentation "Test not possible to add two users with same email.")
  (with-correct-config backend
    (destructuring-bind (email password first-name last-name lng lat sex)
        (list (format nil "~A@ovorost.com" (gensym))
              (format nil "~A-unit-test" (gensym))
              "unit" "test" "40" "1" "M")
      (lift:ensure (null (find-user backend email :password password)))
      (add-user backend email password first-name last-name
                lng lat sex)
      (lift:ensure-condition 'existing-user-error
        (add-user backend email password first-name last-name
                  lng lat sex)))))
    

;; Resort infos

(lift:addtest (ovorost-dao-test)
  dao-add-resort-infos-test
  (:documentation "Test resort-infos API : search, add and delete..")
  (with-correct-config backend
    (destructuring-bind (name country region lng lat url)
        (list (format nil "~A" (gensym)) "france" "pyrenees" "40" "1"
              "http://www.google.fr")
      (lift:ensure (null (find-resort-infos backend name)))
      (let ((sql-resort-infos
             (add-resort-infos backend name country region lng lat url)))
        (lift:ensure (and (string-equal name (resort-infos-name sql-resort-infos))
                          (string-equal lng (resort-infos-longitude sql-resort-infos))
                          (string-equal lat (resort-infos-latitude sql-resort-infos))))))))


;; Event

(lift:addtest (ovorost-dao-test)
  dao-add-event-test
  (:documentation "Test adding a new event.")
  (with-correct-config backend
    (let ((sql-user (add-user backend
                              (format nil "~A@ovorost.com" (gensym))
                              (format nil "~A-unit-test" (gensym))
                              "unit" "test" "44" "1" "M"))
          (sql-resort-infos (add-resort-infos backend
                                              (format nil "resort-~A" (gensym))
                                              "france" "pyrenees" "44" "1"
                                              "http://www.google.fr")))
      (destructuring-bind (title begin end note content public-p tags)
          (list (format nil "Event ~A" (gensym))
                "2007-12-31" "2008-01-02" 7  "Test event" t (list "France" "Pyrenees"))
        (let ((sql-event
               (add-event backend
                          (user-email sql-user) (user-password sql-user)
                          title begin end (resort-infos-name sql-resort-infos)
                          note content public-p tags)))
          (lift:ensure (string-equal title (event-title sql-event)))
          (lift:ensure (string-equal begin (event-date-begin sql-event)))
          (lift:ensure (string-equal end (event-date-end sql-event)))
          (lift:ensure (= note (event-note sql-event))))))))

