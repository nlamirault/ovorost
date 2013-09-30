;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          web.lisp
;;;; Purpose:       Unit Test for Ovorost web site.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-test)


(lift:deftestsuite ovorost-web-test (ovorost-test)
  ((ovorost-ut (make-ovorost 'ut))
   (test-user nil))
  (:run-setup :once-per-suite)
  (:setup (progn
            (ovorost-web:setup ovorost-ut)
            (ovorost-web:start ovorost-ut)
            (destructuring-bind (email password first-name last-name lng lat sex)
                '("test-user@ovorost.com" "test" "first-name" "last-name" "12" "12" "M")
              (setf test-user
                    (add-user (backend-of ovorost-ut)
                              email password
                              first-name last-name lng lat sex)))
            (ovorost-web::active-user (user-email test-user)
                                      (user-password test-user))
            (sleep 5)))
  (:teardown (ovorost-web:stop ovorost-ut))
  (:documentation "Unit Test suite for Ovorost website."))
  


(defmacro with-selenium-ovorost (page &body body)
  "Macro with set selenium:*selenium-driver-url* and selenium:*selenium-session*
open page URL, executes BODY and close the browser."
  `(let* ((selenium:*selenium-driver-url*
           "http://localhost:4444/selenium-server/driver")
          (url (format nil "http://localhost:~A"
                       (configuration-port
                        (ovorost-web:config-of ovorost-ut))))
          (selenium:*selenium-session*
           (selenium:do-get-new-browser-session  "*firefox" url)))
     (selenium:do-open (format nil "~A/~A" url ,page))
     (unwind-protect
;;           (handler-case
;;               (progn ,@body)
;;             (selenium:execution-error (cond)
;;               (format t "~A" cond)))
          (progn
            ,@body)
       (selenium:do-test-complete))))



;;
;; To test selenium
;;

;; (lift:addtest (ovorost-web-test)
;;   google-test
;;   (:documentation "Test Selenium")
;;   (let* ((selenium:*selenium-driver-url* "http://localhost:4444/selenium-server/driver")
;;          (selenium:*selenium-session*
;;           (selenium:do-get-new-browser-session  "*firefox" "http://www.google.com")))
;;     (selenium:do-open "http://www.google.com/webhp?hl=en")
;;     (selenium:do-type "q" "hello world")
;;     (selenium:do-click "btnG")
;;     (selenium:do-wait-for-page-to-load "5000")
;;     (lift:ensure (string= (selenium:do-get-title) "hello world - Google Search"))))


;; (defmacro with-selenium-ovorost (page &body body)
;;   "Macro with set selenium:*selenium-driver-url* and selenium:*selenium-session*
;; open page URL, executes BODY and close the browser."
;;   `(let* ((selenium:*selenium-driver-url*
;;            "http://localhost:4444/selenium-server/driver")
;;           (url (format nil "http://localhost:~A"
;;                        (configuration-port
;;                         (ovorost-web:config-of ovorost-ut))))
;;           (selenium:*selenium-session*
;;            (selenium:do-get-new-browser-session  "*firefox" url)))
;;      (selenium:do-open (format nil "~A/~A" url ,page))
;;      (unwind-protect
;; ;;           (handler-case
;; ;;               (progn ,@body)
;; ;;             (selenium:execution-error (cond)
;; ;;               (format t "~A" cond)))
;;           (progn
;;             ,@body)
;;        (selenium:do-test-complete))))


(defun get-uri-name (uri)
  "Extract URI name : http://xx.xx.xx/name."
  (car (last (cl-ppcre:split "/" uri))))


;; unit tests for web site


(lift:addtest (ovorost-web-test)
  about-page
  (with-selenium-ovorost "about"
    (lift:ensure (selenium:do-is-element-present "header"))
    (lift:ensure (selenium:do-is-element-present "about"))
    (lift:ensure (selenium:do-is-element-present "stat"))
    (lift:ensure (selenium:do-is-element-present "footer"))
    (lift:ensure (selenium:do-is-element-present "l-menu-index"))
    (lift:ensure (selenium:do-is-element-present "l-menu-news"))
    (lift:ensure (selenium:do-is-element-present "l-menu-login"))
    (lift:ensure (selenium:do-is-element-present "l-menu-about"))
    (lift:ensure (string-equal "Ovorost" (selenium:do-get-title)))))


(lift:addtest (ovorost-web-test)
  index-page
  (with-selenium-ovorost "index"
    (lift:ensure (selenium:do-is-element-present "header"))
    (lift:ensure (selenium:do-is-element-present "footer"))
    (lift:ensure (selenium:do-is-element-present "regions"))
    (lift:ensure (selenium:do-is-element-present "map"))
    (lift:ensure (selenium:do-is-element-present "l-menu-index"))
    (lift:ensure (selenium:do-is-element-present "l-menu-news"))
    (lift:ensure (selenium:do-is-element-present "l-menu-login"))
    (lift:ensure (selenium:do-is-element-present "l-menu-about"))
    (lift:ensure (string-equal "Ovorost" (selenium:do-get-title)))))


(lift:addtest (ovorost-web-test)
  news-page
  (with-selenium-ovorost "news"
    (lift:ensure (selenium:do-is-element-present "header"))
    (lift:ensure (selenium:do-is-element-present "footer"))
    (lift:ensure (selenium:do-is-element-present "news"))
    (lift:ensure (selenium:do-is-element-present "l-menu-index"))
    (lift:ensure (selenium:do-is-element-present "l-menu-news"))
    (lift:ensure (selenium:do-is-element-present "l-menu-login"))
    (lift:ensure (selenium:do-is-element-present "l-menu-about"))
    (lift:ensure (string-equal "Ovorost" (selenium:do-get-title)))))


(lift:addtest (ovorost-web-test)
  login-page
  (with-selenium-ovorost "login"
    (lift:ensure (selenium:do-is-element-present "header"))
    (lift:ensure (selenium:do-is-element-present "footer"))
    (lift:ensure (selenium:do-is-element-present "formIdent"))
    (lift:ensure (selenium:do-is-element-present "sign"))
    (lift:ensure (selenium:do-is-element-present "l-menu-index"))
    (lift:ensure (selenium:do-is-element-present "l-menu-news"))
    (lift:ensure (selenium:do-is-element-present "l-menu-login"))
    (lift:ensure (selenium:do-is-element-present "l-menu-about"))
    (lift:ensure (string-equal "Ovorost" (selenium:do-get-title)))))


(lift:addtest (ovorost-web-test)
  logout-page
  (with-selenium-ovorost "login"
      (selenium:do-type "login" (user-email test-user))
      (selenium:do-type "password" (user-password test-user))
      (selenium:do-submit "identify")
      (selenium:do-wait-for-page-to-load "5000")
      (lift:ensure
       (string-equal (get-uri-name (selenium:do-get-location))
                     "index"))
      (lift:ensure (selenium:do-is-element-present "l-menu-index"))
      (lift:ensure (selenium:do-is-element-present "l-menu-news"))
      (lift:ensure (selenium:do-is-element-present "l-menu-logout"))
      (lift:ensure (selenium:do-is-element-present "l-menu-account"))
      (lift:ensure (selenium:do-is-element-present "l-menu-events"))
      (lift:ensure (selenium:do-is-element-present "l-menu-about"))
      (selenium:do-click "l-menu-logout")
      (selenium:do-wait-for-page-to-load "5000")
      (lift:ensure
       (string-equal (get-uri-name (selenium:do-get-location))
                     "index"))))


(lift:addtest (ovorost-web-test)
  account-page
  (with-selenium-ovorost "login"
      (selenium:do-type "login" (user-email test-user))
      (selenium:do-type "password" (user-password test-user))
      (selenium:do-submit "identify")
      (selenium:do-wait-for-page-to-load "5000")
      (selenium:do-click "l-menu-account")
      (selenium:do-wait-for-page-to-load "5000")
      (lift:ensure
       (string-equal (get-uri-name (selenium:do-get-location))
                     "account"))
      (lift:ensure (selenium:do-is-element-present "header"))
      (lift:ensure (selenium:do-is-element-present "footer"))
      (lift:ensure (selenium:do-is-element-present "account"))
      (lift:ensure (selenium:do-is-element-present "user-account"))
      (lift:ensure (selenium:do-is-element-present "user-account-gravatar"))
      (lift:ensure (selenium:do-is-element-present "events-tags"))
      (lift:ensure (selenium:do-is-element-present "user-stats"))
      (lift:ensure (selenium:do-is-element-present "l-menu-index"))
      (lift:ensure (selenium:do-is-element-present "l-menu-news"))
      (lift:ensure (selenium:do-is-element-present "l-menu-logout"))
      (lift:ensure (selenium:do-is-element-present "l-menu-account"))
      (lift:ensure (selenium:do-is-element-present "l-menu-events"))
      (lift:ensure (selenium:do-is-element-present "l-menu-about"))))


(lift:addtest (ovorost-web-test)
  account-admin-page
  (with-selenium-ovorost "login"
    (setf (user-active-p test-user) t
          (user-administrator-p test-user) t)
    (update-user (backend-of ovorost-ut) test-user)
    (selenium:do-type "login" (user-email test-user))
    (selenium:do-type "password" (user-password test-user))
    (selenium:do-submit "identify")
    (selenium:do-wait-for-page-to-load "5000")
    (selenium:do-click "l-menu-admin")
    (selenium:do-wait-for-page-to-load "5000")
    (lift:ensure
     (string-equal (get-uri-name (selenium:do-get-location))
                   "admin"))
    (lift:ensure (selenium:do-is-element-present "l-menu-index"))
    (lift:ensure (selenium:do-is-element-present "l-menu-news"))
    (lift:ensure (selenium:do-is-element-present "l-menu-logout"))
    (lift:ensure (selenium:do-is-element-present "l-menu-account"))
    (lift:ensure (selenium:do-is-element-present "l-menu-events"))
    (lift:ensure (selenium:do-is-element-present "l-menu-admin"))
    (lift:ensure (selenium:do-is-element-present "l-menu-about"))
    (lift:ensure (selenium:do-is-element-present "header"))
    (lift:ensure (selenium:do-is-element-present "footer"))
    (lift:ensure (selenium:do-is-element-present "admin"))
    (lift:ensure (selenium:do-is-element-present "admin-users"))
    (lift:ensure (selenium:do-is-element-present "admin-user"))
    (lift:ensure (selenium:do-is-element-present "admin-events"))
    (setf (user-active-p test-user) nil
          (user-administrator-p test-user) nil)
    (update-user (backend-of ovorost-ut) test-user)))


;; Problem : how test with recaptcha ??
(lift:addtest (ovorost-web-test)
  signup-page
  (with-selenium-ovorost "sign-up"
    (destructuring-bind (email password first-name last-name lng lat sex)
        (list (format nil "~A@ovorost.com" (gensym))
              (format nil "~A-unit-test" (gensym))
              "unit" "test" "40" "1" "M")
      (selenium:do-type "email" email)
      (selenium:do-type "password" password)
      (selenium:do-type "confirm-passwd" password)
      (selenium:do-type "first-name" first-name)
      (selenium:do-type "last-name" last-name)
      (selenium:do-type "longitude" lng)
      (selenium:do-type "latitude" lat)
      (selenium:do-type "sex" sex)
;;       (selenium:do-submit "add-user")
;;       (selenium:do-wait-for-page-to-load "5000")
;;       (lift:ensure
;;        (string-equal (get-uri-name (selenium:do-get-location))
;;                      "login"))
;;       (ovorost-web::active-user email password)
;;       (selenium:do-type "login" email)
;;       (selenium:do-type "password" password)
;;       (selenium:do-submit "identify")
;;       (selenium:do-wait-for-page-to-load "5000")
;;       (lift:ensure
;;        (string-equal (get-uri-name (selenium:do-get-location))
;;                      "index")))))
      )))


(lift:addtest (ovorost-web-test)
  event-page
  (with-selenium-ovorost "login"
    (setf (user-active-p test-user) t)
    (update-user (backend-of ovorost-ut) test-user)
    (selenium:do-type "login" (user-email test-user))
    (selenium:do-type "password" (user-password test-user))
    (selenium:do-submit "identify")
    (selenium:do-wait-for-page-to-load "5000")
    (selenium:do-click "l-menu-events")
    (selenium:do-wait-for-page-to-load "5000")
    (lift:ensure
     (string-equal (get-uri-name (selenium:do-get-location))
                   "events"))
    (lift:ensure (selenium:do-is-element-present "header"))
    (lift:ensure (selenium:do-is-element-present "footer"))
    (lift:ensure (selenium:do-is-element-present "events"))
    (lift:ensure (selenium:do-is-element-present "user-events"))
    (lift:ensure (selenium:do-is-element-present "l-menu-index"))
    (lift:ensure (selenium:do-is-element-present "l-menu-news"))
    (lift:ensure (selenium:do-is-element-present "l-menu-logout"))
    (lift:ensure (selenium:do-is-element-present "l-menu-account"))
    (lift:ensure (selenium:do-is-element-present "l-menu-events"))
    (lift:ensure (selenium:do-is-element-present "l-menu-about"))
    (destructuring-bind (title from to resort tags note content public-p)
        (list "unit test event"
              (first (cl-ppcre:split " " (ovorost-tools::iso-time)))
              (first (cl-ppcre:split " " (ovorost-tools::iso-time)))
              "Peyragudes"
              "unit tests event"
              "5" "An event for unit tests of Ovorost." "t")
      (selenium:do-type "title" title)
      (selenium:do-type "from" from)
      (selenium:do-type "to" to)
      (selenium:do-type "resort" resort)
      (selenium:do-type "tags" tags)
      (selenium:do-type "note" note)
      (selenium:do-type "content" content)
      (selenium:do-type "public-p" public-p)
      (selenium:do-submit "add-event")
      (selenium:do-wait-for-page-to-load "5000")
      (lift:ensure
       (string-equal (get-uri-name (selenium:do-get-location))
                     "events"))
      (lift:ensure (selenium:do-is-element-present "user-events")))
    (setf (user-active-p test-user) nil)))



(defun get-country-regions (country)
  "Get names of availables regions for a `COUNTRY'."
  (let ((data
         (loop for region in (list-directory 
                              (format nil "~A~A/~A/"
                                      ovorost-web::*directory*
                                      ovorost-web::*content-directory*
                                      country))
               collect (file-namestring region))))
    (mapcar #'(lambda (token)
                (cl-ppcre:regex-replace-all ".csv" token ""))
            data)))


(lift:addtest (ovorost-web-test)
  country-resorts-page
  (loop for country in ovorost-web::*countries*
     do (with-selenium-ovorost (format nil "index?country=~A" (first country))
          (loop for region in (get-country-regions (first country))
             do (lift:ensure (selenium:do-is-text-present region))))))
