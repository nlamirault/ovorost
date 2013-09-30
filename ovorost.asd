;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ovorost.asd
;;;; Purpose:       ASDF definition for Ovorost
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 by Nicolas Lamirault
;;;;
;;;; *************************************************************************



(in-package :asdf)


(defsystem ovorost
    :name "ovorost"
    :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :maintainer "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
    :version "0.4-pre0"
    :licence "(c) Perave"
    :description "Ovorost : social web site about ski."
    :depends-on (:hunchentoot
                 :html-template
                 :cl-smtp
                 :cl-i18n ;; patchee le 28/02/2008
                 :drakma
                 :cl-ppcre
                 :clsql-postgresql
                 :cl-json
                 :closure-html
                 :cxml-stp
                 :cl-google-chart)
    :components
    ((:module :lib
              :components
              ((:file "ovorost-ajax")
               (:file "cl-gravatar")
               (:file "cl-recaptcha")
               ))
     (:module :src
              :depends-on (:lib)
              :components
              ((:module :tools
                        :components
                        ((:file "package")
                         (:file "condition" :depends-on ("package"))
                         (:file "tools" :depends-on ("package"))))
               (:module :config
                        :components
                        ((:file "package")
                         (:file "condition" :depends-on ("package"))
                         (:file "config" :depends-on ("condition")))
                        :depends-on (:tools))
               (:module :dao
                        :components
                        ((:file "package")
                         (:file "specials" :depends-on ("package"))
                         (:file "condition" :depends-on ("package"))
                         (:file "api" :depends-on ("condition"))
                         (:file "sql" :depends-on ("api" "specials")))
                        :depends-on (:config))
               (:module :services
                        :depends-on (:tools)
                        :components
                        ((:file "package")
                         (:file "specials" :depends-on ("package"))
                         (:file "informations" :depends-on ("package"))))
               (:module :web
                        :components
                        ((:file "package")
                         (:file "condition" :depends-on ("package"))
                         (:file "debug" :depends-on ("package"))
                         (:file "specials" :depends-on ("package"))
                         (:file "ajax" :depends-on ("specials"))
                         (:file "i18n" :depends-on ("specials"))
                         (:file "db" :depends-on ("specials"))
                         (:file "tools" :depends-on ("specials"))
                         (:file "api" :depends-on ("specials"))
                         (:file "feed" :depends-on ("specials"))
                         (:file "web" :depends-on ("i18n" "tools"))
                         (:file "ovorost" :depends-on ("ajax"
                                                      "debug"
                                                      "condition"
                                                      "db"
                                                      "web"
                                                      "api")))
                        :depends-on (:dao :services))
               ))))
