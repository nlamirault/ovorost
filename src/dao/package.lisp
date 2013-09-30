;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition file for Ovorost DAO package.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(defpackage :ovorost-dao
  (:use :cl :ovorost-tools)
  (:documentation "Ovorost DAO system.")
  (:export #:backend
           #:open-backend
           #:close-backend
           #:make-sql-backend

           #:create-sql-database
           
           ;; API
           
           #:add-infos
           #:get-infos

           #:print-user
           #:user
           #:user-id
           #:user-first-name
           #:user-last-name
           #:user-email
           #:user-password
           #:user-longitude
           #:user-latitude
           #:user-events
           #:user-sex
           #:user-active-p
           #:user-administrator-p
           #:get-users
           #:find-user
           #:get-user
           #:add-user
           #:update-user
           #:delete-user

           #:country
           #:country-name
           #:country-regions
           #:get-countries
           #:find-country
           #:add-country
           #:delete-country
           
           #:region
           #:region-name
           #:region-resorts
           #:find-region
           #:add-region
           ;;#:delete-region

           #:resort-infos
           #:resort-infos-name
           #:resort-infos-longitude
           #:resort-infos-latitude
           #:resort-infos-url
           #:resort-infos-slopes
           #:resort-infos-snow
           #:resort-infos-avalanche
           #:resort-infos-updated
           #:get-resorts-infos
           #:add-resort-infos
           #:update-resort-infos
           ;;#:delete-resort-infos
           #:find-resort-infos
           #:print-resort-infos

           #:event
           #:event-id
           #:event-title
           #:event-date
           #:event-date-begin
           #:event-date-end
           #:event-note
           #:event-tags
           #:event-content
           #:event-public-p
           #:event-resort-infos
           #:add-event
           #:get-event
           #:get-event-from-id
           #:delete-event
           #:get-events-tag
           #:find-event
           #:print-event
           
           #:add-news
           #:get-all-news

           #:tag
           #:tag-name

           ;; conditions

           #:backend-error
           
           #:unknown-country-error
           #:existing-country-error

           #:unknown-region-error
           #:existing-region-error

           #:unknown-user-error
           #:existing-user-error

           #:unknown-resort-error
           #:existing-resort-error

           #:unknown-event-error
           #:existing-event-error
           


           #:*print-ovorost-dao*

           ))

