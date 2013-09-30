;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.lisp
;;;; Purpose:       Database API of Ovorost
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************



(in-package :ovorost-dao)



(defclass backend ()
  ()
  (:documentation "A store system."))


(defgeneric open-backend (backend &optional sql-recording-p)
  (:documentation "Open the database.
`BACKEND' specify a store system.
`SQL-RECORDING-P' is to start or not the SQL recording."))


(defgeneric close-backend (backend)
  (:documentation "Close the database.
`BACKEND' specify a store system."))


(defgeneric add-infos (backend infos)
  (:documentation "Add informations about all ski resorts for each country.
`BACKEND' specify a store system."))


(defgeneric get-infos (backend)
  (:documentation "Retreive all informations about resorts.
`BACKEND' specify a store system."))


;; Users


(defgeneric get-users (backend)
  (:documentation "Retrieve a list of all users.
`BACKEND' specify a store system."))


(defgeneric find-user (backend email &key password active-p)
  (:documentation "Search for an user identify by `EMAIL'.
`BACKEND' specify a store system.
`EMAIL', `PASSWORD' and `ACTIVE-P' are user's attributes."))


(defgeneric get-user (backend id)
  (:documentation "Retrieve an user by his identifiant.
`BACKEND' specify a store system.
`ID' is the user identification."))


(defgeneric add-user (backend email password first-name last-name
                              longitude latitude sex
                              &key active-p administrator-p)
  (:documentation "Create an user, identified by his EMAIL.
`BACKEND' specify a store system.
`EMAIL', `PASSWORD', `FIRST-NAME', `LAST-NAME' specify users informations.
`LONGITUDE' and `LATITUDE' is for geolocalization of the user.
`SEX' if Male or Female.
`ACTIVE-P' specify is this user is already active or not for the web
authentication.
`ADMINISTRATOR-P' specify is this user have 'administrator' role or not.
Throws an `EXISTING-USER-ERROR' if `EMAIL' is already in database."))


(defgeneric update-user (backend user)
  (:documentation "Update informations of an user.
`BACKEND' specify a store system.
`USER' : the user to update informations."))


(defgeneric delete-user (backend email password)
  (:documentation "Delete an user.
`BACKEND' specify a store system.
`EMAIL' and `PASSWORD' are used to find the user."))


(defgeneric print-user (backend user stream)
  (:documentation "Print USER informations to STREAM.
`BACKEND' specify a store system."))


;; Coutries


(defgeneric add-country (backend name)
  (:documentation "Creates a new country.
`BACKEND' specify a store system.
`NAME' specify the country's name.
Throws an `EXISTING-COUNTRY-ERROR' if `NAME' is already in database."))


(defgeneric find-country (backend name)
  (:documentation "Search for a country.
`BACKEND' specify a store system.
`NAME' is a country name."))


;; Regions


(defgeneric find-region (backend name)
  (:documentation "Retreive a region.
`BACKEND' specify a store system.                                                                             `NAME' is a country name.")) 


(defgeneric add-region (backend region-name country-name)
  (:documentation "Creates a new regions for a country.
`BACKEND' specify a store system.
`REGION-NAME' is the name of the new region.
`COUNTRY-NAME' is name of which country this region comes from.
Throws an `EXISTING-REGION-ERROR' if `REGION-NAME' is already in database."))


;; Resorts


(defgeneric get-resorts-infos (backend)
  (:documentation "Retrieve a list of all ski resorts informations.
`BACKEND' specify a store system"))


(defgeneric add-resort-infos (backend name country-name region-name longitude latitude url)
  (:documentation "Create a ski resort.
`BACKEND' specify a store system
`NAME' is the event's title.
`COUNTRY-NAME' is name of which country this region comes from.
`REGION-NAME' is the name of the new region.
`LONGITUDE' and `LATITUDE' specify geolocalization.
`URL' is a website when we could take informations.
Throws an `EXISTING-RESORT-ERROR' if `NAME' is already in database."))


(defgeneric update-resort-infos (backend name slopes snow avalanche updated)
  (:documentation "Update informations for a ski resort.
`BACKEND' specify a store system.
`NAME' is the resort's name.
`SLOPES' is a list of green, blue, red and black open slopes.
`SNOW' is a list of snow in different altitude.
`AVALANCHE' is name of the avalanche risk image.
`UPDATED' specify a timestamp of the update operation."))


;; (defgeneric delete-resort (backend name)
;;   (:documentation  "Delete the resort identified by NAME."))


(defgeneric find-resort-infos (backend name)
  (:documentation "Search informations for a ski resort.
`BACKEND' specify a store system.                                                                             `NAME' is the resort's name."))


(defgeneric print-resort-infos (backend resort stream type)
  (:documentation "Print RESORT informations to STREAM."))


;; Tags


(defgeneric add-tag (backend name event-id)
  (:documentation "Add a new tag.
`BACKEND' is a store system.
`NAME' is the new tag.
`EVENT-ID' is which event have this tag."))


;; Events


(defgeneric get-user-events (backend email password)
  (:documentation "Get a list of all user's events.
`BACKEND' is a store system.
`EMAIL' and `PASSWORD' is user's identification."))


(defgeneric add-event (backend email password title begin end resort-name
                               note content public-p &optional tags)
  (:documentation "Create a ski event owned by an user identified by EMAIL.
`BACKEND' is a store system.
`EMAIL' and `PASSWORD' is user's identification.
`TITLE' is the event title.
`BEGIN' and `END' is the date of the event.
`RESORT-NAME' is the ski resort.
`NOTE' corresponds to a notation of this event (between 0 and 10).
`CONTENT' is the text description.
`PUBLIC-P' specifiy if others users could read or not this event.
`TAGS' is a list of informations corresponding to this event.
Throws an `UNKNOWN-USER-ERROR' if there is no user identify by `EMAIL'
and `PASSWORD'.
Throws an `UNKNOWN-RESORT-ERROR' if `RESORT-NAME' is unknown."))


(defgeneric get-event (backend title email password)
  (:documentation "Get an user event.
`BACKEND' is a store system.
`TITLE' is the event's title.
`EMAIL' and `PASSWORD' is user's identification."))


(defgeneric get-event-from-id (backend id)
  (:documentation "Get an user event.
`BACKEND' is a store system.
`ID' is the event identifient."))


(defgeneric delete-event (backend title email password)
  (:documentation  "Delete the ski event.
`BACKEND' is a store system.
`TITLE' is the event's title
`EMAIL' and `PASSWORD' is user's identification."))


(defgeneric get-events-tag (backend name)
  (:documentation "Retrieve all public events which have a common tag.
`BACKEND' is a store system.
`NAME' is the tag title."))


(defgeneric find-event (backend name)
  (:documentation "Search for a ski event.
`BACKEND' is a store system
`NAME' is for searching an event title."))


(defgeneric print-event (backend event stream)
  (:documentation "Print EVENT informations to STREAM."))


;; News

(defgeneric add-news (backend title date content)
  (:documentation "Add a news.
`BACKEND' is a store system.
`TITLE' describe the news
`DATE' specify when this news is created.
`CONTENT' is the text description."))


(defgeneric get-all-news (backend)
  (:documentation "Retrieve a list of all news.
`BACKEND' is a store system."))