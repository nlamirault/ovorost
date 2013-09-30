;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          feed.lisp
;;;; Purpose:       Feeds for the Ovorost web site.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)



(defparameter *rss-news-feed*
  "<rss version=\"2.0\">
<channel>
<title> ~A ~A</title>
<link>~A</link>
<description>Ovorost news feed.</description>
<language>en-us</language>
<pubDate>~A</pubDate>
<lastBuildDate>~A</lastBuildDate>
<generator> Ovorost </generator>
<webMaster>ovorost-admin at ovorost.com</webMaster>")


(defparameter *rss-user-feed*
  "<rss version=\"2.0\">
<channel>
<title> ~A ~A ~A</title>
<link>~A</link>
<description>Ovorost user feed.</description>
<language>en-us</language>
<pubDate>~A</pubDate>
<lastBuildDate>~A</lastBuildDate>
<generator> Ovorost </generator>
<webMaster>ovorost-admin at ovorost.com</webMaster>")

(defparameter *rss-user-event*
  "~&<item><title>~A</title><link>~A</link><description>~A</description><pubDate>~A</pubDate><guid>~A</guid></item>")


(defun handle-news-feed ()
  "Publish news feed."
  (hunchentoot:log-message :error "Feed News")
  (let ((stream (hunchentoot:send-headers)))
    (format stream *rss-news-feed*
            "Ovorost" "news"
            "http://www.google.com"
            (iso-time) (iso-time))
    (format stream "~&</channel>~&</rss>")))
    

(defun handle-user-feed ()
  "Publish user feed."
  (let* ((email (hunchentoot:get-parameter "email")))
    (hunchentoot:log-message :error "Feed User Infos ~A" email)
    (let ((user (find-user (backend-of *ovorost*) email)))
      (hunchentoot:log-message :error "Find user ~A" (user-first-name user))
      (when user
        (let ((stream (hunchentoot:send-headers)))
          (format stream *rss-user-feed*
                  "Ovorost"
                  (trim (user-first-name user)) (trim (user-last-name user))
                  "http://www.google.com"
                  (iso-time) (iso-time))
          (loop for event in (user-events user)
             when (event-public-p event)
             do (format stream *rss-user-event*
                        (trim (event-title event))
                        (event-id event) ;;"http://www.google.com"
                        (trim (event-title event))
                        (iso-time) (event-id event))) ;"http://www.google.com"))
          (format stream "~&</channel>~&</rss>"))))))
