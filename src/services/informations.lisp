;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          informations.lisp
;;;; Purpose:       Retreive informations about ski resorts.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2008 by Nicolas Lamirault
;;;;
;;;; ovorost users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(in-package :ovorost-services)


(defun parse-ski-info (uri)
  "Do a http request to a ski resort informations, parse the html page and
extract informations about slopes, snow and avalanche risk.
`URI' is a HTTP link.
Return a list which contains :
- a list of slopes informations : green, blue, red and black.
- a list of snow informations
- an URI to the avalanche risk."
  (let* ((str (drakma:http-request uri))
         (document (chtml:parse str (cxml-stp:make-builder)))
         (snow '())
         green blue red black avalanche-risk)
    (stp:do-recursively (a document)
      (cond 
        ;; ((and (typep a 'stp:element)
;;               (equal (stp:local-name a) "td")
;;               (or
;;                (cl-ppcre:scan "s4_slope_" (stp:attribute-value a "class"))
;;                (equal (stp:attribute-value a "class") "difficulty")))
;;          (progn
;;            (format t "~&HTML=~A" (stp:list-children a))
;;            (cond ((string-equal (stp:attribute-value a "class")
;;                                 "green")
;;                   (setf green (stp:string-value a)))
;;                  ((string-equal (stp:attribute-value a "class")
;;                                 "blue")
;;                   (setf blue (stp:string-value a)))
;;                  ((string-equal (stp:attribute-value a "class")
;;                                 "red")
;;                   (setf red (stp:string-value a)))
;;                  ((string-equal (stp:attribute-value a "class")
;;                                 "black")
;;                   (setf black (stp:string-value a)))
;;                  ((string-equal (stp:attribute-value a "class") "s4_data")
;;                   (when (and (string-equal (stp:attribute-value a "width")
;;                                            "20%")
;;                              (string-equal (stp:attribute-value a "align")
;;                                            "center"))
;;                     (when (and (string-not-equal (stp:string-value a) "")
;;                                (not (cl-ppcre:scan "km/h" (stp:string-value a))))
;;                       (push (stp:string-value a) snow))))
;;                  (t (warn "No slopes for ~A:~A~%"
;;                           (stp:attribute-value a "class")
;;                           (stp:string-value a))))))

        ;; Slopes
        ((and (typep a 'stp:element)
              (equal (stp:attribute-value a "class") "green")
              (cl-ppcre:scan "/" (stp:string-value a)))
         (setf green (stp:string-value a)))
        ((and (typep a 'stp:element)
              (equal (stp:attribute-value a "class") "blue")
              (cl-ppcre:scan "/" (stp:string-value a)))
         (setf blue (stp:string-value a)))
        ((and (typep a 'stp:element)
              (equal (stp:attribute-value a "class") "red")
              (cl-ppcre:scan "/" (stp:string-value a)))
         (setf red (stp:string-value a)))
        ((and (typep a 'stp:element)
              (equal (stp:attribute-value a "class") "black")
              (cl-ppcre:scan "/" (stp:string-value a)))
         (setf black (stp:string-value a)))

        ;; Snow
        ((and (typep a 'stp:element)
              (equal (stp:local-name a) "td"))
         (when (and (cl-ppcre:scan "cm" (stp:string-value a))
                    (not (cl-ppcre:scan "Total" (stp:string-value a))))
                    ;;(cl-ppcre:scan "/" (stp:string-value a)))
           (push (trim (stp:string-value a)) snow)))

        ;; Avalanche risk
;;;         ((and (typep a 'stp:element)
;;;               (equal (stp:local-name a) "img")
;;;               (search "weather/avalanche_risk"
;;;                       (stp:attribute-value a "src")))
;;;          (setf avalanche-risk (stp:attribute-value a "src")))))
        ((and (typep a 'stp:element)
              (equal (stp:local-name a) "span")
              (cl-ppcre:scan "1-|2-|3-|4-|5-" (stp:attribute-value a "class")))
         (format t "~&SNOW ? ~A" (stp:string-value a))
         (setf avalanche-risk (first (cl-ppcre:split "/" (trim (stp:string-value a))))))))

    (list (list green blue red black)
          (reverse snow)
          ;;(car (last (cl-ppcre:split "/" avalanche-risk))))))
          avalanche-risk)))
