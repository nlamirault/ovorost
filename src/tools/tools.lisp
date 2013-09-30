;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tools.lisp
;;;; Purpose:       Some tools for Ovorost
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************

(in-package :ovorost-tools)


(defun directory-p (dir)
  "Predicat for directory"
  (string-equal (directory-namestring dir) dir))


(defun list-directory (dir)
  "List content of a directory"
  (let ((wildcard (make-pathname :name :wild
                                 :type :wild
                                 :defaults dir)))
    (directory wildcard)))


(defun get-ovorost-directory ()
  "Get Ovorost configuration directory."
  (let ((directory (concatenate 'string
                                (sb-ext:posix-getenv "HOME")
                                "/"
                                ".ovorost/")))
    (ensure-directories-exist directory)
    directory))



(defun trim (sequence)
  "Delete some characters : #\Space, #\Tab and #\Newline."
  (string-trim '(#\Space #\Tab #\Newline) sequence))
  


(defun print-date (stream)
  "Print to STREAM a string representation of current date."
  (let ((day-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
                     "Saturday" "Sunday")))
    (multiple-value-bind
          (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (declare (ignore dst-p))
      (format stream "~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d)"
              hour
              minute
              second
              (nth day-of-week day-names)
              month
              date
              year
              (- tz)))))


;; From Hunchentoot
(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))


(defun occurrences (list &key (test #'eql) (key #'car))
  (loop with map = '()
     for element in list
     as count = (find element map :key key :test test)
     if count do (incf (cdr count))
     else do (push (cons element 1) map)
     finally (return map)))
