;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          i18n.lisp
;;;; Purpose:       Internationalizsation of ovorost
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 by Nicolas Lamirault
;;;;
;;;; *************************************************************************



(in-package :ovorost-web)



(defun init-i18n (languages)
  "Load translation files into a hash-table wich keys are the language code.
LANGUAGES is a list of language."
  (setf cl-i18n:*translation-file-root*
        (concatenate 'string *directory* "i18n/"))
  (let ((ht (make-hash-table :test #'equal)))
    (loop for lang in languages
       as data = (cl-i18n:load-language lang)
       do (let ((translation (make-hash-table :test #'equal)))
            (maphash #'(lambda (key value) 
                         (format t "Add ~A ~A~%" key value)
                         (setf (gethash key translation) value))
                     data)
            (setf (gethash lang ht) translation)))
    ht))


(defun translate (token language)
  "Translate TOKEN into LANGUAGE. If no translatation is found return TOKEN."
  (hunchentoot:log-message :error
                           "Search translation for ~A in ~A~%"
                           token language)
  (let ((ht (gethash language *translation*)))
    (if ht
        (let ((translated (cl-i18n:translate token ht)))
          (if translated
              translated
              token))
        (progn
          (warn "Found no translation.")
          token))))


