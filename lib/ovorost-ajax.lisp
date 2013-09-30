;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          hunchentoot.lisp
;;;; Purpose:       AJAX handler for HUNCHENTOOT.
;;;; Programmer:    nicolas lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file is Copyright (c) 2006 by nicolas lamirault
;;;;
;;;;  users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;
;;;; *************************************************************************


(defpackage :ovorost-ajax
  (:use :cl)
  (:documentation "Ajax tool for Hunchentoot. Based on cl-ajax.")
  (:export #:defexported
           #:install-hunchentoot-ajax-handler 
           ))


(in-package :ovorost-ajax)


;; --------------------------------------------------------------------
;; Stolen from cl-ajax 

(defun ajax-function-name (fn)
  ;; Thanks to kpreid for suggesting OR failover into f-l-e. 2005-09-21.
  (or 
    ; #+sbcl `(string (caddr (multiple-value-list (function-lambda-expression ,fn))))
    #+sbcl (sb-impl::%fun-name fn)
    #+armedbear (string (nth-value 2 (function-lambda-expression fn)))
    #+cmu (cmu-function-name fn)
    #+cormanlisp (handler-case (getf (cl::function-info-list fn) 'cl::function-name)
                    (error () nil))
    #+ecl (si:compiled-function-name fn)
    #+openmcl (ccl:function-name fn)
    #+allegro (string (xref::object-to-function-name fn))
    #+clisp (string (nth-value 2 (function-lambda-expression fn)))
    #+lispworks (let ((res (system::function-name fn)))
                   (if (null res)
                     (string (nth-value 2 (function-lambda-expression fn)))
                     (when res
                       (symbol-name res))))
    (string (nth-value 2 (function-lambda-expression fn)))))


;; Management of exported functions.
;; This version is a bit smarter:
;; - re-exporting will replace, not just append
;; - can unexport by symbol or string
(let ((exported-functions (make-hash-table :test 'equal)))

  (defun get-exported-functions ()
    "Access the hash of exported (i.e. allowed) functions."
    exported-functions)
  
  (defun unexport-function (fun)
    "Remove fun from the list of allowed functions.
     fun can either be the name of a function or the function itself."
    (remhash (string-upcase (if (stringp fun) fun (ajax-function-name fun)))
             exported-functions))

  (defun get-function (fun-name)
    "Return the function associated with the name, or nil for failure."
    (gethash (string-upcase fun-name) exported-functions))

  (defun unexport-all () 
    (clrhash exported-functions))

  (defun export-function (fun &optional (fun-name (if (functionp fun)
                                                    (ajax-function-name fun)
                                                    (symbol-name fun))))
    "Ensure that fun-name is mapped to fun in the allowed functions list."
    (setf (gethash (string-upcase fun-name) exported-functions)
          (if (functionp fun) fun (symbol-function fun))))

  ;; Updated 2005-03-15 to only export if the function has been successfully
  ;; defined.
  (defmacro defexported (name params &rest body)
    "Shorthand macro for defining a function to export."
    (let ((f (gensym)))
      `(let ((,f (defun ,name ,params ,@body)))
         (if ,f (export-function #',name))))))


(defun run-function (fun args keys)
  "Run the named exported function with the provided argument lists. Returns
   the result and whether the call was successful.
   E.g. (run-function #'format (t \"~A~%\" (+ 5 6)) nil)."
   ;(format t "Running: ~A~%~{~A ~}~%~{~A ~}~%" fun args keys) 
   (if fun
     (values (apply fun (append args keys)) t)
     (values nil nil)))


(defun parse-argument-list (arg-alist &optional (lookup-fun #'find-symbol))
  "Returns an argument keylist from an alist.
   Associations with strings beginning 'ajax-' are ordered by appearance
   and clustered into arg-list. All other associations are looked up by
   keyword and listed in key-list.
   An optional lookup function can be passed in -- usually #'intern, to
   force the creation of unknown keywords.
   Arguments are read using read-from-string, but are not evaluated unless
   the looked-up function does it deliberately."
  (let ((arg-list '())
        (key-list '()))
    (dolist (x arg-alist)
      (if (eq 5 (string< "ajax-" (car x))) 
        (setf arg-list 
              (nconc arg-list 
                     `(,(read-from-string (cadr x)))) )
        (setf key-list 
              (nconc key-list 
                     `(,(funcall 
                          lookup-fun 
                          (string-upcase (car x)) 
                          "KEYWORD") ,(read-from-string (cadr x)))))))
    (values arg-list key-list)))

;; --------------------------------------------------------------------


(defparameter *xml-response*
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>
 <response>~A<result xmlns=\"http://www.w3.org/1999/xhtml\">~A</result></response>")


(defun ajax-handler ()
  "Ajax Handler for HUNCHENTOOT."
  (let* ((fun-valid (get-function (hunchentoot:get-parameter "ajax-fun")))
         (return-xml (hunchentoot:get-parameter "ajax-xml"))
         (elem-id (hunchentoot:get-parameter "ajax-elem")))
;;     (hunchentoot:log-message :error (format nil "=> ~A"
;;                                      (hunchentoot:get-parameters)))
;;     (hunchentoot:log-message :error
;;                  (format nil "~A ~A ~A"
;;                          fun-valid return-xml elem-id))
    (when fun-valid
        (multiple-value-bind (args keys)
            (parse-argument-list
             (remove-if #'(lambda (x) 
                            (member (car x) 
                                    '("ajax-fun" "ajax-xml" "ajax-elem") 
                                    :test #'string=))
                        (mapcar (lambda (pair)
                                  (list (car pair) (cdr pair)))
                                (hunchentoot:get-parameters))))
          (when (string-equal "true" return-xml)
            (setf (hunchentoot:content-type) "text/xml; charset=iso-8859-1"))
          (hunchentoot:log-message :error
                                   (format nil "HUNCHENTOOT ~A"
                                           hunchentoot:*default-content-type*))
          (let ((stream (hunchentoot:send-headers)))
            (if (string-equal "true" return-xml)
                (progn
                  (hunchentoot:log-message :error "XML requis")
                  (format stream
                          *xml-response*
                          (if elem-id
                              (concatenate 'string "<elem_id>" elem-id "</elem_id>")
                              "")
                          (run-function fun-valid args keys)))
                (progn
                  (hunchentoot:log-message :error "XML non requis")
                  (format stream "~A"
                          (run-function fun-valid args keys))))
            (finish-output stream))))))


(defun install-hunchentoot-ajax-handler (&key (name "/hunchentoot/"))
  "Install AJAX handler for HUNCHENTOOT."
  (setf hunchentoot:*dispatch-table*
        (nconc
         (list (apply #'hunchentoot:create-prefix-dispatcher 
                      (list (concatenate 'string name "ajax-function/")
                            #'ajax-handler))))))

