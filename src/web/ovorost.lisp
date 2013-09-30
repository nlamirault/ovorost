;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ovorost.lisp
;;;; Purpose:       Ski resorts using google maps.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)



(defclass ovorost ()
  ((config :initform nil
           :initarg :config
           :accessor config-of
           :documentation "The configuration.")
   (backend :initform nil
            :initarg :backend
            :accessor backend-of
            :documentation "The database backend.")
   (server :initform nil
           :initarg :server
           :accessor server-of
           :documentation "The web server."))
  (:documentation "The Ovorost web site."))



(defun make-ovorost (environment)
  "Creates a new Ovorost system."
  (let ((ovorost (make-instance 'ovorost)))
    (with-slots (config backend server) ovorost
      (setf config
            (load-configuration (concatenate 'string
                                             *directory*
                                             "/etc/environments/"
                                             (string-downcase (symbol-name environment))
                                             "/ovorost.conf")))
      (destructuring-bind (host base login password)
          (configuration-sql config)
        (setf backend
              (make-sql-backend host base login password :postgresql)))
      ;;(load-resorts)))
      )
    ovorost))


(defgeneric setup (ovorost &optional new-sql-connection)
  (:documentation "Creates the SQL schema, initialize countries and ski resorts."))


(defmethod setup ((ovorost ovorost) &optional new-sql-connection)
  (with-slots (backend) ovorost
    (when new-sql-connection
      (open-backend backend))
    ;;(open-backend backend)
    (create-sql-database)
    (set-up-resorts backend)
    (when new-sql-connection
      (close-backend backend))))


(defgeneric start (ovorost)
  (:documentation "Start the web site."))


(defmethod start ((ovorost ovorost))
  (setf *ovorost* ovorost)
  (with-slots (config backend server) *ovorost*
    (let ((html-dir (concatenate 'string *directory* *templates-directory*)))
      (setf html-template:*default-template-pathname* html-dir)
;;       (when update
;;         (update-ovorost backend))
      (setf hunchentoot:*default-handler* 'handle-default
            hunchentoot:*http-error-handler* 'handle-error
            hunchentoot:*dispatch-table*
            (nconc
             (mapcar (lambda (args)
                       (apply #'hunchentoot:create-regex-dispatcher args))
                     '(("^/index" handle-index)
                       ("^/about" handle-about)
                       ("^/dev" handle-api)
                       ("^/news" handle-news)
                       ("^/tags" handle-tags)
                       ("^/account" handle-account)
                       ("^/events" handle-events)
                       ("^/add-event" handle-add-event)
                       ("^/show-event" handle-show-event)
                       ("^/login" handle-login)
                       ("^/logout" handle-logout)
                       ("^/identify" handle-identify)
                       ("^/sign-up" handle-sign-up)
                       ("^/add-user" handle-add-user)
                       ("^/admin" handle-admin)
                       ("^/update-user" handle-update-user)
                       ("^/language" handle-language)
                       ;; Feeds
                       ("^/feeds/news" handle-news-feed)
                       ("^/feeds/user" handle-user-feed)
                       ;; API
                       ("^/api/version" api-version)
                       ("^/api/news" api-news)
                       ("^/api/resorts" api-resorts-infos)
                       ("^/api/user" api-user-infos)
                       ("^/api/event" api-user-events)
                       ;; Default
                       ("^/$" handle-index)
                       ))
             (list (hunchentoot:create-folder-dispatcher-and-handler
                    "/css/"
                    (make-pathname :defaults (concatenate 'string
                                                          *directory*
                                                          *stylesheets-directory*))))
             (list (hunchentoot:create-folder-dispatcher-and-handler
                    "/js/"
                    (make-pathname :defaults (concatenate 'string
                                                          *directory*
                                                          *javascript-directory*))))
             (list (hunchentoot:create-folder-dispatcher-and-handler
                    "/img/"
                    (make-pathname :defaults (concatenate 'string
                                                          *directory*
                                                          *images-directory*))))
             (ovorost-ajax:install-hunchentoot-ajax-handler :name "/")
             (list #'hunchentoot:default-dispatcher)))
      (open-backend backend)
      (setf *resorts* (get-up-resorts backend)
            *translation* (init-i18n *languages*))
      (handler-case
          (progn
            (hunchentoot:log-message :info "Start Ovorost ~A"
                                     (configuration-environment config))
            (setf server
                  (hunchentoot:start-server :port (configuration-port config)
                                            :mod-lisp-p (configuration-mod-lisp-p config))))
        (sb-bsd-sockets:address-in-use-error (condition)
          (error 'web-server-error :message condition))))))


(defgeneric stop (ovorost)
  (:documentation "Stop the web site."))


(defmethod stop ((ovorost ovorost))
  (with-slots (config backend server) ovorost
    (hunchentoot:log-message :info "Stop Ovorost ~A"
                             (configuration-environment config))
    (hunchentoot:stop-server server)
    (close-backend backend)))



(defgeneric update (ovorost &optional new-sql-connection)
  (:documentation "Update Ovorost ski resorts informations."))


(defmethod update ((ovorost ovorost) &optional new-sql-connection)
  (with-slots (backend) ovorost
    (when new-sql-connection
      (open-backend backend))
    (update-ovorost backend)
    (setf *resorts* (get-up-resorts backend))
    (when new-sql-connection
      (close-backend backend))))


(defgeneric reload-i18n (ovorost)
  (:documentation "Reload internationalization of Ovorost."))


(defmethod reload-i18n ((ovorost ovorost))
  (setf *translation* (init-i18n *languages*)))


(defgeneric reload-resorts (ovorost)
  (:documentation "Reload resorts informations from CSV files."))


(defmethod reload-resorts ((ovorost ovorost))
  (setf *resorts* (get-up-resorts (backend-of ovorost))))


