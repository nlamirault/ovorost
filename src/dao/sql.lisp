;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sql.lisp
;;;; Purpose:       Database system of Ovorost
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************



(in-package :ovorost-dao)



(defclass sql-db (backend)
  ((server :initform nil
           :initarg :server
           :accessor sql-db-server)
   (name :initform nil
         :initarg :name
         :accessor sql-db-name)
   (user :initform nil
         :initarg :user
         :accessor sql-db-user)
   (password :initform nil
             :initarg :password
             :accessor sql-db-password)
   (database-type :initform nil
                  :initarg :database-type
                  :accessor sql-db-database-type))
  (:documentation "SQL backend."))



(defun make-sql-backend (server name user password database-type)
  (make-instance 'sql-db
                 :server server :name name :user user
                 :password password :database-type database-type))



(defmethod open-backend ((sql-db sql-db) &optional (sql-recording-p t))
  (with-slots (server name user password database-type) sql-db
    (case database-type
      ((:mysql :postgresql :postgresql-socket)
       (clsql:connect `(,server ,name ,user ,password)
                      :database-type database-type))
      )
    (setf clsql:*default-caching* nil)
    (when sql-recording-p
      (clsql:start-sql-recording))))


(defmethod close-backend ((sql-db sql-db))
  (clsql:disconnect)) ; :database sql-db))


#.(clsql:enable-sql-reader-syntax)


(defun create-country-sequence ()
  (clsql:create-sequence "seq_country"))


(defun drop-country-sequence ()
  (clsql:drop-sequence "seq_country" :if-does-not-exist :ignore))


(defun get-next-country-id ()
  (caar (clsql:query (format nil "SELECT NEXTVAL('seq_country')"))))


(clsql:def-view-class country ()
  ((countryid :db-kind :key
              :type integer
              :db-constraints :not-null
              :db-type "SERIAL"
              :accessor country-id
              :initarg :countryid)
   (name :type (string 70)
         :accessor country-name
         :initarg :name)
   (regions :reader country-regions
            :db-kind :join
            :db-info (:join-class region
                                  :home-key countryid
                                  :foreign-key countryid
                                  :set t))
   (resort-infos :reader country-resort-infos
                  :db-kind :join
                  :db-info (:join-class resort-infos
                                        :home-key countryid
                                        :foreign-key countryid
                                        :set t)))
  (:base-table country))



(defmethod print-object ((country country) stream)
  (if *print-ovorost-dao*
      (with-slots (name regions) country
        (format stream "~&Country ~A" name)
        (loop for region in regions
           do (print-object region stream)))
      (print-unreadable-object (country stream :type t :identity t))))


(defun create-region-sequence ()
  (clsql:create-sequence "seq_region"))


(defun drop-region-sequence ()
  (clsql:drop-sequence "seq_region" :if-does-not-exist :ignore))


(defun get-next-region-id ()
  (caar (clsql:query (format nil "SELECT NEXTVAL('seq_region')"))))


(clsql:def-view-class region ()
  ((regionid :db-kind :key
             :db-constraints :not-null
             :db-type "SERIAL"
             :type integer
             :accessor region-id
             :initarg :regionid)
   (name :type (string 70)
         :accessor region-name
         :initarg :name)
   (countryid :type integer
              :initarg :countryid)
   (resorts :reader region-resorts
            :db-kind :join
            :db-info (:join-class resort-infos
                                  :home-key regionid
                                  :foreign-key regionid
                                  :set t))
   )
  (:base-table region))


(defmethod print-object ((region region) stream)
  (if *print-ovorost-dao*
      (with-slots (name resorts) region
        (format stream "~&Region ~A" name)
        (loop for resort-info in resorts
           do (print-object resort-info stream)))
      (print-unreadable-object (region stream :type t :identity t))))


(defun create-resort-infos-sequence ()
  (clsql:create-sequence "seq_resort_infos"))


(defun drop-resort-infos-sequence ()
  (clsql:drop-sequence "seq_resort_infos" :if-does-not-exist :ignore))


(defun get-next-resort-infos-id ()
  (caar (clsql:query (format nil "SELECT NEXTVAL('seq_resort_infos')"))))


(clsql:def-view-class resort-infos ()
  ((resort-infos-id :db-kind :key
                    :db-constraints :not-null
                    :db-type "SERIAL"
                    :type integer
                    :accessor resort-infos-id
                    :initarg :resort-infos-id)
   (name :accessor resort-infos-name
         :type (string 100)
         :initarg :name)
   (url :accessor resort-infos-url
        :type (string 255)
        :initarg :url)
   (longitude :accessor resort-infos-longitude
              :type (string 20)
              :initarg :longitude)
   (latitude :accessor resort-infos-latitude
             :type (string 20)
             :initarg :latitude)
   (slopes :accessor resort-infos-slopes
           :type (string 255)
           :initarg :slopes)
   (snow :accessor resort-infos-snow
         :type (string 255)
         :initarg :snow)
   (avalanche :accessor resort-infos-avalanche
              :type (string 255)
              :initarg :avalanche)
   (updated :accessor resort-infos-updated
            :type (string 50)
            :initarg :updated)
   (countryid :type integer
              :accessor resort-infos-countryid
              :initarg :countryid)
   (country :accessor resort-country
            :db-kind :join
            :db-info (:join-class country
                                  :home-key countryid
                                  :foreign-key countryid
                                  :set nil))
   (regionid :type integer
             :accessor resort-regionid
             :initarg :regionid)
   (region :accessor resort-region
           :db-kind :join
           :db-info (:join-class region
                                 :home-key regionid
                                 :foreign-key regionid
                                 :set nil)))
  (:base-table resort-infos))


(defmethod print-object ((resort-infos resort-infos) stream)
  (if *print-ovorost-dao*
      (with-slots (name url longitude latitude) resort-infos
        (format stream "~&Resort ~A ~A ~A/~A"
                name url latitude longitude))
      (print-unreadable-object (resort-infos stream :type t :identity t))))


(defun create-event-sequence ()
  (clsql:create-sequence "seq_event"))


(defun drop-event-sequence ()
  (clsql:drop-sequence "seq_event" :if-does-not-exist :ignore))


(defun get-next-event-id ()
  (caar (clsql:query (format nil "SELECT NEXTVAL('seq_event')"))))


(clsql:def-view-class event ()
  ((eventid :db-kind :key
            :db-constraints :not-null
            :db-type "SERIAL"
            :type integer
            :accessor event-id
            :initarg :eventid)
   (title :type (string 150)
          :accessor event-title
          :initarg :title)
   (date :type (string 50)
         :initarg :date
         :accessor event-date)
   (date-begin :type (string 20)
               :accessor event-date-begin
               :initarg :date-begin)
   (date-end :type (string 20)
             :accessor event-date-end
             :initarg :date-end)
   (userid :type integer
           :initarg :userid)
   (resort-infos-id :type integer
                    :initarg :resort-infos-id)
   (note :type integer
         :accessor event-note
         :initarg :note)
   (content :type string
            :db-type "TEXT"
            :accessor event-content
            :initarg :content)
   (public-p :accessor event-public-p
             :type boolean
             :initform nil
             :initarg :public-p)
   (tags :reader event-tags
         :db-kind :join
         :db-info (:join-class tag
                   :home-key eventid
                   :foreign-key eventid
                   :set t))
   (resort-infos :reader event-resort-infos
                 :db-kind :join
                 :db-info (:join-class resort-infos
                           :home-key resort-infos-id
                           :foreign-key resort-infos-id
                           :set nil)))
  (:base-table ski-events))


(defmethod print-object ((event event) stream)
  (if *print-ovorost-dao*
      (with-slots (title date-begin date-end note tags content public) event
        (format stream "~&Event ~A ~A:~A ~A : ~A ~A~&"
                title date-begin date-end note content public)
        (loop for tag in tags
           do (print-object tag stream)))
      (print-unreadable-object (event stream :type t :identity t))))


(defun create-user-sequence ()
  (clsql:create-sequence "seq_user"))


(defun drop-user-sequence ()
  (clsql:drop-sequence "seq_user" :if-does-not-exist :ignore))


(defun get-next-user-id ()
  (caar (clsql:query (format nil "SELECT NEXTVAL('seq_user')"))))


(clsql:def-view-class user ()
  ((userid :db-kind :key
           :db-constraints :not-null
           :db-type "SERIAL"
           :type integer
           :accessor user-id
           :initarg :userid)
   (first-name :accessor user-first-name
               :type (string 30)
               :initarg :first-name)
   (last-name :accessor user-last-name
              :type (string 30)
              :initarg :last-name)
   (email :accessor user-email
          :type (string 100)
          :initarg :email)
   (password :accessor user-password
             :type (string 100)
             :initarg :password)
   (longitude :accessor user-longitude
              :type (string 20)
              :initarg :longitude)
   (latitude :accessor user-latitude
             :type (string 20)
             :initarg :latitude)
   (sex :accessor user-sex
        :initarg :sex
        :type (string 1))
   (active-p :accessor user-active-p
             :type boolean
             :initform nil
             :initarg :active-p)
   (administrator-p :accessor user-administrator-p
                    :type boolean
                    :initform nil
                    :initarg :administrator-p)
   (events :reader user-events
           :db-kind :join
           :db-info (:join-class event
                                 :home-key userid
                                 :foreign-key userid
                                 :set t)))
  (:base-table users))


(defmethod print-object ((user user) stream)
  (if *print-ovorost-dao*
      (with-slots (userid first-name last-name email password longitude latitude events) user
        (format stream "~&User : ~A~%~A ~A <~A> ~A~%~A , ~A"
                userid (trim first-name) (trim last-name) (trim email)
                (trim password) (trim latitude) (trim longitude)))
      (print-unreadable-object (user stream :type t :identity t))))


(defun create-resort-sequence ()
  (clsql:create-sequence "seq_resort"))


(defun drop-resort-sequence ()
  (clsql:drop-sequence "seq_resort" :if-does-not-exist :ignore))


(defun get-next-resort-id ()
  (caar (clsql:query (format nil "SELECT NEXTVAL('seq_resort')"))))


(clsql:def-view-class resort ()
  ((resortid :db-kind :key
             :db-constraints :not-null
             :db-type "SERIAL"
             :type integer
             :initarg :resortid)
   (infos-id :accessor resort-infos-id
             :initarg :infos-id)
   (resort-infos :accessor resort-infos
                 :db-kind :join
                 :db-info (:join-class resort-infos
                                       :home-key infos-id
                                       :foreign-key resortid
                                       :set t))    
   (lower-part :type (string 10)
               :accessor resort-lower-part
               :initarg :lower-part)
   (higher-part :type (string 10)
                :accessor resort-higher-part
                :initarg :higher-part)
   (temperature :type (string 10)
                :accessor resort-temperature
                :initarg :temperature)
   (lifts :type (string 40)
          :accessor resort-lifts
          :initarg :lifts)
   (slopes :type (string 40)
           :accessor resort-slopes
           :initarg :slopes)
   (difficulties :type (string 40)
                 :accessor resort-difficulties
                 :initarg :difficulties)
   (updated :type (string 150)
            :accessor resort-updated
            :initarg :updated)
   )
  (:base-table resort))



(defun create-tag-sequence ()
  (clsql:create-sequence "seq_tag"))


(defun drop-tag-sequence ()
  (clsql:drop-sequence "seq_tag" :if-does-not-exist :ignore))


(defun get-next-tag-id ()
  (caar (clsql:query (format nil "SELECT NEXTVAL('seq_tag')"))))


(clsql:def-view-class tag ()
  ((tagid :db-kind :key
          :db-constraints :not-null
          :db-type "SERIAL"
          :type integer
          :initarg :tagid)
   (name :type (string 50)
         :initarg :name
         :accessor tag-name)
   (eventid :type integer
            :accessor tag-eventid
            :initarg :eventid))
  (:base-table tag))


(defmethod print-object ((tag tag) stream)
  (if *print-ovorost-dao*
      (with-slots (name) tag
        (format stream "~&Tag ~A" name))
      (print-unreadable-object (tag stream :type t :identity t))))


(clsql:def-view-class news ()
  ((newsid :db-kind :key
           :db-constraints :not-null
           :db-type "SERIAL"
           :type integer
           :initarg :newsid)
   (title :type (string 50)
          :initarg :title
           :accessor news-title)
   (date :type (string 50)
         :initarg :date
         :accessor news-date)
   (content :type string
            :db-type "TEXT"
            :accessor news-content
            :initarg :content))
  (:base-table news))


(defun create-news-sequence ()
  (clsql:create-sequence "seq_news"))


(defun drop-news-sequence ()
  (clsql:drop-sequence "seq_news" :if-does-not-exist :ignore))


(defun get-next-news-id ()
  (caar (clsql:query (format nil "SELECT NEXTVAL('seq_news')"))))


(defmethod print-object ((news news) stream)
  (if *print-ovorost-dao*
      (with-slots (title date content) news
        (format stream "~&~A [~A]~&~A" title date content))
      (print-unreadable-object (news stream :type t :identity t))))


;; -----
;; Init
;; -----



(defun init-sql-model ()
  (format t "~&Drop tables ...")
  (clsql:drop-view-from-class 'country)
  (clsql:drop-view-from-class 'resort)
  (clsql:drop-view-from-class 'resort-infos)
  (clsql:drop-view-from-class 'user)
  (clsql:drop-view-from-class 'event)
  (clsql:drop-view-from-class 'region)
  (clsql:drop-view-from-class 'tag)
  (clsql:drop-view-from-class 'news)
  
  (format t "~&Drop sequences ...")
  (drop-country-sequence)
  (drop-resort-sequence)
  (drop-resort-infos-sequence)
  (drop-user-sequence)
  (drop-event-sequence)
  (drop-region-sequence)
  (drop-tag-sequence)
  (drop-news-sequence)

  (format t "~&Creates tables ...")
  (clsql:create-view-from-class 'country)
  (clsql:create-view-from-class 'resort)
  (clsql:create-view-from-class 'resort-infos)
  (clsql:create-view-from-class 'user)
  (clsql:create-view-from-class 'event)
  (clsql:create-view-from-class 'region)
  (clsql:create-view-from-class 'tag)
  (clsql:create-view-from-class 'news)

  (format t "~&Creates sequences ...")
  (create-country-sequence)
  (create-resort-sequence)
  (create-resort-infos-sequence)
  (create-user-sequence)
  (create-event-sequence)
  (create-region-sequence)
  (create-tag-sequence)
  (create-news-sequence)
  )
  

(defun create-sql-database (&optional ignore-errors-p)
  "Creates SQL model."
  (if ignore-errors-p
      (ignore-errors
        (init-sql-model))
      (init-sql-model)))


;; ----
;; API
;; ----


(defmethod get-countries ((sql-db sql-db))
  (mapcar #'car (clsql:select 'country)))



(defmethod find-country ((sql-db sql-db) name)
  (caar (clsql:select 'country :where [= [slot-value 'country 'name]
                                         name])))


(defmethod add-country ((sql-db sql-db) name)
  (format t "~&Add Country ~A" name)
  (let ((country (find-country sql-db name)))
    (if country
        (error 'existing-country-error :name name)
        (progn
          (setf country (make-instance 'country
                                       :countryid (get-next-country-id)
                                       :name name))
          (clsql:update-records-from-instance country)
          country))))


(defmethod delete-country ((sql-db sql-db) name)
  (clsql:delete-records :from [country]
                        :where [= [slot-value 'country 'name]
                                   name]))


(defmethod find-region ((sql-db sql-db) name)
  (caar (clsql:select 'region :where [= [slot-value 'region 'name]
                                         name])))


(defmethod add-region ((sql-db sql-db) region-name country-name)
  (format t "Add region ~A ~A ~%" region-name country-name)
  (let ((country (find-country sql-db country-name)))
    (unless country
      (setf country (add-country sql-db country-name)))
    (let ((region (find-region sql-db region-name)))
      (if region
          (error 'existing-region-error :name region-name)
          (progn
            (setf region (make-instance 'region
                                        :regionid (get-next-region-id)
                                        :name region-name
                                        :countryid (country-id country)))
            (clsql:update-records-from-instance region)
            region)))))


(defmethod get-users ((sql-db sql-db))
  (mapcar #'car (clsql:select 'user)))



(defmethod find-user ((sql-db sql-db) email &key password active-p)
  (cond ((and email password active-p)
         (caar (clsql:select 'user :where [and [= [slot-value 'user 'email]
                                                   email]
                                               [= [slot-value 'user 'password]
                                                   password]
                                               [= [slot-value 'user 'active-p]
                                                   active-p]])))
        ((and email password)
         (caar (clsql:select 'user :where [and [= [slot-value 'user 'email]
                                                   email]
                                               [= [slot-value 'user 'password]
                                                   password]])))
        (t
         (caar (clsql:select 'user :where [= [slot-value 'user 'email]
                                              email])))))


(defmethod get-user ((sql-db sql-db) id)
  (caar (clsql:select 'user :where [and [= [slot-value 'user 'userid]
                                           id]])))


(defmethod add-user ((sql-db sql-db) email password first-name last-name
                     longitude latitude sex &key active-p administrator-p)
  (let ((user (find-user sql-db email)))
    (if user
        (error 'existing-user-error :email email)
        (progn
          (setf user (make-instance 'user
                                    :userid (get-next-user-id)
                                    :email email :password password
                                    :first-name first-name :last-name last-name
                                    :longitude longitude :latitude latitude
                                    :sex sex :active-p active-p
                                    :administrator-p administrator-p))
          (clsql:update-records-from-instance user)
          user))))


(defmethod update-user ((sql-db sql-db) user)
  (clsql:update-records-from-instance user)                                                                  
    user)


(defmethod delete-user ((sql-db sql-db) email password)
  (clsql:delete-records :from [users] :where [and [= [slot-value 'user 'email]
                                                     email]
                                                 [= [slot-value 'user 'password]
                                                     password]]))


(defmethod get-resorts-infos ((sql-db sql-db))
  (mapcar #'car (clsql:select 'resort-infos)))


(defmethod find-resort-infos ((sql-db sql-db) name)
  (caar (clsql:select 'resort-infos :where [= [slot-value 'resort-infos 'name]
                                               name])))


(defmethod add-resort-infos ((sql-db sql-db) name country-name region-name longitude latitude url)
  (format t "Add SQL resort ~A ~A ~A ~%"
          name country-name region-name); longitude latitude url)
  (let ((country (find-country sql-db country-name)))
    (unless country
      (setf country (add-country sql-db country-name)))
    (let ((region (find-region sql-db region-name)))
      (unless region
       (setf region (add-region sql-db region-name country-name)))
      (let ((resort (find-resort-infos sql-db name)))
        (if resort
            (error 'existing-resort-error :name name)
            (progn
              (setf resort (make-instance 'resort-infos
                                          :name name
                                          :countryid (country-id country)
                                          :regionid (region-id region)
                                          :longitude longitude
                                          :latitude latitude
                                          :url url))
              (clsql:update-records-from-instance resort)
              resort))))))


(defmethod update-resort-infos ((sql-db sql-db) name slopes snow avalanche updated)
  (let ((resort (find-resort-infos sql-db name)))
    (when resort
      (format t "Update ~A informations : ~A ~A ~A~%" name slopes snow avalanche)
      (setf (resort-infos-slopes resort) slopes
            (resort-infos-snow resort) snow
            (resort-infos-avalanche resort) avalanche
            (resort-infos-updated resort) updated)
      (clsql:update-records-from-instance resort)
      resort)))


(defmethod get-tags ((sql-db sql-db))
  (mapcar #'car (clsql:select 'tag)))


(defmethod add-tag ((sql-db sql-db) name event-id)
  (let ((tag (make-instance 'tag
                            :tagid (get-next-tag-id)
                            :name name
                            :eventid event-id)))
    (clsql:update-records-from-instance tag)
    tag))


(defmethod get-user-events ((sql-db sql-db) email password)
  (let ((user (find-user sql-db email :password password)))
    (when user
      (user-events user))))



(defmethod add-event ((sql-db sql-db) email password title begin end resort-name
                      note content public-p &optional tags)
  (let ((user (find-user sql-db email :password password))
        (resort (find-resort-infos sql-db resort-name)))
    (unless user
      (error 'unknown-user-error :email email))
    (unless resort
      (error 'unknown-resort-error :name resort-name))
    (unless (find resort-name tags :test #'string-equal)
      (push resort-name tags))
    (let ((event (make-instance 'event
                                :eventid (get-next-event-id)
                                :userid (user-id user)
                                :title title
                                :date (iso-time)
                                :date-begin begin
                                :date-end end
                                :resort-infos-id (resort-infos-id resort)
                                :content content
                                :public-p public-p
                                :note note)))
      (clsql:update-records-from-instance event)
      (loop for name in tags
           do (add-tag sql-db name (event-id event)))
      event)))


(defmethod get-event ((sql-db sql-db) title email password)
  (let ((user (find-user sql-db email :password password)))
    (when user
        (caar (clsql:select 'event :where [and [= [slot-value 'event 'userid]
                                                  (user-id user)]
                                               [= [slot-value 'event 'title]
                                                   title]])))))


(defmethod get-event-from-id ((sql-db sql-db) id)
  (caar (clsql:select 'event :where [and [= [slot-value 'event 'eventid]
                                             id]])))



(defmethod delete-event ((sql-db sql-db) title email password)
  (let ((user (find-user sql-db email :password password)))
    (when user
      (clsql:delete-records :from [event]
                            :where [and [= [slot-value 'event 'userid]
                                            (user-id user)]
                                        [= [slot-value 'event 'title]
                                            title]]))))


(defmethod get-events-tag ((sql-db sql-db) name)
  (let ((tags (mapcar #'car
                      (clsql:select 'tag :where [= [slot-value 'tag 'name]
                                                    name]))))
    (loop for tag in tags
       as event = (get-event-from-id sql-db (tag-eventid tag))
       when (event-public-p event)
       collect event)))


(defmethod add-news ((sql-db sql-db) title date content)
  (let ((news (make-instance 'news :newsid (get-next-news-id)
                                   :title title
                                   :date date
                                   :content content)))
    (clsql:update-records-from-instance news)
    news))


(defmethod get-all-news ((sql-db sql-db))
  (mapcar #'car (clsql:select 'news)))





#.(clsql:disable-sql-reader-syntax)