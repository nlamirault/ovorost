;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          web.lisp
;;;; Purpose:       Web site of Ovorost.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; This file, part of ovorost, is Copyright (c) 2007, 2008 Perave,
;;;; by Nicolas Lamirault
;;;;
;;;; *************************************************************************


(in-package :ovorost-web)


;; -----
;; i18n
;; -----


(defun set-user-language (&optional lang)
  "Check the user's language and save into the session."
  (let ((locale
         (if lang
             lang
             (first (cl-ppcre:split "-"
                                    (hunchentoot:header-in "Accept-Language"))))))
    (unless locale
      (setf locale +default-locale+))
    (setf (hunchentoot:session-value 'locale) locale)))



(defun i18n (token)
  "Translate token into current language find by session variable."
  (translate token (hunchentoot:session-value 'locale)))


;; ------
;; Tools
;; ------


(defun get-user-from-session ()
  "Get an user or nil."
  (let ((data (hunchentoot:session-value 'user-id)))
    (when data
      (find-user (backend-of *ovorost*) (car data) :password (cdr data)))))


(defmacro with-user-identity ((ident) &body body)
  "Macro which get identification from session.
If there is no active session, redirect to the homepage, otherwise
executes BODY."
  `(let ((data (hunchentoot:session-value 'user-id))
         ,ident)
     (if (not (null data))
         (progn 
           (setf ,ident (find-user (backend-of *ovorost*)
                                   (car data) :password (cdr data)))
           ,@body)
         (hunchentoot:redirect "/index"))))


(defun make-web-page (path &rest variables)
  "Print the HTML template."
  (hunchentoot:log-message :error "Lang : ~A"
                           (hunchentoot:header-in "Accept-Language"))
  (unless (hunchentoot:session-value 'locale)
    (set-user-language))
  (let* ((user (get-user-from-session))
         (analytics (string-equal (configuration-environment (config-of *ovorost*))
                                  "prod"))
         (common (list :title *project*
                       :company "Perave"
                       :version *version*
                       :google_analytics analytics
                       :lang (hunchentoot:session-value 'locale)
                       :i18n_menu_index (i18n "Index")
                       :i18n_menu_news (i18n "News")
                       :i18n_menu_login (i18n "Login")
                       :i18n_menu_logout (i18n "Logout")
                       :i18n_menu_account (i18n "Account")
                       :i18n_menu_events (i18n "Events")
                       :i18n_menu_admin (i18n "Admin")
                       :i18n_menu_about (i18n "About")
                       :i18n_events (i18n "Events")
                       :admin (if user
                                  (user-administrator-p user)
                                  nil)
                       :ident (hunchentoot:session-value 'user-id))))
    (with-output-to-string (stream)
      (html-template:fill-and-print-template path
                                             (append common (car variables))
                                             :stream stream))))


;; ---------
;; Web site
;; ---------


(defun handle-about ()
  "Print about page."
  (hunchentoot:log-message :debug "Present about page")
  (let ((country-param (hunchentoot:get-parameter "country"))
        countries-resorts-uri country-uri)
    (with-output-to-string (os)
      (if country-param
          (graph-country-resorts country-param os)
          (graph-country-resorts "france" os))
      (setf country-uri (get-output-stream-string os)))
    (with-output-to-string (os)
        (countries-resorts-chart os)
        (setf countries-resorts-uri (get-output-stream-string os)))
    (make-web-page #p"about.html"
                   (list :i18n_about_text (i18n "About-Text")
                         :countries (loop for country in *countries*
                                          as name = (first country)
                                          as selected = (if (string-equal name country-param)
                                                          "selected"
                                                          "")
                                          collect (list :name name
                                                        :selected selected))   
                         :country_resorts country-uri ;;(cl-ppcre:regex-replace-all "&" country-uri "&amp;")
                         :countries_resorts countries-resorts-uri))))
                         ;;(cl-ppcre:regex-replace-all "&" countries-resorts-uri "&amp;")))))


(defun handle-default ()
  "Modify the Hunchentoot default dispatcher."
  (make-web-page #p"default.html"))


(defun handle-error (arg)
  "Modify the Hunchentoot error dispatcher."
  (declare (ignore arg))
  (make-web-page #p"error.html"
                 (list :message (format nil "~A"
                                        (hunchentoot:session-value 'error-msg))))
  (setf (hunchentoot:session-value 'error-msg) ""))


(defun handle-account ()
  "Print account page."
  (hunchentoot:log-message :debug "Present account page")
  (with-user-identity (user)
    (let (pie-events-uri)
      (with-output-to-string (os)
        (user-events-pie-chart user os)
        (setf pie-events-uri (get-output-stream-string os)))
      (with-accessors ((first-name user-first-name)
                       (last-name user-last-name)
                       (email user-email)
                       (latitude user-latitude)
                       (longitude user-longitude)) user
        (make-web-page #p"account.html"
                       (list :i18n_firstname (i18n "AccountPageFirstName")
                             :i18n_lastname (i18n "AccountPageLastName")
                             :i18n_email (i18n "AccountPageEmail")
                             :i18n_localization (i18n "AccountPageLocalization")
                             :i18n_gravatar (i18n "AccountPageGravatarText")
                             :first_name first-name
                             :last_name last-name
                             :email email
                             :latitude latitude
                             :longitude longitude
                             :gravatar (cl-gravatar:get-gravatar email)
                             :pie_events_uri pie-events-uri
                             :tags (loop for data in
                                        (ovorost-tools::occurrences 
                                         (mapcan #'event-tags (user-events user))
                                         :test (lambda (tag name)
                                                 (string-equal (tag-name tag) name))
                                         :key (lambda (tag)
                                                (tag-name (car tag))))
                                      collect (list :tag_size (format nil "~A" (cdr data))
                                                    :tag_name (trim (tag-name (car data)))))))))))

;;                            :tags (loop for tag in (remove-duplicates
;;                                                    (mapcan #'event-tags
;;                                                            (user-events user))
;;                                                    :test #'string-equal
;;                                                    :key #'tag-name)
;;                                     collect (list :tag_name (trim (tag-name tag)))))))))


(defun handle-index ()
  "Print main page."
  (hunchentoot:log-message :debug "Index")
  (let* ((country-param (hunchentoot:get-parameter "country"))
         (country-name (if country-param
                           country-param
                           (first (first *countries*))))
         (regions (concatenate 'string
                               *directory* *content-directory* country-name "/"))
         (country-data (find country-name *countries*
                             :test #'string-equal :key #'first))
         (user (get-user-from-session))
         (user-lat (if user
                       (trim (user-latitude user))
                       "0"))
         (user-lng (if user
                       (trim (user-longitude user))
                       "0"))
         (user-sex (if user
                       (trim (user-sex user))
                       "0"))
         (gravatar (if user 
                       (cl-gravatar:get-gravatar (user-email user))
                       (cl-gravatar:get-gravatar "a"))))
    (hunchentoot:log-message :error (format nil "~A" country-name))
    (make-web-page #p"index.html"
                   (list ;;:welcome welcome
                         :ident (hunchentoot:session-value 'user-id)
                         :google_maps_key (configuration-key (config-of *ovorost*))
                         :countries (loop for country in *countries*
                                       as name = (first country)
                                       as selected = (if (string-equal name country-name)
                                                         "selected"
                                                         "")
                                       collect (list :name name
                                                     :selected selected))
                         :latitude (second country-data)
                         :longitude (third country-data)
                         :user_lat user-lat
                         :user_lng user-lng
                         :user_sex user-sex
                         :gravatar gravatar
                         :level (fourth country-data)
                         :regions (loop for region in (list-directory regions)
                                     as i = 1 then (1+ i)
                                     collect (list :index (format nil "~A" i)
                                                   :country country-name
                                                   :region_name (pathname-name region)))))))


(defun handle-events ()
  "Print web page about ski events."
  (hunchentoot:log-message :debug "Events")
  (with-user-identity (ident)
    (let* ((country-name (first (first *countries*)))
           (country-data (find country-name *countries*
                             :test #'string-equal :key #'first)))
      (hunchentoot:log-message :error "Events ~A" (length (user-events ident)))
      (make-web-page #p"events.html"
                     (list :ident (hunchentoot:session-value 'user-id)
                           :google_maps_key (configuration-key (config-of *ovorost*))
                           :i18n_event (i18n "EventsPageEvent")
                           :i18n_title (i18n "EventsPageTitle")
                           :i18n_from (i18n "EventsPageFrom")
                           :i18n_to (i18n "EventsPageTo")
                           :i18n_resort (i18n "EventsPageResort")
                           :i18n_tags (i18n "EventsPageTags")
                           :i18n_note (i18n "EventsPageNote")
                           :i18n_content (i18n "EventsPageContent")
                           :i18n_public (i18n "EventsPagePublic")
                           :latitude (second country-data)
                           :longitude (third country-data)
                           :level (fourth country-data)
                           :events (loop for event in (user-events ident)
                                      collect (list :title (trim (event-title event))
                                                    :begin_date (trim (event-date-begin event))
                                                    :end_date (trim (event-date-end event))))
                            :resorts (sort
                                      (loop for resort-infos in (get-resorts-infos (backend-of *ovorost*))
                                            collect (list :name (trim (resort-infos-name resort-infos))))
                                      #'string<= :key #'second)
                           )))))


(defun handle-login ()
  "Print login page."
  (hunchentoot:log-message :info "Login [~A]"
                           (hunchentoot:session-value 'error-msg))
  (make-web-page #p"login.html"
                 (list :error_msg (hunchentoot:session-value 'error-msg)
                       :i18n_login_email (i18n "Email-Text")
                       :i18n_login_password (i18n "Password-Text")
                       :i18n_login_member (i18n "NotMember-Text"))))


(defun handle-logout ()
  "Kill session, and redirect to login page."
  (hunchentoot:log-message :debug "Logout")
  (hunchentoot:delete-session-value 'user-id)
  (hunchentoot:redirect "/index"))


(defun handle-identify ()
  "Check if we find a correct user account."
  (let* ((email (hunchentoot:post-parameter "login"))
         (password (hunchentoot:post-parameter "password")))
    (hunchentoot:log-message :info (format nil "Search user ~A / ~A" email password))
    (let ((user (find-user (backend-of *ovorost*) email :password password)))
      (if user
          (if (user-active-p user)
              (progn
                (setf (hunchentoot:session-value 'user-id)
                      (cons email password)) ;user)
                (hunchentoot:redirect "/index"))
              (hunchentoot:redirect "/login"))
          (progn
            (setf (hunchentoot:session-value 'error-msg)
                  "Ident failed")
            (hunchentoot:redirect "/login"))))))


(defun handle-sign-up ()
  "Display a new sign up page."
  (hunchentoot:log-message :debug "Resorts")
  (make-web-page #p"signup.html"
                 (list :i18n_email (i18n "SignupPageEmail")
                       :i18n_password (i18n "SignupPagePassword")
                       :i18n_confirm_password (i18n "SignupPageConfirmPassword")
                       :i18n_firstname (i18n "SignupPageFirstName")
                       :i18n_lastname (i18n "SignupPageLastName")
                       :i18n_latitude (i18n "SignupPageLatitude")
                       :i18n_longitude (i18n "SignupPageLongitude")
                       :i18n_Sex (i18n "SignupPageSex")
                       :i18n_male (i18n "SignupPageMale")
                       :i18n_female (i18n "SignupPageFemale")
                       :captcha_public (car (configuration-captcha (config-of *ovorost*))))))
                       

(defun handle-add-user ()
  "Add a new user."
  (let* ((email (hunchentoot:post-parameter "email"))
         (password (hunchentoot:post-parameter "password"))
         (confirm-passwd (hunchentoot:post-parameter "confirm-passwd"))
         (last-name (hunchentoot:post-parameter "last-name"))
         (first-name (hunchentoot:post-parameter "first-name"))
         (latitude (hunchentoot:post-parameter "latitude"))
         (longitude (hunchentoot:post-parameter "longitude"))
         (sex (hunchentoot:post-parameter "sex")))
    (let ((captcha-verified
           (cl-recaptcha:verify-captcha (cdr (configuration-captcha (config-of *ovorost*)))
                                        (hunchentoot:real-remote-addr)
                                        (hunchentoot:post-parameter "recaptcha_challenge_field")
                                        (hunchentoot:post-parameter "recaptcha_response_field"))))
      (hunchentoot:log-message :error "ReCaptcha : ~A]]]" captcha-verified)
      (if captcha-verified
          (progn
            (hunchentoot:log-message :info
                                     "New user [~A ~A ~A ] ~A ~A ~A ~A / ~A"
                                     email password confirm-passwd
                                     last-name first-name sex  latitude longitude)
            (if (string-equal password confirm-passwd)
                (progn
                  (add-user (ovorost-web:backend-of *ovorost*)
                            email password first-name last-name
                            longitude latitude sex)
                  (send-mail *from* email
                             (car *user-added-text*)
                             (format nil (cdr *user-added-text*)
                                     first-name last-name email password))
                  (hunchentoot:redirect "/identify"))
                (hunchentoot:redirect "/sign-up")))
          (hunchentoot:redirect "/sign-up")))))


(defun handle-add-event ()
  "Add a new event."
  (hunchentoot:log-message :error "Add event")
  (with-user-identity (user)
    (let ((title (hunchentoot:post-parameter "title"))
          (from (hunchentoot:post-parameter "from"))
          (to (hunchentoot:post-parameter "to"))
          (resort (hunchentoot:post-parameter "resort"))
          (note (hunchentoot:post-parameter "note"))
          (content (hunchentoot:post-parameter "content"))
          (public-p (not (null (hunchentoot:post-parameter "public-p"))))
          (tags (hunchentoot:post-parameter "tags")))
      (hunchentoot:log-message :info "Add ~A ~A ~A ~A ~A ~A ~A ~A"
                               title from to resort tags note content public-p)
       (add-event (backend-of *ovorost*) (user-email user) (user-password user)
                  title from to resort (parse-integer note) content public-p
                  (cl-ppcre:split " " tags)))
    (hunchentoot:redirect "/events")))


(defun handle-show-event ()
  "Display an event."
  (hunchentoot:log-message :info "Display an event")
  (with-user-identity (user)
    (let* ((id (hunchentoot:get-parameter "id"))
           (event (get-event-from-id (ovorost-web:backend-of *ovorost*) id)))
      (make-web-page #p"event.html"
                     (list :ident (hunchentoot:session-value 'user-id)
                           :event_title (trim (event-title event))
                           :begin (trim (event-date-begin event))
                           :end (trim (event-date-end event))
                           :resort (trim (resort-infos-name
                                          (event-resort-infos event)))
                           :note (format nil "~A" (event-note  event))
                           :content (trim (event-content event)))))))


(defun handle-update-user ()
  "Update user informations."
  (hunchentoot:log-message :info "Update user ")
  (with-user-identity (user)
    (let* ((id (hunchentoot:post-parameter "id"))
           (email (hunchentoot:post-parameter "email"))
           (password (hunchentoot:post-parameter "password"))
           (last-name (hunchentoot:post-parameter "last-name"))
           (first-name (hunchentoot:post-parameter "first-name"))
           (latitude (hunchentoot:post-parameter "latitude"))
           (longitude (hunchentoot:post-parameter "longitude"))
           (sex (hunchentoot:post-parameter "sex"))
           (active-p (not (null (hunchentoot:post-parameter "active-p")))))
      (hunchentoot:log-message :info
                               (format nil "Update user ~A [~A ~A] ~A ~A ~A ~A / ~A ~A"
                                       id email password
                                       last-name first-name sex latitude longitude active-p))
      (let ((user (get-user (backend-of *ovorost*) id)))
        (when user
          (setf (user-first-name user) first-name
                (user-last-name user) last-name
                (user-email user) email
                (user-latitude user) latitude
                (user-longitude user) longitude
                (user-sex user) sex
                (user-active-p user) active-p)
          (update-user (backend-of *ovorost*) user)))
      (hunchentoot:redirect "/admin"))))  


(defun handle-admin ()
  "Display administration page."
  (with-user-identity (user)
    (hunchentoot:log-message :error "Administration ~A"
                             (hunchentoot:post-parameter "id"))
    (let* ((id (hunchentoot:post-parameter "id"))
           (users (get-users (backend-of *ovorost*)))
           (user (if id
                     (get-user (backend-of *ovorost*) id)
                     (first users))))
      (with-accessors ((userid user-id)
                       (first-name user-first-name)
                       (last-name user-last-name)
                       (password user-password)
                       (email user-email)
                       (latitude user-latitude)
                       (longitude user-longitude)
                       (sex user-sex)
                       (active-p user-active-p)) user
        (make-web-page #p"admin.html"
                       (list :users (loop for current in users
                                       collect (list :id (format nil "~A" (user-id current))
                                                     :email (trim (user-email current))
                                                     :first_name (trim (user-first-name current))
                                                     :last_name (trim (user-last-name current))
                                                     :selected (if (= (user-id user) (user-id current))
                                                                   "selected"
                                                                   "")))
                             :events (loop for event in (user-events user)
                                        collect (list :title (trim (event-title event))
                                                      :begin (trim (event-date-begin event))
                                                      :end (trim (event-date-end event))
                                                      :resort (trim (resort-infos-name
                                                                     (event-resort-infos event)))
                                                      :note (format nil "~A" (event-note  event))))
                             :id (format nil "~A" userid)
                             :email (trim email)
                             :password (trim password)
                             :first_name (trim first-name)
                             :last_name (trim last-name)
                             :latitude (trim latitude)
                             :longitude (trim longitude)
                             :male_selected (if (string-equal (trim sex) "M")
                                                "selected"
                                                "")
                             :female_selected (if (string-equal (trim sex) "F")
                                                  "selected"
                                                  "")
                             :active_p (if active-p
                                          "checked"
                                          "")
                             :i18n_admin (i18n "AdminPageAdmin")
                             :i18n_users (i18n "AdminPageUsers")
                             :i18n_email (i18n "AdminPageEmail")
                             :i18n_password (i18n "AdminPagePassword")
                             :i18n_firstname (i18n "AdminPageFirstName")
                             :i18n_lastname (i18n "AdminPageLastName")
                             :i18n_latitude (i18n "AdminPageLatitude")
                             :i18n_longitude (i18n "AdminPageLongitude")
                             :i18n_enabled (i18n "AdminPageEnable")
                             :i18n_sex (i18n "AdminPageSex")
                             :i18n_male (i18n "AdminPageMale")
                             :i18n_female (i18n "AdminPageFemale")
                             :i18n_event_title (i18n "AdminPageEventTitle")
                             :i18n_event_begin (i18n "AdminPageEventBegin")
                             :i18n_event_end (i18n "AdminPageEventEnd")
                             :i18n_event_resort (i18n "AdminPageEventResort")
                             :i18n_event_note (i18n "AdminPageEventNote")
                             ))))))



(defun handle-tags ()
  "Print tags page."
  (hunchentoot:log-message :debug "Display news page")
  (with-user-identity (user)
    (let* ((name (hunchentoot:get-parameter "name"))
           (events (get-events-tag (backend-of *ovorost*) name)))
    (make-web-page #p"tags.html"
                   (list :tags (loop for event in events 
                                    collect (list :id (format nil "~A" (event-id event))
                                                  :title (event-title event))))))))


(defun handle-news ()
  "Print news page."
  (hunchentoot:log-message :debug "Display news page")
  (make-web-page #p"news.html"))


(defun handle-api ()
  "Print api page."
  (hunchentoot:log-message :debug "Display news page")
  (make-web-page #p"api.html"))

                 

(defun handle-language ()
  "Change user's language."
  (hunchentoot:log-message :info "Change language")
  (let* ((lang (hunchentoot:get-parameter "lang")))
    (when lang
      (set-user-language lang))
    (hunchentoot:redirect "/index")))
