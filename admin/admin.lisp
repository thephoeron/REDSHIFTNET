;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: admin.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; need user-support functions:
;; - (get-user-avatar session-token)
;; - (get-username session-token)
;; plus ajax callback functions in js for notifications and messages
;; with authenticated json requests
(defparameter *admin-header-logo* (string "/static/images/redshiftnet_header_logo.png"))
(defparameter *admin-login-logo* (string "/static/images/redshiftnet_text_logo.png"))

;; Admin header, footer, and menu template elements
(defun admin-header (title session-token)
  "Template block for admin site header."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:nav :class "navbar navbar-default navbar-fixed-top" :role "navigation"
      (:a :class "navbar-brand" :href "/admin/"
        (:img :class "img-responsive" :src (str (format nil "~A" *admin-header-logo*)) :alt "REDSHIFTNET Admin" :style "height: 40px; width: auto;"))
      (:button :type "button" :class "navbar-toggle btn-danger" :data-toggle "collapse" :data-target ".navbar-to-collapse"
        (:span :class "sr-only" "Toggle Menu")
        (:i :class "icon16 i-arrow-8"))
      (:div :class "collapse navbar-collapse navbar-to-collapse"
        (:ul :class "nav navbar-nav pull-right"
          (:li :class "divider-vertical")
          (:li :class "dropdown"
            (:a :href "#" :class "dropdown-toggle" :data-toggle "dropdown"
              (:i :class "icon24 i-bell-2")
              (:span :class "notification red" "1"))
            (:ul :class "dropdown-menu" :role "menu"
              (:li :role "presentation"
                (:a :href "#" :class ""
                  (:i :class "icon16 i-file-zip")
                  "User NonnyA attached 3 files"))))
          (:li :class "divider-vertical")
          (:li :class "dropdown"
            (:a :href "#" :class "dropdown-toggle" :data-toggle "dropdown"
              (:i :class "icon24 i-envelop-2")
              (:span :class "notification red" "3"))
            (:ul :class "dropdown-menu messages" :role "menu"
              (:li :class "head" :role "presentation"
                (:h4 "Inbox")
                (:span :class "count" "3 Messages")
                (:span :class "new-msg"
                  (:a :href "#" :class "tipB" :title "Compose new message"
                    (:i :class "icon16 i-pencil-5"))))))
          (:li :class "divider-vertical")
          (:li :class "dropdown user"
            (:a :href "#" :class "dropdown-toggle avatar" :data-toggle "dropdown"
              (:img :src "/static/images/avatars/anonymous.jpg" :alt "The User")
              (:span :class "more"
                (:i :class "icon16 i-arrow-down-2")))
            (:ul :class "dropdown-menu" :role "menu"
              (:li :role "presentation"
                (:a :href "#" :class "" (:i :class "icon16 i-cogs") " Settings"))
              (:li :role "presentation"
                (:a :href "#" :class "" (:i :class "icon16 i-user") " Profile"))
              (:li :role "presentation"
                (:a :href "#" :class "" (:i :class "icon16 i-exit") " Logout"))))
          (:li :class "divider-vertical"))))))

(defun admin-footer ()
  "Template block for admin site footer."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:div :class "push" :style "display: inline-block; height: 100px; position: relative;" "&nbsp;")
    (:div :class "container-fluid" :style "position: fixed; bottom: 0; height: 80px; width: 100%; padding: 15px 0; background-color: rgba(238,238,238,0.5); border-top: 1px solid rgba(85,85,85,0.5);"
      (:div :class "row"
        (:div :class "col-lg-12"
          (:p :class "muted credit center"
            (fmt "Crafted in (~C) Common Lisp" #\GREEK_SMALL_LETTER_LAMDA)
            (:br)
            (fmt "Powered by ~A v~A and ~A v~A."
              (lisp-implementation-type)
              (lisp-implementation-version)
              (server-type)
              (server-version))))))))

(defun admin-menu ()
  "Admin Site menu generator function."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:div :class "side-options"
      (:ul (:li (:a :href "#" :id "collapse-nav" :class "act act-primary tip" :title "Collapse Navigation" (:i :class "icon16 i-arrow-left-7")))))
    (:div :class "sidebar-wrapper"
      (:nav :id "mainnav"
        (:ul :class "nav nav-list"
          (:li (:a :href "/admin/dashboard/"
            (:span :class "icon" (:i :class "icon20 i-screen"))
            (:span :class "txt" "Dashboard")))
          (:li (:a :href "/admin/webmail/"
            (:span :class "icon" (:i :class "icon20 i-envelop-2"))
            (:span :class "txt" "Webmail")))
          (:li (:a :href "/admin/calendar/"
            (:span :class "icon" (:i :class "icon20 i-calendar"))
            (:span :class "txt" "Calendar")))
          (:li (:a :href "#tables"
            (:span :class "icon" (:i :class "icon20 i-table-2"))
            (:span :class "txt" "Tables")
            (:ul :class "sub"
              (:li (:a :href "/admin/tables/"
                       (:span :class "icon" (:i :class "icon20 i-table"))
                       (:span :class "txt" "All Tables")))
              (:li (:a :href "/admin/tables/person/"
                       (:span :class "icon" (:i :class "icon20 i-table"))
                       (:span :class "txt" "Person Table"))))))
          (:li (:a :href "#settings"
            (:span :class "icon" (:i :class "icon20 i-cogs"))
            (:span :class "txt" "Settings")
            (:ul :class "sub"
              (:li (:a :href "#"
                      (:span :class "icon" (:i :class "icon20 i-cogs"))
                      (:span :class "txt" "All Settings")))
              (:li (:a :href "#"
                      (:span :class "icon" (:i :class "icon20 i-cube-3"))
                      (:span :class "txt" "Widgets"))))))
          (:li (:a :href "#about"
            (:span :class "icon" (:i :class "icon20 i-notification"))
            (:span :class "txt" "About")
            (:ul :class "sub"
              ;; credits
              (:li (:a :href "#"
                (:span :class "icon" (:i :class "icon20 i-quill-3"))
                (:span :class "txt" "Credits")))
              ;; documentation
              (:li (:a :href "#"
                (:span :class "icon" (:i :class "icon20 i-stack-list"))
                (:span :class "txt" "Documentation")))
              ;; help
              (:li (:a :href "#"
                (:span :class "icon" (:i :class "icon20 i-question"))
                (:span :class "txt" "Help")))))))))))

;; Admin Login page
(eval-when (:compile-toplevel :execute :load-toplevel)
  ;; Gotta make sure these parameters are available at macro expansion time
  ;; Login page
  (defparameter admin-login-styles (list "/static/css/app.css"
                                         "/static/css/custom.css"
                                         "/static/css/pages/login.css"))
  (defparameter admin-login-scripts (list "/static/js/plugins/forms/uniform/jquery.uniform.min.js"
                                          "/static/js/plugins/forms/validation/jquery.validate.js"
                                          "/static/js/app.js"
                                          "/static/js/pages/login.js"))
  ;; Admin Dashboard
  (defparameter admin-dashboard-styles
    (list "/static/js/plugins/tables/datatables/media/css/jquery.dataTables.css"
          "/static/css/app.css"
          "/static/css/custom.css"))
  (defparameter admin-dashboard-scripts 
    (list "/static/js/plugins/forms/uniform/jquery.uniform.min.js"
          "/static/js/plugins/tables/datatables/media/js/jquery.dataTables.js"
          "/static/js/pages/data-tables.js"
          "/static/js/app.js"
          "/static/js/pages/dashboard.js"))
  ;; Admin Calendar
  (defparameter admin-calendar-styles
    (list "/static/js/plugins/misc/fullcalendar/fullcalendar.css"
          "/static/css/app.css"
          "/static/css/custom.css"))
  (defparameter admin-calendar-scripts
    (list "/static/js/plugins/forms/uniform/jquery.uniform.min.js"
          "/static/js/plugins/misc/fullcalendar/fullcalendar.min.js"
          "/static/js/app.js"
          "/static/js/pages/calendar.js")))

(define-rsn-form (admin-login-form :submit "Login" :action "")
  ((username text
    :icon "i-user"
    :validation
    ((not-blank?) "Your username is required"
     (longer-than? 2) "Your username must be longer than 2 characters"))
   (password password
    :icon "i-key"))
  (let ((the-user (cl-who:escape-string username))
        (the-pass (cl-who:escape-string password)))
    ;; need to insert validation here
    (push-success-msg (format nil "Thank you, ~A.  You have logged in successfully." the-user))
    (redirect (hunchentoot:referer))))

(define-rsn-form (admin-forgot-password-form :submit "Recover Password")
  ((username text
    :icon "i-user"
    :validation
    ((not-blank?) "Your username is required"
     (longer-than? 2) "Your username must be longer than 2 characters"))
   (email text
    :icon "i-envelop-2"
    :validation
    ((not-blank?) "Your email address is required"
     (is-email?) "The email address you entered does not appear to be valid.")))
  (let ((the-user (cl-who:escape-string username))
        (the-email (cl-who:escape-string email)))
    (push-success-msg (format nil "Thank you, ~A. Your temporary password has been sent to ~A." the-user the-email))))

(defun admin-login (logo)
  "Login page for admin section."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:div :class "container-fluid"
      (:div :id "login"
        (:div :class "login-wrapper" :data-active "log"
          (:a :class "navbar-brand" :href "#"
            (:img :class "image-responsive" :alt "REDSHIFTNET Admin" :src (str (format nil "~A" logo))))
          (:div :id "log"
            (:div :class "page-header"
              (:h3 :class "center" "Please Login"))
            (show-rsn-form admin-login-form))
          (:div :id "forgot"
            (:div :class "page-header"
              (:h3 :class "center" "Forgot Password"))
            (show-rsn-form admin-forgot-password-form)))
        (:div :id "bar" :data-active "log"
          (:div :class "btn-group btn-group-vertical"
            (:a :id "log" :href "#" :class "btn tipR" :title "Login"
              (:i :class "icon16 i-key"))
            (:a :id "forgot" :href "#" :class "btn tipR" :title "Forgot Password"
              (:i :class "icon16 i-question"))))
        (:div :class "clearfix")))))

;; Admin page generator macros
; (defmacro %basic-admin-app-page ((&key (title "REDSHIFTNET Admin") (styles nil) (scripts nil)) &body body)
;   "Basic, no frills Admin page function, useful for error pages, system notifications, login pages, wrapping AJAX html content, etc."
;   `(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
;     (:html :lang "en"
;       (:head
;         (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
;         (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
;         (:meta :name "application-name" :content "REDSHIFTNET Admin")
;         (:meta :name "author" :content "the Phoeron <//thephoeron.com/>")
;         (:link :rel "stylesheet" :href "/static/css/bootstrap/bootstrap.min.css" :type "text/css" :media "screen")
;         (:link :rel "stylesheet" :type "text/css" :href "/static/css/bootstrap/bootstrap-theme.css")
;         (:link :rel "stylesheet" :type "text/css" :href "/static/css/icons.css")
;         (:link :rel "stylesheet" :type "text/css" :href "/static/js/plugins/ui/jgrowl/jquery.jgrowl.css")
;         "<!--[if IE 8]><link href=\"css/ie8.css\" rel=\"stylesheet\" type=\"text/css\" /><![endif]-->"
;         (:link :rel "apple-touch-icon-precomposed" :sizes "144x144" :href "/static/images/ico/apple-touch-icon-144-precomposed.png")
;         (:link :rel "apple-touch-icon-precomposed" :sizes "114x114" :href "/static/images/ico/apple-touch-icon-114-precomposed.png")
;         (:link :rel "apple-touch-icon-precomposed" :sizes "72x72" :href "/static/images/ico/apple-touch-icon-72-precomposed.png")
;         (:link :rel "apple-touch-icon-precomposed" :href "/static/images/ico/apple-touch-icon-57-precomposed.png")
;         (:link :rel "shortcut icon" :href "/static/images/ico/favicon.png")
;         ;(:link :rel "stylesheet" :href "/redshiftnet.css" :type "text/css" :media "screen")
;         ,@(mapcar (lambda (file)
;                     `(:link :type "text/css" :rel "stylesheet" :media "screen"
;                             :href ,(format nil "~A" file)))
;                       (lol:flatten (list styles)))
;         (:title ,title)
;         "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
;       (:body
;         ,@body
;         (:script :type "text/javascript" :src "/static/js/jquery-1.10.2.min.js")
;         (:script :type "text/javascript" :src "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.2/jquery-ui.min.js")
;         (:script :type "text/javascript" :src "/static/js/bootstrap/bootstrap.min.js")
;         (:script :type "text/javascript" :src "/static/js/conditionizr.min.js")
;         (:script :type "text/javascript" :src "/static/js/plugins/core/nicescroll/jquery.nicescroll.min.js")
;         (:script :type "text/javascript" :src "/static/js/plugins/core/jrespond/jRespond.min.js")
;         (:script :type "text/javascript" :src "/static/js/jquery.redshiftnetAdmin.js")
;         (:script :type "text/javascript" :src "/static/js/plugins/ui/jgrowl/jquery.jgrowl.min.js")
;         ;(:script :type "text/javascript" :src "/redshiftnet.js")
;         ,@(mapcar (lambda (file)
;                     `(:script :type "text/javascript"
;                               :src ,(format nil "~A" file)))
;                   (lol:flatten (list scripts)))))))

; (defun admin-breadcrumb (script-name &optional get-parameters)
;   "Creates bootstrapped navigation breadcrumbs from current request's script-name* and optional get-parameters"
;   (let* ((script-list (split-sequence #\/ script-name :remove-empty-subseqs t)))
;     (html-to-stout
;       (:div :class "crumb"
;         (:ul :class "breadcrumb"
;           (:li (:a :href "/" (:i :class "icon16 i-home-4") "Home"))
;           (if get-parameters
;               (progn
;                 (loop for page in script-list
;                       do (htm
;                            (:li 
;                              (:a :href (str (inclusive-search-and-return-string script-name page))
;                                (str (string-symbol-to-label (cl-who:escape-string page)))))))
;                 (htm
;                   (:li :class "active"
;                     (loop for res in get-parameters
;                           do (htm 
;                                (str (string-symbol-to-label 
;                                       (format nil "~A" (car res)))) 
;                                (str ": ")
;                                (str (string-symbol-to-label 
;                                       (format nil "~A" (cdr res)))))))))
;               ; else
;               (progn
;                 (loop for page in script-list
;                       for i upto (length script-list)
;                       when (< i (1- (length script-list)))
;                         do (htm
;                           (:li (:a :href (str (inclusive-search-and-return-string script-name page))
;                             (str (string-symbol-to-label (cl-who:escape-string page))))))
;                       when (eq i (1- (length script-list)))
;                         do (htm (:li :class "active"
;                         (str (string-symbol-to-label (cl-who:escape-string page)))))))
;                 ))))))

; (defmacro %admin-app-page ((&key (title "REDSHIFTNET Admin") (styles nil) (scripts nil) header menu footer) &body body)
;   "Standard app page template."
;   `(%basic-admin-app-page (:title ,title :styles ,(eval styles) :scripts ,(eval scripts))
;     (cl-who:with-html-output (hunchentoot::*standard-output*)
;       (:header :id "header"
;         (funcall ,header ,title (hunchentoot:session-value 'token)))
;       (:div :class "main"
;         (:aside :id "sidebar" (funcall ,menu))
;         (:section :id "content"
;           (:div :class "wrapper"
;             (admin-breadcrumb (hunchentoot:script-name*) (hunchentoot:get-parameters*))
;             (:div :class "container-fluid"
;               ,@body))))
;       (:footer :id "footer"
;         (funcall ,footer)))))

;; Admin auth page macro
;; admin pages return nil unless inside an ssl vhost defrequest
; (defmacro %admin-auth-page ((title lpf) &body body)
;   "Core auth-page template."
;   `(postmodern:with-connection (list ,*primary-db* ,*primary-db-user* ,*primary-db-pass* ,*primary-db-host*)
;     (when (hunchentoot:ssl-p)
;      (if (null (hunchentoot:session-value 'token))
;          (cond ((eql :post (hunchentoot:request-method*))
;                 (let ((username (trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "username"))))
;                       (password (trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "password")))))
;                   (if (validate-credentials username password)
;                       (progn (create-new-session username)
;                              ,@body)
;                       (%basic-admin-app-page (:title "Login Failed"
;                                               :scripts ,@(list admin-login-scripts)
;                                               :styles ,@(list admin-login-styles))
;                         (show-all-messages)
;                         (funcall ,lpf *admin-login-logo*)))))
;                ((eql :get (hunchentoot:request-method*))
;                 (progn (hunchentoot:start-session)
;                        (%basic-admin-app-page (:title "Login"
;                                                :scripts ,@(list admin-login-scripts)
;                                                :styles ,@(list admin-login-styles))
;                          (cl-who:with-html-output (hunchentoot::*standard-output*)
;                            (funcall ,lpf *admin-login-logo*)))))
;                (t (hunchentoot:redirect "/403/")))
;          ;; else
;          (let* ((token (hunchentoot:session-value 'token))
;                 (sesh-id (get-session-id-by-token token))
;                 (the-sesh (postmodern:get-dao 'public-session token))
;                 (the-user (public-session-user-id token)))
;            (if (validate-session token)
;                (progn
;                 (setf (expiry-date the-sesh)
;                       (local-time:format-timestring nil (local-time:now)))
;                       (create-new-session (username the-user))
;                       (cl-who:with-html-output (hunchentoot::*standard-output*)
;                         ,@body))
;                (progn (%basic-admin-app-page (:title "Error: Validation Failure"
;                                               :scripts ,@(list admin-login-scripts)
;                                               :styles ,@(list admin-login-styles))
;                         (show-all-messages)
;                         (cl-who:with-html-output (hunchentoot::*standard-output*)
;                           (funcall ,lpf *admin-login-logo*))))))))))

(defmacro admin-page ((title login-page-fun &key (styles nil) (scripts nil)) &body body)
  "Admin site page generator macro."
  `(%auth-page (title ,login-page-fun)
     (%app-page (:title ,title :header #'admin-header
                 :menu #'admin-menu :footer #'admin-footer
                 :scripts ,scripts :styles ,styles)
      (cl-who:with-html-output (hunchentoot::*standard-output*)
        ,@body))))

;; Admin Dashboard
(defun admin-dashboard ()
  "Admin site dashboard widget generator function."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:div :id "heading" :class "page-header"
      (:h1 (:i :class "icon20 i-dashboard") " Dashboard"))
    (:div :class "row"
      (:p :class "lead"
        "Welcome to REDSHIFTNET Admin. This is a good place to put your favourite dashboard widgets. There should probably be a list somewhere that is built through the interface, so users can customize it... Some dependencies need to be rebuilt."))))

(defrequest rsn-admin (:vhost vhost-admin)
  (admin-page ("REDSHIFTNET Admin :: Lander" #'admin-login
               :styles admin-dashboard-styles
               :scripts admin-dashboard-scripts)
     (hunchentoot:redirect "/admin/dashboard/")))

(defrequest rsn-admin/dashboard (:vhost vhost-admin)
  (admin-page ("[REDSHIFTNET Admin :: Dashboard]" #'admin-login
               :styles admin-dashboard-styles
               :scripts admin-dashboard-scripts)
    (admin-dashboard)))

(defrequest rsn-admin/webmail (:vhost vhost-admin)
  (admin-page ("REDSHIFTNET Admin :: Webmail" #'admin-login
               :styles admin-dashboard-styles
               :scripts admin-dashboard-scripts)
    (html-to-stout
      (:div :id "heading" :class "page-header"
        (:h1 (:i :class "icon20 i-envelop-2") " Webmail"))
      (:div :class "row"
        (:div :id "email" :class "col-lg-12"
          (:div :class "email-bar")
          (:div :class "email-nav well")
          (:div :class "email-wrapper"))))))

(defrequest rsn-admin/calendar (:vhost vhost-admin)
  (admin-page ("REDSHIFTNET Admin :: Calendar" #'admin-login
               :styles admin-calendar-styles
               :scripts admin-calendar-scripts)
    (html-to-stout
      (:div :id "heading" :class "page-header"
        (:h1 (:i :class "icon20 i-calendar") " Calendar"))
      (:div :class "row"
        (:div :class "col-lg-8"
          (:div :class "panel plain"
            (:div :class "panel-heading"
              (:i :class "icon20 i-calendar-3")
              (:h4 "Calendar")
              (:a :href "#" :class "minimize"))
            (:div :class "panel-body noPadding"
              (:div :id "calendar"))))
        (:div :class "col-lg-4"
          (:div :class "page-header"
            (:h4 "Events " (:small "drop events to calendar")))
          (:div :id "external-events"
            (:div :class "external-event" "Meeting with client")
            (:div :class "external-event" "Telephone interview")
            (:div :class "external-event" "Face-to-face")))))))

;; Edit page for database record
(defun generate-edit-page-for-database-record (table-name record-id)
  "Accepts a table name and record id, and creates a new request object with an auto-generated form that allows you to edit the record."
  )

(defun output-named-database-table-as-html (table-name)
  "Outputs the named database table as html."
  (postmodern:with-connection (list *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
    (let* ((all-records
              (postmodern:query
                (format nil "SELECT * FROM ~A" (normalize-for-sql table-name))
                :alists)))
      (html-to-stout
        (:div :id "heading" :class "page-header"
          (:h1 (:i :class "icon20 i-table" 
            "&nbsp" (string-symbol-to-label table-name))))
        (:div :class "row"
          (:div :class "col-lg-12"
            (:table :class "table table-striped table-bordered table-hover" :id "dataTable"
              (:thead (:tr
                (loop for a in all-records
                      for i upto 0
                      do (loop for b in a
                               unless (eq (car b) :password)
                               do (htm (:th (str (string-symbol-to-label (format nil "~A" (car b))))))))))
              (:tbody
                (loop for x in all-records
                      do (htm (:tr (loop for y in x
                                         unless (eq (car y) :password)
                                         do (htm (:td (str (format nil "~A" (cdr y)))))))))))))))))

;; Output all database tables as html
(defun output-all-database-tables-as-html ()
  "Introspects on the current toplevel database and generates an admin page that lists all tables grouped by app, following the PostgreSQL naming convention 'appname_tablename'."
 (postmodern:with-connection (list *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
  (let* ((all-tables (loop for table in (list-database-tables)
                           collect (split-sequence #\_ table)))
         (parsed-tables (loop for (x y) in all-tables
                              collect x))
         (table-group-names (remove-duplicates parsed-tables :test #'string=))
         (table-group-lists (loop for z in table-group-names
                                  collect (list z (loop for x in all-tables
                                                        when (string= (car x) z)
                                                             collect x))))
         (final-table-list (loop for (x . y) in table-group-lists
                                 collect (list x (lol:flatten
                                               (loop for i in y
                                                     collect (loop for j in i
                                                                   collect (format nil "~{~a~^-~}" j))))))))
    (html-to-stout
    (loop for (x . y) in final-table-list
          do (htm
               (:div :class "panel panel-default"
                 (:div :class "panel-heading"
                   (:div :class "icon" (:i :class "icon20 i-table-2"))
                   (:h4 (str (string-symbol-to-label x)) (:small " app tables"))
                   (:a :href "#" :class "minimize"))
                 (:div :class "panel-body noPadding"
                   (:table :class "table table-bordered"
                     (:tbody
                     (loop for i in y
                           do (loop for j in i
                              do (progn
                                (htm
                                (:tr
                                  (:td
                                    (:i :class "icon20 i-table")
                                    "&nbsp;"
                                    (:a :href (str (format nil "/admin/tables/?name=~a" j))
                                      (str (string-symbol-to-label j)))))))))))))
               (:div :class "clearfix" "&nbsp;")))))))

;; All Tables request
;; Accepts get-parameter 'name' to display specific table
;; Otherwise it lists all tables in current db grouped by app
(defrequest rsn-admin/tables (:vhost vhost-admin)
  (admin-page ("REDSHIFTNET Admin :: All Tables" #'admin-login
               :styles admin-dashboard-styles
               :scripts admin-dashboard-scripts)
    (if (hunchentoot:get-parameter "name")
        (output-named-database-table-as-html (hunchentoot:get-parameter "name"))
        ; else
        (cl-who:with-html-output (hunchentoot::*standard-output*)
          (:div :id "heading" :class "page-header"
            (:h1 (:i :class "icon20 i-dashboard") " All Tables"))
          (:div :class "row"
            (:div :class "col-lg-8"
              (output-all-database-tables-as-html))
            (:div :class "col-lg-4"
              (:div :class "panel panel-default"
                (:div :class "panel-heading"
                  (:div :class "icon" (:i :class "icon20 i-power"))
                  (:h4 "Recent activity")
                  (:a :href "#" :class "minimize"))
                (:div :class "panel-body noPadding"
                  (:ul :class "recent-activity"
                    (:li :class "item"
                      (:span :class "icon green" (:i :class "icon16 i-user"))
                      (:span :class "text"
                        "user "
                        (:a :href "#" "ManningP")
                        " changed password on user "
                        (:a :href "#" "NonnyA"))
                      (:span :class "ago" "10 min ago"))
                    (:li :class "item"
                      (:span :class "icon blue" (:i :class "icon16 i-notification"))
                      (:span :class "text"
                        (:a :href "#" "5 callbacks")
                        " were automatically closed")
                      (:span :class "ago" "1 day ago"))
                    (:li :class "item"
                      (:span :class "icon red" (:i :class "icon16 i-key"))
                      (:span :class "text"
                        "user "
                        (:a :href "#" "DarkoD")
                        " has "
                        (:a :href "#" "5 failed login attempts"))
                      (:span :class "ago" "2 days ago")))))))))))

;; EOF
