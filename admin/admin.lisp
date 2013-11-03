;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: admin.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; need user-support functions:
;; - (get-user-avatar session-token)
;; - (get-username session-token)
;; plus ajax callback functions in js for notifications and messages
;; with authenticated json requests

;; Admin header, footer, and menu template elements
(defun admin-header (title session-token)
  "Template block for admin site header."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:nav :class "navbar navbar-default navbar-fixed-top" :role "navigation"
      (:a :class "navbar-brand" :href "/admin/"
        (:img :src "/static/images/logo.png" :alt "REDSHIFTNET Admin" :class "img-responsive"))
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
    (:div :class "push")
    (:div :class "container"
      (:div :class "row"
        (:div :class "col-md-12"
          (:p :class "muted credit"
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
          (:li (:a :href "/admin/"
            (:span :class "icon" (:i :class "icon20 i-screen"))
            (:span :class "txt" "Dashboard")))
          (:li (:a :href "#tables"
            (:span :class "icon" (:i :class "icon20 i-table-2"))
            (:span :class "txt" "Tables")
            (:ul :class "sub"
              (:li (:a :href "/admin/tables/"
                       (:span :class "icon" (:i :class "icon20 i-table"))
                       (:span :class "txt" "All Tables")))
              (:li (:a :href "/admin/tables/person/"
                       (:span :class "icon" (:i :class "icon20 i-table"))
                       (:span :class "txt" "Person Table")))))))))))

;; Admin Login page
(eval-when (:compile-toplevel :execute :load-toplevel)
  ;; Gotta make sure these parameters are available at macro expansion time
  ;; Login page
  (defparameter admin-login-styles (list "/static/css/pages/login.css"))
  (defparameter admin-login-scripts (list "/static/js/plugins/forms/uniform/jquery.uniform.min.js"
                                          "/static/js/plugins/forms/validation/jquery.validate.js"
                                          "/static/js/pages/login.js"))
  ;; Admin Dashboard
  (defparameter admin-dashboard-styles
    (list "/static/js/plugins/tables/datatables/jquery.dataTables.css"
          "/static/css/custom.css"
          "/static/css/app.css"))
  (defparameter admin-dashboard-scripts 
    (list "/static/js/plugins/forms/uniform/jquery.uniform.min.js"
          "/static/js/plugins/tables/datatables/jquery.dataTables.min.js"
          "/static/js/pages/data-tables.js"
          "/static/js/pages/dashboard.js"
          "/static/js/app.js")))

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
            (:img :class "image-responsive" :alt "REDSHIFTNET Admin" :src (str logo)))
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
(defmacro %basic-admin-app-page ((&key (title "REDSHIFTNET Admin") (styles nil) (scripts nil)) &body body)
  "Basic, no frills Admin page function, useful for error pages, system notifications, login pages, wrapping AJAX html content, etc."
  `(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
      (:head
        (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
        (:meta :name "application-name" :content "REDSHIFTNET Admin")
        (:meta :name "author" :content "the Phoeron <//thephoeron.com/>")
        (:link :rel "stylesheet" :href "/static/css/bootstrap/bootstrap.min.css" :type "text/css" :media "screen")
        (:link :rel "stylesheet" :type "text/css" :href "/static/css/bootstrap/bootstrap-theme.css")
        (:link :rel "stylesheet" :type "text/css" :href "/static/css/icons.css")
        (:link :rel "stylesheet" :type "text/css" :href "/static/js/plugins/forms/uniform/uniform.default.css")
        (:link :rel "stylesheet" :type "text/css" :href "/static/js/plugins/ui/jgrowl/jquery.jgrowl.css")
        "<!--[if IE 8]><link href=\"css/ie8.css\" rel=\"stylesheet\" type=\"text/css\" /><![endif]-->"
        (:link :rel "apple-touch-icon-precomposed" :sizes "144x144" :href "/static/images/ico/apple-touch-icon-144-precomposed.png")
        (:link :rel "apple-touch-icon-precomposed" :sizes "114x114" :href "/static/images/ico/apple-touch-icon-114-precomposed.png")
        (:link :rel "apple-touch-icon-precomposed" :sizes "72x72" :href "/static/images/ico/apple-touch-icon-72-precomposed.png")
        (:link :rel "apple-touch-icon-precomposed" :href "/static/images/ico/apple-touch-icon-57-precomposed.png")
        (:link :rel "shortcut icon" :href "/static/images/ico/favicon.png")
        ;(:link :rel "stylesheet" :href "/redshiftnet.css" :type "text/css" :media "screen")
        (:link :rel "stylesheet" :href "/static/css/app.css" :type "text/css" :media "screen")
        ,@(mapcar (lambda (file)
                    `(:link :type "text/css" :rel "stylesheet" :media "screen"
                            :href ,(format nil "~A" file)))
                      (lol:flatten (list styles)))
        (:title ,title)
        "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
      (:body
        ,@body
        (:script :type "text/javascript" :src "/static/js/jquery-1.10.2.min.js")
        (:script :type "text/javascript" :src "http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.2/jquery-ui.min.js")
        (:script :type "text/javascript" :src "/static/js/bootstrap/bootstrap.min.js")
        (:script :type "text/javascript" :src "/static/js/conditionizr.min.js")
        (:script :type "text/javascript" :src "/static/js/plugins/core/nicescroll/jquery.nicescroll.min.js")
        (:script :type "text/javascript" :src "/static/js/plugins/core/jrespond/jRespond.min.js")
        (:script :type "text/javascript" :src "/static/js/jquery.redshiftnetAdmin.js")
        (:script :type "text/javascript" :src "/static/js/plugins/ui/jgrowl/jquery.jgrowl.min.js")
        ;(:script :type "text/javascript" :src "/redshiftnet.js")
        (:script :type "text/javascript" :src "/static/js/app.js")
        ,@(mapcar (lambda (file)
                    `(:script :type "text/javascript"
                              :src ,(format nil "~A" file)))
                  (lol:flatten (list scripts)))))))

(defmacro %admin-app-page ((&key (title "REDSHIFTNET Admin") (styles nil) (scripts nil) header menu footer) &body body)
  "Standard app page template."
  `(%basic-admin-app-page (:title ,title :styles ,(eval styles) :scripts ,(eval scripts))
    (cl-who:with-html-output (hunchentoot::*standard-output*)
      (:header :id "header"
        (funcall ,header ,title (hunchentoot:session-value 'token)))
      (:div :class "main"
        (:aside :id "sidebar" (funcall ,menu))
        (:section :id "content"
          (:div :class "wrapper"
            (:div :class "crumb"
              (:ul :class "breadcrumb"
                (:li (:a :href "/" (:i :class "icon16 i-home-4") "Home"))
                (:li (:a :href "/admin/" "Admin"))
                (:li :class "active" (:a :href "/admin/" "Dashboard"))))
            (:div :class "container-fluid"
              ,@body))))
      (:footer :id "footer"
        (funcall ,footer)))))

;; Admin auth page macro
;; admin pages return nil unless inside an ssl vhost defrequest
(defmacro %admin-auth-page ((title lpf) &body body)
  "Core auth-page template."
  `(when (hunchentoot:ssl-p)
     (if (null (hunchentoot:session-value 'token))
         (cond ((eql :post (hunchentoot:request-method*))
                (let ((username (trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "username"))))
                      (password (trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "password")))))
                  (if (validate-credentials username password)
                      (progn (create-new-session username)
                             ,@body)
                      (%basic-admin-app-page (:title "Login Failed"
                                              :scripts ,@(list admin-login-scripts)
                                              :styles ,@(list admin-login-styles))
                        (show-all-messages)
                        (funcall ,lpf "/static/images/redshiftnet_text_logo.png")))))
               ((eql :get (hunchentoot:request-method*))
                (progn (hunchentoot:start-session)
                       (%basic-admin-app-page (:title "Login"
                                               :scripts ,@(list admin-login-scripts)
                                               :styles ,@(list admin-login-styles))
                         (cl-who:with-html-output (hunchentoot::*standard-output*)
                           (funcall ,lpf "/static/images/redshiftnet_text_logo.png")))))
               (t (hunchentoot:redirect "/403/")))
         ;; else
         (let* ((token (hunchentoot:session-value 'token))
                (the-sesh (postmodern:get-dao 'public-session token))
                (the-user (public-session-user-id token)))
           (if (validate-session token)
               (progn (update-public-session-exp-date (local-time:format-timestring nil (local-time:now)) token)
                      (create-new-session the-user)
                      (cl-who:with-html-output (hunchentoot::*standard-output*)
                        ,@body))
               (progn (%basic-admin-app-page (:title "Error: Validation Failure"
                                              :scripts ,@(list admin-login-scripts)
                                              :styles ,@(list admin-login-styles))
                        (show-all-messages)
                        (cl-who:with-html-output (hunchentoot::*standard-output*)
                          (funcall ,lpf "/static/images/redshiftnet_text_logo.png")))))))))

(defmacro admin-page ((title login-page-fun &key (styles nil) (scripts nil)) &body body)
  "Admin site page generator macro."
  `(%admin-auth-page (title ,login-page-fun)
     (%admin-app-page (:title ,title :header #'admin-header
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
        "Welcome to REDSHIFTNET Admin.  This is a good place to put your favourite dashboard widgets.  There should probably be a list somewhere that is built through the interface, so users can customize it..."))))

(defrequest rsn-admin (:vhost vhost-ssl)
  (admin-page ("REDSHIFTNET Admin :: Dashboard" #'admin-login
               :styles admin-dashboard-styles
               :scripts admin-dashboard-scripts)
     (admin-dashboard)
     (cl-who:with-html-output (hunchentoot::*standard-output*)
       (:p "A test paragraph, just to see what the deal is... and more... and..."))))

;; Edit page for database record
(defun generate-edit-page-for-database-record (table-name record-id)
  "Accepts a table name and record id, and creates a new request object with an auto-generated form that allows you to edit the record."
  )

;; Generate page for database table
;; doesn't seem to be working yet; keep getting 'table-name not a string'
;; type errors on macro-expansion
(defmacro generate-page-for-database-table (table-name)
  "Accepts a database table name and creates a new request object populated by that table's records."
  (declare (type string table-name))
  (let* ((all-records 
           (postmodern:query (:select '* :from (intern table-name)) :alists))
         (request-name (intern (format nil "rsn-admin/tables/~A/" table-name)))
         (request-title (format nil "REDSHIFTNET Admin :: Tables :: ~A" (string-symbol-to-label table-name))))
    `(defrequest ,request-name (:vhost ,vhost-ssl)
      (admin-page (,request-title #'admin-login
                   :styles ,admin-dashboard-styles
                   :scripts ,admin-dashboard-scripts)
        (cl-who:with-html-output (hunchentoot::*standard-output*)
          (:div :id "heading" :class "page-header"
            (:h1 (:i :class "icon20 i-table") "&nbsp" request-title))
          (:div :class "row"
            (:div :class "col-lg-12"
              (:table :class "table table-striped table-bordered table-hover" :id "dataTable"
                (:thead
                  (:tr
                    (loop for a in all-records
                          for i upto 0
                          do (loop for b in a
                                   do (htm (:th (str (format nil "~A" (car b)))))))))
                (:tbody
                  (loop for x in all-records
                        do (htm (:tr (loop for y in x
                                           do (htm (:td (str (format nil "~A" (cdr y))))))))))))))))))

; (with-connection (list *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
;   (format t "~%; Connecting to PostgreSQL and creating table pages...")
;   (loop for table in (list-database-tables)
;         do (let ((the-table (format nil "~A" (substitute #\- #\_ table))))
;              (format t "~%; -- ~A" the-table)
;              (generate-page-for-database-table the-table))))

;; Output all database tables as html
(defun output-all-database-tables-as-html ()
  "Introspects on the current toplevel database and generates an admin page that lists all tables grouped by app, following the PostgreSQL naming convention 'appname_tablename'."
  (let* ((all-tables (cl:loop for table in (list-database-tables)
                           collect (split-sequence #\_ table)))
         (parsed-tables (cl:loop for (x y) in all-tables
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
               (:div :class "col-lg-8"
                (:div :class "page-header"
                  (:h3 (str (string-symbol-to-label x))))
               (:table :class "table table-striped"
                 (:thead
                   (:tr (:th "Table Name")))
                 (:tbody
                 (loop for i in y
                       do (loop for j in i
                          do (progn
                            (htm
                            (:tr
                              (:td
                                (:i :class "icon20 i-table")
                                "&nbsp;"
                                (:a :href (str (format nil "/admin/tables/~a/" j))
                                  (str (string-symbol-to-label j)))))))))))))))))

(defrequest rsn-admin/tables (:vhost vhost-ssl)
  (admin-page ("REDSHIFTNET Admin :: All Tables" #'admin-login
               :styles admin-dashboard-styles
               :scripts admin-dashboard-scripts)
    (cl-who:with-html-output (hunchentoot::*standard-output*)
      (:div :id "heading" :class "page-header"
        (:h1 (:i :class "icon20 i-dashboard") " All Tables"))
      (:div :class "row"
        (output-all-database-tables-as-html)))))

;; EOF
