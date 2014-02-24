;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: admin-requests.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

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
  (admin-page ("REDSHIFTNET Admin :: Admin" #'admin-login
               :styles admin-dashboard-styles
               :scripts admin-dashboard-scripts)
     (hunchentoot:redirect "/admin/dashboard/")))

(defrequest rsn-admin/dashboard (:vhost vhost-admin)
  (admin-page ("REDSHIFTNET Admin :: Dashboard" #'admin-login
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
  (postmodern:with-connection *db*
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
 (postmodern:with-connection *db*
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
