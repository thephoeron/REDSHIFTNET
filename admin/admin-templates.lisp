;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: admin-templates.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(defparameter *admin-header-logo* (string "/static/images/redshiftnet_header_logo.png"))
(defparameter *admin-login-logo* (string "/static/images/redshiftnet_text_logo.png"))

;; Admin Login page
(defparameter admin-login-styles 
  (list "/static/css/app.css"
        "/static/css/custom.css"
        "/static/css/pages/login.css"))
(defparameter admin-login-scripts 
  (list "/static/js/plugins/forms/uniform/jquery.uniform.min.js"
        "/static/js/plugins/forms/validation/jquery.validate.js"
        "/static/js/app.js"
        "/static/js/pages/login.js"))
;; Admin Dashboard
(defparameter admin-dashboard-styles
  (list "/static/js/plugins/tables/datatables/media/css/jquery.dataTables.css"
        "/static/css/app.css"
        "/css/app.css"
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
        "/static/js/pages/calendar.js"))

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

;; EOF
