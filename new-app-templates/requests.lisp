;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: requests.lisp

(in-package :{APPNAME})

;; Define your request/return functions here

;; WWW-INDEX
(defparameter www-index-styles (list "/static/css/custom.css"
                                     "/static/css/app.css"))
(defparameter www-index-scripts (list "/static/js/pages/dashboard.js"
                                      "/static/js/app.js"))

(defrequest www-index (:vhost www-vhost)
  (%{APPNAME}-app-page (:title "{APPNAME} :: Welcome"
                       :header #'rsn::admin-header :menu #'rsn::admin-menu
                       :footer #'rsn::admin-footer
                       :scripts www-index-scripts
                       :styles www-index-styles)
    (cl-who:with-html-output (hunchentoot::*standard-output*)
      (:div :class "content"
        (:h1 "Welcome to {APPNAME}, powered by REDSHIFTNET!")))))

;; SSL-INDEX -- inner app-page macro should be wrapped in an auth-page
(defparameter ssl-index-styles (list "/static/css/custom.css"
                                     "/static/css/app.css"))
(defparameter ssl-index-scripts (list "/static/js/pages/dashboard.js"
                                      "/static/js/app.js"))

(defrequest ssl-index (:vhost ssl-vhost)
  (%{APPNAME}-app-page (:title "{APPNAME} :: Welcome"
                       :header #'rsn::admin-header :menu #'rsn::admin-menu
                       :footer #'rsn::admin-footer
                       :scripts ssl-index-scripts
                       :styles ssl-index-styles)
    (cl-who:with-html-output (hunchentoot::*standard-output*)
      (:div :class "content"
        (:h1 "Welcome to {APPNAME}, powered by REDSHIFTNET and SSL!")))))

;; Optional splash page for all acceptors
(define-easy-handler ({APPNAME}-splash-page :uri "/") ()
  (redirect "/index/"))

;; EOF
