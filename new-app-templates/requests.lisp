;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: requests.lisp

(in-package :{APPNAME})

;; Define your request/return functions here

;; WWW-INDEX

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
