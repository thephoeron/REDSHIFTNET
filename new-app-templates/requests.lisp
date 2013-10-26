;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: requests.lisp

(in-package :{APPNAME})

;; Optional splash page
(define-easy-handler ({APPNAME}-splash-page :uri "/") ()
  (redirect "/index/"))

;; Define your request/return functions here

(defrequest www-index (:vhost *www-vhost*)
  ({APPNAME}-app-page ("{APPNAME} :: Welcome")
  	(cl-who:with-html-output (hunchentoot::*standard-output*)
  	  (:div :class "content"
  	  	(:h1 "Welcome to {APPNAME}, powered by REDSHIFTNET!")))))

(defrequest ssl-index (:vhost *ssl-vhost*)
  ({APPNAME}-app-page ("{APPNAME} :: Welcome")
  	(cl-who:with-html-output (hunchentoot::*standard-output*)
  	  (:div :class "content"
  	  	(:h1 "Welcome to {APPNAME}, powered by REDSHIFTNET and SSL!")))))

;; EOF
