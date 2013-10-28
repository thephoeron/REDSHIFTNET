;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: packages.lisp

(in-package :cl-user)

(defpackage #:{APPNAME}
  (:use :cl :cl-who :hunchentoot :redshiftnet)
  (:export #:{APPNAME}-start
           #:{APPNAME}-stop
           #:{APPNAME}-restart)
  (:documentation
   "A REDSHIFTNET-powered web application."))

;; EOF
