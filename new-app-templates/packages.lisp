;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: packages.lisp

(in-package :cl-user)

(defpackage #:{APPNAME}
  (:use :cl :cl-who :parenscript :postmodern :hunchentoot :redshiftnet)
  (:shadowing-import-from :parenscript #:%)
  (:export #:{APPNAME}-start
           #:{APPNAME}-stop
           #:{APPNAME}-restart)
  (:documentation
   "A REDSHIFTNET-powered web application."))

;; EOF
