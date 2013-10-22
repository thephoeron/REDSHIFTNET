;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: {APPNAME}.lisp

(defpackage #:{APPNAME}
  (:use :cl :redshiftnet)
  (:import-from :hunchentoot #:header-in
		#:set-cookie #:set-cookie* #:cookie-in
		#:user-agent #:referer)
  (:documentation
   "A REDSHIFTNET-powered web application."))

(in-package :{APPNAME})

