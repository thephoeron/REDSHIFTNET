;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: {APPNAME}.asd

(defpackage #:{APPNAME}-asd
  (:use :cl :asdf))

(in-package :{APPNAME}-asd)

(defsystem {APPNAME}
    :name "{APPNAME}"
    :version "0.0.1"
    :maintainer "Maintainer Name <email@example.org>"
    :author "Name <email@example.org>"
    :licence "MIT"
    :description "{APPNAME}"
    :depends-on (:redshiftnet)
    :components ((:file "packages")
                 (:file "config")
                 (:file "styles")
                 (:file "scripts")
                 (:file "db")
                 (:file "forms")
                 (:file "templates")
                 (:file "requests")))

;; EOF
