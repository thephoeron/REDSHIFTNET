;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;;file: redshiftnet.asd

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage redshiftnet-asd
  (:nicknames #:rsn-op)
  (:use :cl :asdf)
  (:export #:server-type #:server-version
           #:defrequest #:basic-app-page #:app-page
           #:*rsn-version* #:make-app #:make-app-op))

(in-package :redshiftnet-asd)

(defparameter *rsn-version* "0.1.0r1")

(export '*rsn-version*)

(defsystem redshiftnet
  :version #.*rsn-version*
  :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "MIT"
  :description "Common Lisp Web Application Framework for enterprise data-driven apps that require hardened security and high performance."
  :serial t
  :depends-on (:cl-isaac
               :let-over-lambda
               :uber-shell
               :cl-fad
               :cl-smtp
               :cl-who
               :css-lite
               :cl-css
               :parenscript
               :cl-json
               :yason
               :split-sequence
               :cl-ppcre
               :local-time
               :postmodern
               :submarine
               :ironclad
               :babel
               :hunchentoot
               :docutils)
  :components ((:file "packages")
               (:file "vhost")
               (:file "utils")
               (:file "db-utils")
               (:module "ui"
                :serial t
                :components ((:file "packages")
                             (:file "ui-grid")
                             (:file "ui")))
               (:file "scripts")
               (:file "styles")
               (:file "web")
               (:file "templates")
               (:module "forms"
                :serial t
                :components ((:file "forms-utils")
                             (:file "forms-recaptcha")
                             (:file "forms-macros")
                             (:file "forms")))
               (:module "auth"
                :serial t
                :components ((:file "auth-db")
                             (:file "auth-realms")
                             (:file "auth-groups")
                             (:file "auth-users")
                             (:file "auth-sessions")
                             (:file "auth-forms")
                             (:file "auth-templates")
                             (:file "auth-requests")
                             (:file "auth")))
               (:file "config")
               (:module "admin"
                :serial t
                :components ((:file "admin-db")
                             (:file "admin-styles")
                             (:file "admin-scripts")
                             (:file "admin-forms")
                             (:file "admin-templates")
                             (:file "admin-requests")
                             (:file "admin")))
               (:module "blog"
                :serial t
                :components ((:file "blog-db")
                             (:file "blog-requests")
                             (:file "blog-admin")
                             (:file "blog")))
               (:file "redshiftnet")
               (:file "make-new-app"))
  :in-order-to ((make-app-op (load-op "redshiftnet"))))

;;;; Stolen from Weblocks
;;;; make-app-op operation
(defclass make-app-op (operation)
  ()
  (:documentation "Allows to specialize built-in ASDF methods to create
   a new REDSHIFTNET app."))

(defmethod perform ((o make-app-op) (c component))
  "Creates a new REDSHIFTNET application"
  nil)

(defmethod perform ((o make-app-op) (c (eql (find-system :redshiftnet))))
  "Creates a new REDSHIFTNET application when (rsn-op:make-app 'name \"/path/to/target/\")
   is called."
  (let ((app-name (cadr (member :name (asdf::operation-original-initargs o))))
        (app-target (cadr (member :target (asdf::operation-original-initargs o)))))
    (funcall (intern (symbol-name :make-application) (find-package :redshiftnet))
             app-name app-target)))

(defmethod operation-done-p ((o make-app-op) (c (eql (find-system :redshiftnet))))
  nil)

;;;; helper function that hides away the unnecessary arguments to
;;;; (asdf:operate)

(defun make-app (name &optional target)
  "Creates a new REDSHIFTNET app named <name> into directory <target> 
   based on the new-app-template.  <target> defaults to ~/quicklisp/local-projects/"
  (asdf:operate 'make-app-op :redshiftnet :name name :target target))

;; EOF
