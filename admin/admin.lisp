;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: admin.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(defun admin-header (title)
  "Template block for admin site header."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    )
  )

(defun admin-footer ()
  "Template block for admin site footer."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    )
  )

(defun admin-menu ()
  "Admin Site menu generator function."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    )
  )

(defun admin-dashboard ()
  "Admin site dashboard widget generator function."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    )
  )

(defmacro admin-page (name uri title &body body)
  "Admin site page generator macro."
  `(auth-page (,name uri title)
      (admin-header title)
      (admin-menu)
      (,@body)
      (admin-footer)))

(defrequest rsn-admin (:vhost *ssl-vhost*)
  (admin-page ()
     (admin-dashboard)))

;; EOF
