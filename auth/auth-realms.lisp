;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;; file: auth-realms.lisp

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; Realms are an abstraction of the VHOST functionality, so you can separate
;; and define your users according to the areas of the web-app for which they
;; are meant to have access

;; Create a new realm
(defun create-new-realm (name)
  (let ((the-realm (make-instance 'rsn-auth-realm :name name)))
    (postmodern:insert-dao the-realm)))

(defun update-realm ()
  )

(defun create-or-update-realm ()
  )

;; EOF
