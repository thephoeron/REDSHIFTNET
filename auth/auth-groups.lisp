;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;; file: auth-groups.lisp

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; Create a new group
(defun create-new-group (name realm)
  (postmodern:with-connection *db*
    (let* ((the-realm-id (get-realm-id-by-name realm))
           (the-group (make-instance 'rsn-auth-group :name name :realm-id the-realm-id)))
      (postmodern:insert-dao the-group))))

(defun update-group ()
  (postmodern:with-connection *db*
    nil))

(defun create-or-update-group ()
  (postmodern:with-connection *db*
    nil))

;; EOF
