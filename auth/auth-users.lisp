;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth-users.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; Create a new user
(defun create-new-user (username password firstname lastname email group)
  (let ((the-pass (hardened-password password)))
    (postmodern:insert-dao
      (make-instance 'public-user :username username
                                  :password the-pass
                                  :first-name firstname
                                  :last-name lastname
                                  :email email
                                  :group group
                                  :is-active t
                                  :last-modified (local-time:format-timestring nil (local-time:now))))))

(defun update-user ()
  )

(defun create-or-update-user ()
  )

;; EOF
