;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth-forms.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(define-rsn-form (auth-login-form :submit "Login" :action "")
  ((username text
    :icon "i-user"
    :validation
    ((not-blank?) "Your username is required"
     (longer-than? 2) "Your username must be longer than 2 characters"))
   (password password
    :icon "i-key"))
  (let ((the-user (cl-who:escape-string username))
        (the-pass (cl-who:escape-string password)))
    ;; need to insert validation here
    (push-success-msg (format nil "Thank you, ~A.  You have logged in successfully." the-user))
    (redirect (hunchentoot:referer))))

(define-rsn-form (auth-forgot-password-form :submit "Recover Password")
  ((username text
    :icon "i-user"
    :validation
    ((not-blank?) "Your username is required"
     (longer-than? 2) "Your username must be longer than 2 characters"))
   (email text
    :icon "i-envelop-2"
    :validation
    ((not-blank?) "Your email address is required"
     (is-email?) "The email address you entered does not appear to be valid.")))
  (let ((the-user (cl-who:escape-string username))
        (the-email (cl-who:escape-string email)))
    (push-success-msg (format nil "Thank you, ~A. Your temporary password has been sent to ~A." the-user the-email))))

;; EOF
