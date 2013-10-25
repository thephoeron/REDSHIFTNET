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

(defun admin-login ()
  "Login form for admin section."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    )
  )

(defun admin-dashboard ()
  "Admin site dashboard widget generator function."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    )
  )

(defun %basic-admin-app-page ()
  )

(defun %admin-app-page ()
  )

;; admin pages return nil unless inside an ssl vhost defrequest
(defmacro %admin-auth-page ((title login-page-fun) &body body)
  "Core auth-page template."
  `(when (hunchentoot:ssl-p)
     (if (null (hunchentoot:session-value 'token))
         (cond ((eql :post (hunchentoot:request-method*))
                (let ((username (trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "username"))))
                      (password (trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "password")))))
                  (if (validate-credentials username password)
                      (progn (create-new-session username)
                             (,@body))
                      (%basic-admin-app-page (:title "Login Failed")
                        (show-all-messages)
                        (,@login-page-fun)))))
               ((eql :get (hunchentoot:request-method*))
                (progn (hunchentoot:start-session)
                       (%basic-admin-app-page (:title "Login")
                         (cl-who:with-html-output (hunchentoot::*standard-output*)
                         (,@login-page-fun)))))
               (t (hunchentoot:redirect "/403/")))
         ;; else
         (let* ((token (hunchentoot:session-value 'token))
                (the-sesh (postmodern:get-dao 'public-session token))
                (the-user (public-session-user-id token)))
           (if (validate-session token)
               (progn (update-public-session-exp-date (local-time:format-timestring nil (local-time:now)) token)
                      (create-new-session the-user)
                      (cl-who:with-html-output (hunchentoot::*standard-output*)
                        ,@body))
               (progn (%basic-admin-app-page (:title "Error: Validation Failure")
                        (show-all-messages)
                        (cl-who:with-html-output (hunchentoot::*standard-output*)
                          (,@login-page-fun)))))))))

(defmacro admin-page ((title login-page-fun) &body body)
  "Admin site page generator macro."
  `(%admin-auth-page ,name (uri title ,@login-page-fun)
     (%admin-app-page ()
      (admin-header title)
      (admin-menu)
      (cl-who:with-html-output (hunchentoot::*standard-output*)
        ,@body)
      (admin-footer))))

(defrequest rsn-admin (:vhost *ssl-vhost*)
  (admin-page ("REDSHIFTNET Dashboard" #'admin-login)
     (admin-dashboard)))

;; EOF
