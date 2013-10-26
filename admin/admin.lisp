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

(defmacro %basic-admin-app-page ((&key (title "REDSHIFTNET Admin") (styles nil) (scripts nil)) &body body)
  "Basic, no frills Admin page function, useful for error pages, system notifications, login pages, wrapping AJAX html content, etc."
  `(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
      (:head
        (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
        (:link :rel "stylesheet" :href "/static/css/bootstrap.min.css" :type "text/css" :media "screen")
        (:link :rel "stylesheet" :href "/redshiftnet.css" :type "text/css" :media "screen")
        (:link :rel "stylesheet" :href "/admin.css" :type "text/css" :media "screen")
        ,@(mapcar (lambda (file)
                    `(:link :type "text/css" :rel "stylesheet" :media "screen"
                            :href ,(format nil "~A" file)))
                     styles)
        (:title ,title)
        "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
      (:body
        ,@body
        (:script :type "text/javascript" :src "/static/js/jquery-1.9.1.min.js")
        (:script :type "text/javascript" :src "/static/js/bootstrap.min.js")
        (:script :type "text/javascript" :src "/redshiftnet.js")
        (:script :type "text/javascript" :src "/admin.js")
        ,@(mapcar (lambda (file)
                    `(:script :type "text/javascript"
                              :src ,(format nil "~A" file)))
                  scripts)))))

(defmacro %admin-app-page ((&key (title "REDSHIFTNET Admin") (styles nil) (scripts nil) header menu footer) &body body)
  "Standard app page template."
  `(%basic-admin-app-page (:title ,title :styles ,@styles :scripts ,@scripts)
    (cl-who:with-html-output (hunchentoot::*standard-output*)
      (:header :id "header"
        (,@header ,title))
      (:div :class "main"
        (:aside :id "sidebar" (,@menu))
        (:div :id "content" ,@body))
      (:footer :id "footer"
        (,@footer)))))

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
     (%admin-app-page (:title ,title :header #'admin-header
                       :menu #'admin-menu :footer #'admin-footer)
      (cl-who:with-html-output (hunchentoot::*standard-output*)
        ,@body))))

(defrequest rsn-admin (:vhost *ssl-vhost*)
  (admin-page ("REDSHIFTNET Admin :: Dashboard" #'admin-login)
     (admin-dashboard)))

;; EOF
