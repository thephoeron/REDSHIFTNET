;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: admin.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; Admin header, footer, and menu template elements
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

;; Admin Login page
(defparameter admin-login-styles '("/static/css/admin/login.css"))
(defparameter admin-login-scripts 
  '("/static/js/plugins/forms/uniform/jquery.uniform.min.js"
    "/static/js/plugins/forms/validation/jquery.validate.js"
    "/static/js/pages/login.js"))

(define-rsn-form (admin-login-form :submit "Login" :general-validation (#'check-password "Bad password. Try again."))
  ((username text
    :validation
    ((not-blank?) "Your username is required"
     (longer-than? 2) "Your username must be longer than 2 characters"))
   (password password))
  (let ((the-user (cl-who:escape-string username))
        (the-pass (cl-who:escape-string password)))
    (push-success-msg (format nil "Thank you, ~A.  You have logged in successfully." the-user))))

(define-rsn-form (admin-forgot-password-form :submit "Recover Password")
  ((username text
    :validation
    ((not-blank?) "Your username is required"
     (longer-than? 2) "Your username must be longer than 2 characters"))
   (email text
    :validation
    ((not-blank?) "Your email address is required"
     (matches? "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$") "The email address you entered does not appear to be valid.")))
  (let ((the-user (cl-who:escape-string username))
        (the-email (cl-who:escape-string email)))
    (push-success-msg (format nil "Thank you, ~A. Your temporary password has been sent to ~A." the-user the-email))))

(defun admin-login ()
  "Login form for admin section."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:div :class "container-fluid"
      (:div :id "login"
        (:div :class "login-wrapper" :data-active "log"
          (:a :class "navbar-brand" :href "#"
            (:img :src "/static/images/rsn-text-logo.png"
                  :alt "REDSHIFTNET Admin" :class "image-responsive"))
          (:div :id "log"
            (:div :class "page-header"
              (:h3 :class "center" "Please Login"))
            (show-rsn-form admin-login-form))
          (:div :id "forgot"
            (:div :class "page-header"
              (:h3 :class "center" "Forgot Password"))
            (show-rsn-form admin-forgot-password-form)))
        (:div :id "bar" :data-active "log"
          (:div :class "btn-group btn-group-vertical"
            (:a :id "log" :href "#" :class "btn tipR" :title "Login"
              (:i :class "icon16 i-key"))
            (:a :id "forgot" :href "#" :class "btn tipR" :title "Forgot Password"
              (:i :class "icon16 i-question"))))
        (:div :class "clearfix")))))

;; Admin page generator macros
(defmacro %basic-admin-app-page ((&key (title "REDSHIFTNET Admin") (styles nil) (scripts nil)) &body body)
  "Basic, no frills Admin page function, useful for error pages, system notifications, login pages, wrapping AJAX html content, etc."
  `(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
      (:head
        (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
        (:meta :name "application-name" :content "REDSHIFTNET Admin")
        (:meta :name "author" :content "the Phoeron <//thephoeron.com/>")
        (:link :rel "stylesheet" :href "/static/css/bootstrap.min.css" :type "text/css" :media "screen")
        (:link :rel "stylesheet" :type "text/css" :href "/static/css/bootstrap/bootstrap-theme.css")
        (:link :rel "stylesheet" :type "text/css" :href "/static/css/icons.css")
        (:link :rel "stylesheet" :type "text/css" :href "/static/js/plugins/forms/uniform/uniform.default.css")
        "<!--[if IE 8]><link href=\"css/ie8.css\" rel=\"stylesheet\" type=\"text/css\" /><![endif]-->"
        (:link :rel "apple-touch-icon-precomposed" :sizes "144x144" :href "/static/images/ico/apple-touch-icon-144-precomposed.png")
        (:link :rel "apple-touch-icon-precomposed" :sizes "114x114" :href "/static/images/ico/apple-touch-icon-114-precomposed.png")
        (:link :rel "apple-touch-icon-precomposed" :sizes "72x72" :href "/static/images/ico/apple-touch-icon-72-precomposed.png")
        (:link :rel "apple-touch-icon-precomposed" :href "/static/images/ico/apple-touch-icon-57-precomposed.png")
        (:link :rel "shortcut icon" :href "/static/images/ico/favicon.png")
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
        (:script :type "text/javascript" :src "/static/js/conditionizr.min.js")
        (:script :type "text/javascript" :src "/static/js/plugins/core/nicescroll/jquery.nicescroll.min.js")
        (:script :type "text/javascript" :src "/static/js/plugins/core/jrespond/jRespond.min.js")
        (:script :type "text/javascript" :src "/static/js/jquery.redshiftnetAdmin.js")
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

;; Admin auth page macro
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
                      (%basic-admin-app-page (:title "Login Failed"
                                              :scripts admin-login-scripts
                                              :styles admin-login-styles)
                        (show-all-messages)
                        (,@login-page-fun)))))
               ((eql :get (hunchentoot:request-method*))
                (progn (hunchentoot:start-session)
                       (%basic-admin-app-page (:title "Login"
                                               :scripts admin-login-scripts
                                               :styles admin-login-styles)
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
               (progn (%basic-admin-app-page (:title "Error: Validation Failure"
                                              :scripts admin-login-scripts
                                              :styles admin-login-styles)
                        (show-all-messages)
                        (cl-who:with-html-output (hunchentoot::*standard-output*)
                          (,@login-page-fun)))))))))

(defmacro admin-page ((title login-page-fun &key (styles nil) (scripts nil)) &body body)
  "Admin site page generator macro."
  `(%admin-auth-page ,name (uri title ,@login-page-fun)
     (%admin-app-page (:title ,title :header #'admin-header
                       :menu #'admin-menu :footer #'admin-footer
                       :scripts ,@scripts :styles ,@styles)
      (cl-who:with-html-output (hunchentoot::*standard-output*)
        ,@body))))

;; Admin Dashboard
(defparameter admin-dashboard-styles '())
(defparameter admin-dashboard-scripts '())

(defun admin-dashboard ()
  "Admin site dashboard widget generator function."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    )
  )

(defrequest rsn-admin (:vhost *ssl-vhost*)
  (admin-page ("REDSHIFTNET Admin :: Dashboard" #'admin-login
               :styles admin-dashboard-styles
               :scripts admin-dashboard-scripts)
     (admin-dashboard)))

;; EOF
