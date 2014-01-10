;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;; file: auth-templates.lisp

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(defun auth-login (logo)
  "Login for secure front-end pages."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:div :class "container-fluid"
      (:div :id "login"
        (:div :class "login-wrapper" :data-active "log"
          (:a :class "navbar-brand" :href "#"
            (:img :class "image-responsive" :alt "REDSHIFTNET" :src (str (format nil "~A" logo))))
          (:div :id "log"
            (:div :class "page-header"
              (:h3 :class "center" "Please Login"))
            (show-rsn-form auth-login-form))
          (:div :id "forgot"
            (:div :class "page-header"
              (:h3 :class "center" "Forgot Password"))
            (show-rsn-form auth-forgot-password-form)))
        (:div :id "bar" :data-active "log"
          (:div :class "btn-group btn-group-vertical"
            (:a :id "log" :href "#" :class "btn tipR" :title "Login"
              (:i :class "icon16 i-key"))
            (:a :id "forgot" :href "#" :class "btn tipR" :title "Forgot Password"
              (:i :class "icon16 i-question"))))
        (:div :class "clearfix")))))

;; auth page macro -- returns nil unless inside an ssl vhost defrequest
(defmacro %auth-page ((title lpf) &body body)
  "Core auth-page template."
  `(postmodern:with-connection (list ,*primary-db* ,*primary-db-user* ,*primary-db-pass* ,*primary-db-host*)
    (when (hunchentoot:ssl-p)
     (if (null (hunchentoot:session-value 'token))
         (cond ((eql :post (hunchentoot:request-method*))
                (let ((username (trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "username"))))
                      (password (trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "password")))))
                  (if (validate-credentials username password)
                      (progn (create-new-session username)
                             ,@body)
                      (%basic-app-page (:title "Login Failed"
                                        :scripts ,@(list login-scripts)
                                        :styles ,@(list login-styles))
                        (show-all-messages)
                        (funcall ,lpf *login-logo*)))))
               ((eql :get (hunchentoot:request-method*))
                (progn (hunchentoot:start-session)
                       (%basic-app-page (:title "Login"
                                         :scripts ,@(list login-scripts)
                                         :styles ,@(list login-styles))
                         (cl-who:with-html-output (hunchentoot::*standard-output*)
                           (funcall ,lpf *login-logo*)))))
               (t (hunchentoot:redirect "/403/")))
         ;; else
         (let* ((token (hunchentoot:session-value 'token))
                (sesh-id (get-session-id-by-token token))
                (the-sesh (postmodern:get-dao 'rsn-auth-session sesh-id))
                (user-id (user-id the-sesh))
                (the-user (postmodern:get-dao 'rsn-auth-user user-id)))
           (if (validate-session token)
               (progn
                 (setf (expiry-date the-sesh)
                       (local-time:format-timestring nil (local-time:now)))
                 (postmodern:update-dao the-sesh)
                 (create-new-session (username the-user))
                 (cl-who:with-html-output (hunchentoot::*standard-output*)
                   ,@body))
               (progn
                 (%basic-app-page (:title "Error: Validation Failure"
                                   :scripts ,@(list login-scripts)
                                   :styles ,@(list login-styles))
                   (show-all-messages)
                   (cl-who:with-html-output (hunchentoot::*standard-output*)
                     (funcall ,lpf *login-logo*))))))))))

(defmacro auth-page ((title login-page-fun &key (styles nil) (scripts nil)) &body body)
  "Authenticated page generator macro."
  `(%auth-page (title ,login-page-fun)
     (%app-page (:title ,title :header #'%app-header
                 :menu #'%app-menu :footer #'%app-footer
                 :scripts ,scripts :styles ,styles)
      (cl-who:with-html-output (hunchentoot::*standard-output*)
        ,@body))))

;; EOF
