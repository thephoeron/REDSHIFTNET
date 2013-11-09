;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(hunchentoot:define-easy-handler (rsn-auth-css :uri "/rsn-auth.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    (("body")
      (:padding-top "40px"
       :padding-bottom "40px"
       :background-color "#f5f5f5"))
    ((".form-signin")
      (:max-width "300px"
       :padding "19px 29px 29px"
       :margin "0 auto 20px"
       :background-color "#fff"
       :border "1px solid #e5e5e5"
       :-webkit-border-radius "5px"
       :-moz-border-radius "5px"
       :border-radius "5px"
       :-webkit-box-shadow "0 1px 2px rgba(0,0,0,.05)"
       :-moz-box-shadow "0 1px 2px rgba(0,0,0,.05)"
       :box-shadow "0 1px 2px rgba(0,0,0,.05)")
      ((".form-signin-heading")
        (:margin-bottom "0"))
      (("input[type=\"text\"], input[type=\"password\"]")
        (:font-size "16px"
         :height "auto"
         :margin-bottom "15px"
         :padding "7px 9px")))))

;; EOF
