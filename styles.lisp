;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: web.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;;; Contents
;;;; * Shared Styles -- 24
;;;; * Typography
;;;; * Bootstrap custom styles
;;;; * Page structure
;;;; * Off-canvas styles
;;;; * Custom Panels
;;;; * Plugin custom styles
;;;; * CSS Animations
;;;; * Login Page
;;;; * Error & Offline pages
;;;; * Media Queries

(define-easy-handler (redshiftnet-css :uri "/css/app.css") ()
  (setf (content-type*) "text/css")
  (css-lite:css
    ;; Shared styles
    (("html")
      (:background "url(/static/images/patterns/debut_light.png) repeat"
       :height "100%"))
    (("body")
      (:min-height "100%"
       :display "inline-block"
       :overflow-x "hidden"))
    (("*")
      (:-webkit-box-sizing "border-box"
       :-moz-box-sizing "border-box"
       :box-sizing "border-box"
       :outline "0 !important"))
    ;; Typography
    ;; Bootstrap custom styles
    ;; Page structure
    ;; Off-canvas styles
    ;; Custom panels
    ;; Plugin custom styles
    ;; CSS Animations
    ;; Login Page
    ;; Error & Offline pages
    ;; Media Queries
    ))

;; EOF
