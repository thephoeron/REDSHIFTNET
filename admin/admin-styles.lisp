;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: admin-styles.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;;; Contents

(define-easy-handler (redshiftnet-css :uri "/css/pages/dashboard.css") ()
  (setf (content-type*) "text/css")
  (css-lite:css
    ;; Admin Dashboard specific styles
    ))

;; EOF
