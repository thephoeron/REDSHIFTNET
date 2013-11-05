;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: {APPNAME}.lisp

(in-package :{APPNAME})

(defun {APPNAME}-start ()
    "Server Start function for {APPNAME}"
    (ensure-directories-exist *www-acc-log*)
    (ensure-directories-exist *www-msg-log*)
    (ensure-directories-exist *ssl-acc-log*)
    (ensure-directories-exist *ssl-msg-log*)
    (postmodern:connect-toplevel *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
    (hunchentoot:start www-vhost)
    (hunchentoot:start ssl-vhost)
    (format t "~%~A started and running -- WWW: ~W SSL: ~W" (string-upcase "{APPNAME}") www-vhost ssl-vhost))

(defun {APPNAME}-stop ()
    "Server Stop function for {APPNAME}"
    (when (or www-vhost ssl-vhost)
      (postmodern:disconnect-toplevel)
      (format t "~%~A running on acceptors ~W and ~W~%" (string-upcase "{APPNAME}") (hunchentoot:stop www-vhost) (hunchentoot:stop ssl-vhost)))
    (format t "~%~A stopped successfully...~%" (string-upcase "{APPNAME}")))

(defun {APPNAME}-restart ()
  "Restart Server function for {APPNAME}.  Caveat programmer: Assumes SBCL environment and Quicklisp installed."
  ({APPNAME}-stop)
  (ql:quickload "{APPNAME}")
  ({APPNAME}-start)
  (sb-ext:gc :full t)
  (format t "~%{APPNAME} restarted successfully..."))

;; EOF
