;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: redshiftnet.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;; REDSHIFTNET SERVER

(defun rsn-start (&key (www-port 8080) (ssl-port 8090))
    "Server Start function for REDSHIFTNET"
    (ensure-directories-exist *www-acc-log*)
    (ensure-directories-exist *www-msg-log*)
    (ensure-directories-exist *ssl-acc-log*)
    (ensure-directories-exist *ssl-msg-log*)
    ;(postmodern:connect-toplevel *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
    (setf (port vhost-web) www-port
          (port vhost-ssl) ssl-port)
    (hunchentoot:start vhost-web)
    (hunchentoot:start vhost-ssl)
    (format t "~%REDSHIFTNET Started and Running:~%       WEB: ~W~%       SSL: ~W" vhost-web vhost-ssl))

(defun rsn-stop ()
    "Server Stop function for REDSHIFTNET"
    (when (or vhost-web vhost-ssl)
      ;(postmodern:disconnect-toplevel)
      (format t "~%REDSHIFTNET running on acceptors ~W and ~W" (hunchentoot:stop vhost-web) (hunchentoot:stop vhost-ssl)))
    (format t "~%REDSHIFTNET Stopped successfully..."))

(defun rsn-restart (&key (www-port 8080) (ssl-port 8090))
  "Restart Server function for REDSHIFTNET.  Caveat programmer: Assumes SBCL environment and Quicklisp installed."
  (rsn-stop)
  (ql:quickload "redshiftnet")
  (rsn-start :www-port www-port :ssl-port ssl-port)
  (sb-ext:gc :full t)
  (format t "~%REDSHIFTNET Restarted successfully..."))

;; EOF
