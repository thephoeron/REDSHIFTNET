;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: redshiftnet.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;; REDSHIFTNET SERVER

(defun rsn-start ()
    "Server Start function for REDSHIFTNET"
    ; (when *acc*
    ;     (format t "REDSHIFTNET Already Running on ~W... Stopping First...~%" *acc*)
    ;     (hunchentoot:stop *acc*)
    ;     (setf *acc* nil)
    ;     (format t "REDSHIFTNET Stopped successfully.~%"))
    (ensure-directories-exist *www-acc-log*)
    (ensure-directories-exist *www-msg-log*)
    (ensure-directories-exist *ssl-acc-log*)
    (ensure-directories-exist *ssl-msg-log*)
    (postmodern:connect-toplevel pm-db pm-user pm-pass pm-host)
    (hunchentoot:start www-vhost)
    (hunchentoot:start ssl-vhost)
    (format t "REDSHIFTNET Started and Running:~%       WWW: ~W~%          SSL: ~W~%" www-vhost ssl-vhost))

(defun rsn-stop ()
    "Server Stop function for REDSHIFTNET"
    (when (or www-vhost ssl-vhost)
      (postmodern:disconnect-toplevel)
      (format t "REDSHIFTNET running on acceptors ~W and ~W~%" (hunchentoot:stop www-vhost) (hunchentoot:stop ssl-vhost)))
    (format t "REDSHIFTNET Stopped successfully...~%"))

(defun rsn-restart ()
  "Restart Server function for REDSHIFTNET.  Caveat programmer: Assumes SBCL environment and Quicklisp installed."
  (rsn-stop)
  (ql:quickload "redshiftnet")
  (rsn-start)
  (sb-ext:gc :full t)
  (format nil "REDSHIFTNET Restarted successfully...~%"))

;; EOF
