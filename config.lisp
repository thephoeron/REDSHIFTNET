;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: config.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;;; THIS FILE
(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

;;;; PRIMARY DATABASE CONNECTION
;;;; Assumes Postmodern and PostgreSQL 9.1+

(defparameter *primary-db* "database-name")
(defparameter *primary-db-user* "database-user")
(defparameter *primary-db-pass* "database-password")
(defparameter *primary-db-host* "database-hostname")
(defparameter *primary-db-port* "5432")

;;;; DEFAULT ENVIRONMENT VARIABLES
;;;; For Hunchentoot, Parenscript, and CSS-Lite

(setf ;; for utf-8
      hunchentoot:*default-content-type* "text/html; charset=utf-8"
      ;; for debug mode, set to 't'
      hunchentoot:*catch-errors-p* t
      hunchentoot:*session-secret* (generate-new-session-token)
      hunchentoot:*session-max-time* 1209600 ;; 2 weeks in seconds
      (cl-who:html-mode) :html5
      ps:*js-string-delimiter* #\'
      css-lite:*indent-css* 4
      cl-ppcre:*allow-named-registers* t)

(defparameter *default-directory*
    (pathname (directory-namestring #.(or *compile-file-truename*
                                          *load-truename*)))
    "The directory path of the current file.")

(defparameter *static-folder* (merge-pathnames "new-app-templates/static/" *default-directory*)
	"Static File Folder.  Caveat Programmer: Everything under this directory will be web accessible...")

;;;; Default SSL keys/certificates

(defparameter *ssl-key* (merge-pathnames "auth/key.pem" *default-directory*))
(defparameter *ssl-cert* (merge-pathnames "auth/certificate.pem" *default-directory*))
(defparameter *ssl-req* "1234")

; (setf hunchentoot:*dispatch-table*
;     (list
;         'hunchentoot:dispatch-easy-handlers
;         (hunchentoot:create-folder-dispatcher-and-handler "/static/" *static-folder*)))

;; Define Hunchentoot log files
(defparameter *www-acc-log* (merge-pathnames "log/www-access.log" *default-directory*))
(defparameter *www-msg-log* (merge-pathnames "log/www-message.log" *default-directory*))
(defparameter *ssl-acc-log* (merge-pathnames "log/ssl-access.log" *default-directory*))
(defparameter *ssl-msg-log* (merge-pathnames "log/ssl-message.log" *default-directory*))

(defparameter *error-page-dir* (merge-pathnames "errors/" *default-directory*))

;;; DEFINE VHOST ACCEPTORS AND DISPATCH-TABLES
;;; Note: Easy-Handlers get pushed to every VHOST by default.  Useful for sharing resources between toplevel domains.

(defparameter vhost-web (make-instance 'vhost
                                       :address "0.0.0.0" :port 8080
                                       :access-log-destination *www-acc-log*
                                       :message-log-destination *www-msg-log*
                                       :error-template-directory *error-page-dir*))
(defparameter vhost-ssl (make-instance 'ssl-vhost
                                       :address "0.0.0.0" :port 8090
                                       :access-log-destination *ssl-acc-log*
                                       :message-log-destination *ssl-msg-log*
                                       :error-template-directory *error-page-dir*
                                       :ssl-privatekey-file *ssl-key*
                                       :ssl-certificate-file *ssl-cert*))
;; Ghost Admin vhost acceptor
(defparameter vhost-admin vhost-ssl)

(setf (dispatch-table vhost-web)
      (list
        'hunchentoot:dispatch-easy-handlers
        (hunchentoot:create-folder-dispatcher-and-handler "/static/" *static-folder*)
        (create-static-file-dispatcher-and-handler "/favicon.ico" (make-pathname :name "favicon" :type "png" :version nil :defaults *this-file*)))
      (dispatch-table vhost-ssl)
      (list
        'hunchentoot:dispatch-easy-handlers
        (hunchentoot:create-folder-dispatcher-and-handler "/static/" *static-folder*)
        (create-static-file-dispatcher-and-handler "/favicon.ico" (make-pathname :name "favicon" :type "png" :version nil :defaults *this-file*))))
;; EOF
