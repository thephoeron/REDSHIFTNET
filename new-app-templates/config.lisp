;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: config.lisp

(in-package :{APPNAME})

;;;; THIS FILE
(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

;;;; PRIMARY DATABASE CONNECTION
;;;; Assumes Postmodern and PostgreSQL 9.1+

(defvar *primary-db* "database-name")
(defvar *primary-db-user* "database-user")
(defvar *primary-db-pass* "database-password")
(defvar *primary-db-host* "database-hostname")
(defvar *primary-db-port* "5432")

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

(defparameter *static-folder* (merge-pathnames "static/" *default-directory*)
  "Static File Folder.  Caveat Programmer: Everything under this directory will be web accessible...")

;;;; Default SSL keys/certificates

(defvar *ssl-key* (merge-pathnames "cert/key.pem" *default-directory*))
(defvar *ssl-cert* (merge-pathnames "cert/certificate.pem" *default-directory*))
(defvar *ssl-req* "1234")

;; Define Hunchentoot log files
(defvar *www-acc-log* (merge-pathnames "logs/www-access.log" *default-directory*))
(defvar *www-msg-log* (merge-pathnames "logs/www-message.log" *default-directory*))
(defvar *ssl-acc-log* (merge-pathnames "logs/ssl-access.log" *default-directory*))
(defvar *ssl-msg-log* (merge-pathnames "logs/ssl-message.log" *default-directory*))

;;; DEFINE VHOST ACCEPTORS AND DISPATCH-TABLES
;;; Note: Easy-Handlers get pushed to every VHOST by default.  Useful for sharing resources between multiple toplevel and sub- domains.

(defvar www-vhost (make-instance 'rsn::vhost :address "localhost" :port 8080 :access-log-destination *www-acc-log* :message-log-destination *www-msg-log*))
(defvar ssl-vhost (make-instance 'rsn::ssl-vhost :address "localhost" :port 8090 :access-log-destination *ssl-acc-log* :message-log-destination *ssl-msg-log*
                                      :ssl-privatekey-file *ssl-key* :ssl-certificate-file *ssl-cert*))

;; Ghost Admin vhost acceptor and admin config
;; Comment this setf form out if you want to disable the admin site for your web-app
;; or turn it into a new ssl acceptor if you want to run it from a different top-level
;; domain or sub-domain, such as https://admin.yoursite.org/
(setf redshiftnet::vhost-admin ssl-vhost
      redshiftnet::*admin-header-logo* (string "/static/images/admin_header_logo.png")
      redshiftnet::*admin-login-logo* (string "/static/images/admin_login_logo.png"))


(setf (rsn::dispatch-table www-vhost)
      (list
        'hunchentoot:dispatch-easy-handlers
        (hunchentoot:create-folder-dispatcher-and-handler "/static/" *static-folder*)
        (create-static-file-dispatcher-and-handler "/favicon.ico" (make-pathname :name "favicon" :type "png" :version nil :defaults *this-file*)))
      (rsn::dispatch-table ssl-vhost)
      (list
        'hunchentoot:dispatch-easy-handlers
        (hunchentoot:create-folder-dispatcher-and-handler "/static/" *static-folder*)
        (create-static-file-dispatcher-and-handler "/favicon.ico" (make-pathname :name "favicon" :type "png" :version nil :defaults *this-file*))))

;; EOF
