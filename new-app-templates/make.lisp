;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: make.lisp

(in-package :cl-user)

(require :asdf)
 
;; We're setting up a clean environment, so we have to specifically tell SBCL where
;; Quicklisp is installed
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Define current working directory
(defvar *this-dir* (pathname (directory-namestring #.(or *compile-file-truename* *load-truename*))))
(defvar *log-dir* (merge-pathnames "logs/" *this-dir*))
(defvar *swank-server*)
 
(ql:quickload '("com.dvlsoft.clon" "sb-daemon" "swank" "{APPNAME}"))
 
;; We need to shadow SBCL's (exit) over CLON's, for sanity
(shadowing-import '(exit) 'cl-user)
(use-package :com.dvlsoft.clon)
(nickname-package)
 
;; Use ``(defsynopsis)'' to define the command line options for your program and order of
;; elements in your help string
(defsynopsis (:postfix "...")
    (text :contents (format nil "~A: Command-Line Utility to run REDSHIFTNET powered web-app as a daemon." (string-upcase "{APPNAME}")))
  (group (:header "Web-app Config & Launch Options")
    (stropt :long-name "swank-port"
            :default-value "33010"
            :description "Port to run SWANK server for remote REPL access.  On shared hosts: make sure you reserve this port with a custom app, and secure the $#!% out of it.")
    ;; If you add additional VHOSTs to your web-app, you will need to add command-line arguments for them here
    (stropt :long-name "www-port" 
            :default-value "8080"
            :description "The port to launch the default HTTP server.")
    (stropt :long-name "ssl-port" 
            :default-value "8090"
            :description "The port to launch the default HTTPS server."))
  (group (:header "Immediate exit options:")
    (flag :short-name "h" :long-name "help"
      :description "Print this help and exit.")
    (flag :short-name "v" :long-name "version"
    :description "Print version number and exit.")))

(defvar *{APPNAME}-name* "{APPNAME}")
(defvar *{APPNAME}-title* (string-upcase "{APPNAME}"))
(defvar *{APPNAME}-version* "1.0 alpha")
(defvar *{APPNAME}-version-codename* "Quasar")

(defun {APPNAME}-version ()
  (format t "~A: v~A \"~A\"~%" *{APPNAME}-title* *{APPNAME}-version* *{APPNAME}-version-codename*))

(defun signal-handler (signal)
  (format t "~A received~%" signal)
  (clon::exit))

; ``(main)'' function for our standalone console program.
(defun main ()
  "Entry point for our standalone application."
  (format t "Welcome to ~A.~%" *{APPNAME}-title*)
  (make-context)
  ;; explicitly launch the help menu if it's called from command-line
  (when (getopt :short-name "h")
    (help)
    (clon::exit)) ;; since we shadowed CLON's (exit), we need to refer to it explicitly
  (when (getopt :short-name "v")
    ({APPNAME}-version)
    (clon::exit))
  ;; start server and web-app
  ({APPNAME}:{APPNAME}-start :www-port (parse-integer (getopt :long-name "www-port")) :ssl-port (parse-integer (getopt :long-name "ssl-port")))
  ;; fork off daemon subprocess, based on code from: http://wandrian.net/2012-02-20-1958-lisp-daemons-with-sbcl.html
  (let ((pid-file (format nil "~A.pid" *{APPNAME}-name*)))
    (sb-daemon:daemonize :exit-parent t
                         :output (merge-pathnames "stdout.log" *log-dir*)
                         :error (merge-pathnames "stderr.log" *log-dir*)
                         :pidfile (merge-pathnames pid-file *this-dir*)
                         :sigterm 'signal-handler
                         :sigabrt 'signal-handler
                         :sigint 'signal-handler))
  (setf *swank-server*
        (swank:create-server :port (parse-integer (getopt :long-name "swank-port"))
                             :coding-system "utf-8-unix"
                             :dont-close t))
  (loop (sleep 10))
  (terpri)
  (clon::exit))
 
;; Dump the Lisp image.
(dump "{APPNAME}" main)

;; EOF
