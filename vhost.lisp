;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: vhost.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;; REDSHIFTNET VHOST FUNCTIONALITY
;;; Also need to set up EASY-VIRTUAL-HANDLERS....

;; http vhost
(defclass vhost (hunchentoot:acceptor)
  ;; slots
  ((dispatch-table :initform '() :accessor dispatch-table
                   :documentation "List of dispatch functions")
   (port :initarg :port :accessor port :documentation "Listener port"))
  ;; options
  (:default-initargs
   :address "127.0.0.1"
   :port 8080))

;; https vhost
(defclass ssl-vhost (hunchentoot:ssl-acceptor)
  ;; slots
  ((dispatch-table :initform '() :accessor dispatch-table
                   :documentation "List of SSL Dispatch functions")
   (port :initarg :port :accessor port :documentation "Listener port"))
  ;; options
  (:default-initargs
   :address "127.0.0.1"
   :port 8090))

;;; Specialise ACCEPTOR-DISPATCH-REQUEST for VHOSTs
(defmethod hunchentoot:acceptor-dispatch-request ((vhost vhost) request)
  ;; try REQUEST on each dispatcher in turn
  (mapc (lambda (dispatcher)
      (let ((handler (funcall dispatcher request)))
        (when handler               ; Handler found. FUNCALL it and return result
          (return-from hunchentoot:acceptor-dispatch-request (funcall handler)))))
    (dispatch-table vhost))
  (call-next-method))

;;; Specialize ACCEPTOR-DISPATCH-REQUEST for SSL-VHOSTs
(defmethod hunchentoot:acceptor-dispatch-request ((ssl-vhost ssl-vhost) request)
    ;; try REQUEST on each dispatcher in turn
    (mapc (lambda (dispatcher)
        (let ((handler (funcall dispatcher request)))
            (when handler
                (return-from hunchentoot:acceptor-dispatch-request (funcall handler)))))
        (dispatch-table ssl-vhost))
    (call-next-method))

;;; ======================================================================
;;; Now all we need to do is test it

;;; Instantiate VHOSTs
; (defvar vhost1 (make-instance 'vhost :port 50001))
; (defvar vhost2 (make-instance 'vhost :port 50002))

; ;;; Populate each dispatch table
; (push
;  (hunchentoot:create-prefix-dispatcher "/foo" 'foo1)
;  (dispatch-table vhost1))
; (push
;  (hunchentoot:create-prefix-dispatcher "/foo" 'foo2)
;  (dispatch-table vhost2))

; ;;; Define handlers
; (defun foo1 () "Hello")
; (defun foo2 () "Goodbye")

; ;;; Start VHOSTs
; (hunchentoot:start vhost1)
; (hunchentoot:start vhost2)

;; EOF
