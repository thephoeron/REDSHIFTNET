;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;; file: auth-sessions.lisp

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; generate a cryptographic random 512-bit session token with CL-ISAAC
(defun generate-new-session-token ()
  (let ((ctx (isaac:init-kernel-seed :is64 t)))
    (format nil "~64,'0x" (isaac:rand-bits-64 ctx 512))))

;; Validate the passed session token and user attached to it
(defun validate-session (token)
  "Validate session with passed token string."
  (postmodern:with-connection *db*
    (let* ((sesh-id (get-session-id-by-token token))
           (the-sesh (postmodern:get-dao 'rsn-auth-session sesh-id))
           (exp-date (local-time:universal-to-timestamp (expiry-date the-sesh)))
           (user-id (user-id the-sesh))
           (the-user (postmodern:get-dao 'rsn-auth-user user-id)))
      (handler-case
        (if (and (is-active the-user) ; make sure the user is active
                 (string= (remote-address the-sesh) (hunchentoot:real-remote-addr)) ; make sure the real-remote-addr matches
                 (string= (session-user-agent the-sesh) (hunchentoot:user-agent)) ; make sure the user agent matches
                 (local-time:timestamp<= (local-time:now) exp-date)) ; make sure the session is still valid
            (progn
              (push-success-msg "Your session has been validated.")
              (return-from validate-session t))
            (progn
              (push-error-msg "Your session could not be validated.")
              (return-from validate-session nil)))
        (error ()
          (progn
            (push-error-msg "There was an error validating your session. Please sign in again.")
            (return-from validate-session nil)))))))

;; Create a new Session for the current user
;; Per issue #7, this functionality should be moved to new-app-template's app-auth-page macro
;; OR, extended to accept HUNCHENTOOT:*SESSION*
(defun create-new-session (user &key (session nil))
  "Creates a new database-stored session for the current user."
  (postmodern:with-connection *db*
    (let* ((the-token (rsn:generate-new-session-token))
           (the-user (rsn:get-user-id-by-username user))
           (the-expiry (local-time:format-timestring nil (local-time:adjust-timestamp (local-time:today) (offset :day 14))))
           (the-remote-address (hunchentoot:real-remote-addr))
           (the-user-agent (hunchentoot:user-agent))
           (the-sesh (make-instance 'rsn-auth-session
                                    :token the-token
                                    :user-id the-user
                                    :expiry-date the-expiry
                                    :remote-address the-remote-address
                                    :user-agent the-user-agent)))
      (progn
        ;(hunchentoot:start-session)
        (setf (hunchentoot:session-value 'token (quote session)) the-token)
        (postmodern:insert-dao the-sesh)))))

(defun update-session ()
  (postmodern:with-connection *db*
    nil))

(defun create-or-update-session ()
  (postmodern:with-connection *db*
    nil))



;; EOF
