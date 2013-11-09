;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth-sessions.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; generate a new guaranteed-unique session token with Isaac (from Doug Hoyte (y))
(defun generate-new-session-token ()
  (let ((the-isaac-ctx (isaac:init-kernel-seed)))
    (format nil "~32,'0x" (isaac:rand-bits the-isaac-ctx 128))))

;; Validate the passed session token and user attached to it
(defun validate-session (token)
  "Validate session with passed token string."
  (let* ((sesh-id (get-session-id-by-token token))
         (the-sesh (postmodern:get-dao 'rsn-auth-session sesh-id))
         (exp-date (local-time:parse-timestring (expiry-date the-sesh)))
         (user-id (user-id the-sesh))
         (the-user (postmodern:get-dao 'rsn-auth-user user-id)))
    (handler-case
      (if (and (is-active the-user) ; make sure the user is active
               (string= (remote-address the-sesh) (hunchentoot:real-remote-addr)) ; make sure the real-remote-addr matches
               (string= (session-user-agent the-sesh) (hunchentoot:user-agent)) ; make sure the user agent matches
               (local-time:timestamp<= (local-time:now) exp-date)) ; make sure the session is still valid
          t
          nil)
      (error ()
        (push-error-msg "There was an error validating your session. Please sign in again.")))))

;; Create a new Session for the current user
(defun create-new-session (user)
  (let* ((the-token (generate-new-session-token))
         (the-user (get-user-id-by-username user))
         (the-expiry (local-time:format-timestring nil (local-time:adjust-timestamp (local-time:today) (offset :day 14))))
         (the-remote-address (hunchentoot:real-remote-addr))
         (the-user-agent (hunchentoot:user-agent))
         (the-sesh (make-instance 'rsn-auth-session
                                  :token the-token
                                  :user-id the-user
                                  :exp-date the-expiry
                                  :remote-address the-remote-address
                                  :user-agent the-user-agent)))
    (hunchentoot:start-session)
    (setf (hunchentoot:session-value 'token) the-token)
    (postmodern:insert-dao the-sesh)))

(defun update-session ()
  )

(defun create-or-update-session ()
  )



;; EOF
