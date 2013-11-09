;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth-sessions.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; Convenience functions for logging in to accounts created with Django
;; Sadly, not working... will have to look closer at how Django and Ironclad
;; encode their salts and hashes
(defun convert-django-password-to-ironclad (password)
  (replace-all password "pbkdf2_sha256$" "PBKDF2$SHA256:" :test #'string=))

(defun validate-django-credentials (username password)
  (postmodern:with-connection (list *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
    (handler-case
      (let* ((stored-pass (postmodern:query (:select 'password :from 'auth-user
                                                     :where (:= 'username username)) 
                                            :single!))
             (conv-pass (convert-django-password-to-ironclad stored-pass))
             (test-pass (babel:string-to-octets password)))
        (format t "~%stored-pass: ~A" stored-pass)
        (format t "~%  conv-pass: ~A" conv-pass)
        (format t "~%  test-pass: ~A" test-pass)
        (and (postmodern:query (:select 'is-active :from 'auth-user
                                        :where (:= 'username username)) 
                               :single!)
             (ironclad:pbkdf2-check-password test-pass conv-pass)))
      (error () (format t "~%There was an error validating your django credentials")))))

;; generate a new guaranteed-unique session token with Isaac (from Doug Hoyte (y))
(defun generate-new-session-token ()
  (let ((the-isaac-ctx (isaac:init-kernel-seed)))
    (format nil "~32,'0x" (isaac:rand-bits the-isaac-ctx 128))))

;; Validate the passed session token and user attached to it
(defun validate-session (token)
  "Validate session with passed token string."
  (let* ((the-sesh (postmodern:get-dao 'public-session token))
         (remote-addr (public-session-remote-addr token))
         (user-agent (public-session-user-agent token))
         (exp-date (local-time:parse-timestring (public-session-exp-date token)))
         (username (public-session-user-id token))
         (the-user (postmodern:get-dao 'public-user username)))
    (handler-case
      (if (and (public-user-is-active username) ; make sure the user is active
               (string= remote-addr (hunchentoot:real-remote-addr)) ; make sure the real-remote-addr matches
               (string= user-agent (hunchentoot:user-agent)) ; make sure the user agent matches
               (local-time:timestamp<= (local-time:now) exp-date)) ; make sure the session is still valid
          t
          nil)
      (error () (push-error-msg "Your session could not be validated.  Please sign in again.")))))

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
