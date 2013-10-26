;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth-sessions.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; PBKDF2--Sha256 Password salter and hasher.
(defun hardened-password (password)
  (let ((the-pass (babel:string-to-octets password))
        (the-salt (ironclad:make-random-salt)))
    (ironclad:pbkdf2-hash-password-to-combined-string the-pass :salt the-salt :digest :sha256 :iterations 10000)))

;; validate user and password. they must be active as well
(defun validate-credentials (username password)
  (handler-case
    (let ((stored-pass (public-user-password username))
          (testing-pass (babel:string-to-octets password)))
      (and (public-user-is-active username)
           (ironclad:pbkdf2-check-password testing-pass stored-pass)))
    (error () (push-error-msg "There was an error in validating your credentials."))))

;; validate new password
(defun validate-new-password (new-pass new-pass-again)
  (handler-case
    (if (string= new-pass new-pass-again)
        t
        (progn
          (push-error-msg "The passwords you entered don't match.")
          nil))
    (error () (push-error-msg "The new passwords you entered could not be validated."))))

;; UPDATE PASSWORD IN USER DAO
;; Confirm old-password is correct, new passwords match
(defun update-password (username password new-pass new-pass-again)
  (when (and (validate-new-password new-pass new-pass-again)
             (validate-credentials username password))
    (let ((the-user (postmodern:get-dao 'public-user username))
          (the-pass (hardened-password new-pass)))
      (setf (public-user-password username) the-pass)
      (update-dao the-user))))

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
;; must add update-session and create-or-update-session
(defun create-new-session (user)
  (let ((the-token (generate-new-session-token)))
    (hunchentoot:start-session)
    (setf (hunchentoot:session-value 'token) the-token)
    (postmodern:insert-dao
      (make-instance 'public-session :id the-token
                                     :user-id user
                                     :exp-date (local-time:format-timestring nil (local-time:adjust-timestamp (local-time:today) (offset :day 14)))
                                     :remote-addr (hunchentoot:real-remote-addr)
                                     :user-agent (hunchentoot:user-agent)))))

;; Create a new user
;; must add update-user and create-or-update-user
(defun create-new-user (username password firstname lastname email group)
  (let ((the-pass (hardened-password password)))
    (postmodern:insert-dao
      (make-instance 'public-user :username username
                                  :password the-pass
                                  :first-name firstname
                                  :last-name lastname
                                  :email email
                                  :group group
                                  :is-active t
                                  :last-modified (local-time:format-timestring nil (local-time:now))))))

(defun create-new-realm (name)
  (postmodern:insert-dao (make-instance 'public-realms :name name :last-modified (local-time:format-timestring nil (local-time:now)))))

(defun create-new-group (name realm)
  (postmodern:insert-dao (make-instance 'public-groups :name name :realm realm)))

;; EOF
