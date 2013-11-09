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

;; old version, uses prepared statements commented out in auth-db.lisp
; (defun validate-credentials (username password)
;   (handler-case
;     (let ((stored-pass (public-user-password username))
;           (testing-pass (babel:string-to-octets password)))
;       (and (public-user-is-active username)
;            (ironclad:pbkdf2-check-password testing-pass stored-pass)))
;     (error () (push-error-msg "There was an error in validating your credentials."))))

;; new version, uses new DAO class and accessors, independent DB connection
(defun validate-credentials (username password)
  (declare (string username password))
  (handler-case
    (postmodern:with-connection
        (list *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
      (let* ((user-id (get-user-id-by-username username))
             (the-user (postmodern:get-dao 'rsn-auth-user user-id))
             (the-pass (password the-user))
             (test-pass (babel:string-to-octets password)))
        (if (and (is-active the-user)
                 (ironclad:pbkdf2-check-password test-pass the-pass))
            (progn
              (push-success-msg "You have successfully logged in.")
              (return t))
            (progn
              (push-error-msg "Login Failed")
              (return nil))))
    (error ()
      (push-error-msg "There was an error validating your credentials."))))

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

;; validate new password
;; this function is obviated by validator predicates in RSN-FORMS
;; slotted for removal
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

;; old version -- dao integration pitiful
; (defun update-password (username password new-pass new-pass-again)
;   (when (and (validate-new-password new-pass new-pass-again)
;              (validate-credentials username password))
;     (let ((the-user (postmodern:get-dao 'public-user username))
;           (the-pass (hardened-password new-pass)))
;       (setf (public-user-password username) the-pass)
;       (update-dao the-user))))

;; new version, expects new-passwords to be pre-validated
(defun update-password (username password new-pass)
  (declare (string username password new-pass))
  (handler-case
    (postmodern:with-connection
        (list *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
      (let* ((user-id (get-user-id-by-username username))
             (the-user (postmodern:get-dao 'rsn-auth-user user-id))
             (the-new-pass (hardened-password new-pass)))
        (if (validate-credentials username password)
            (progn
              (setf (session-value 'success-msgs) nil)
              (setf (password the-user) the-new-pass)
              (postmodern:update-dao the-user)
              (push-success-msg "Your password has been successfully updated.")
              (return t))
            (progn
              (setf (session-value 'error-msgs) nil)
              (push-error-msg "Validation failed.  Password not updated.")
              (return nil)))))
    (error ()
      (push-error-msg "Your password could not be updated."))))

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
