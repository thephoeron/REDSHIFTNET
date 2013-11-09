;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth-users.lisp

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

;; UPDATE PASSWORD IN USER DAO
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

;; Create a new user
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

(defun update-user ()
  )

(defun create-or-update-user ()
  )

;; EOF
