;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;; file: auth-users.lisp

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; PBKDF2--Sha256 Password salter and hasher functions
(defun make-isaac-salt ()
  "Generate a cryptographic random 512-bit salt for HARDENED-PASSWORD using ISAAC-64 Algorithm."
  (let* ((ctx (isaac:init-kernel-seed :is64 t))
         (string-salt (format nil "~64,'0x" (isaac:rand-bits-64 ctx 512)))
         (octet-salt (babel:string-to-octets string-salt)))
    (return-from make-isaac-salt octet-salt)))

(defun hardened-password (password)
  "Ironclad-powered PBKDF2--SHA512 password hashing function hardened with CL-ISAAC."
  (let ((the-pass (babel:string-to-octets password))
        (the-salt (make-isaac-salt)))
    (ironclad:pbkdf2-hash-password-to-combined-string the-pass :salt the-salt :digest :sha512 :iterations 10000)))

;; validate user and password. they must be active as well
(defun validate-credentials (username password &key (user-table 'rsn-auth-user))
  (declare (string username password))
  (handler-case
    (postmodern:with-connection
        (list *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
      `(let* ((user-id (get-user-id-by-username username))
             (the-user (postmodern:get-dao ,user-table user-id))
             (the-pass (password the-user))
             (test-pass (babel:string-to-octets password)))
        (if (and (is-active the-user)
                 (ironclad:pbkdf2-check-password test-pass the-pass))
            (progn
              (push-success-msg "You have successfully logged in.")
              (return-from validate-credentials t))
            (progn
              (push-error-msg "Login Failed")
              (return-from validate-credentials nil)))))
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
              (return-from update-password t))
            (progn
              (setf (session-value 'error-msgs) nil)
              (push-error-msg "Validation failed.  Password not updated.")
              (return-from update-password nil)))))
    (error ()
      (push-error-msg "Your password could not be updated."))))

;; Create a new user
(defun create-new-user (username password firstname lastname email group &key (is-admin nil))
  (let* ((the-pass (hardened-password password))
  		 (the-group-id (get-group-id-by-name group)))
    (postmodern:insert-dao
      (make-instance 'rsn-auth-user :username username
                                    :password the-pass
                                    :first-name firstname
                                    :last-name lastname
                                    :email email
                                    :group-id the-group-id
                                    :is-active t
                                    :is-admin is-admin
                                    :last-modified (local-time:format-timestring nil (local-time:now))))))

(defun update-user ()
  )

(defun create-or-update-user ()
  )

;; EOF
