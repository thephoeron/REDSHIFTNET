;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;; file: auth-db.lisp

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;;; REALM TABLE

(defclass rsn-auth-realm ()
  ((ID :col-type serial :initarg :id :reader id)
   (NAME :col-type string :initarg :name :accessor name))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-auth-realm
  (!dao-def))

(defprepared get-realm-id-by-name
  (:select 'id :from 'rsn-auth-realm :where (:= 'name '$1))
  :single!)

;;;; GROUP TABLE

(defclass rsn-auth-group ()
  ((ID :col-type serial :initarg :id :reader id)
   (NAME :col-type string :initarg :name :accessor name)
   (REALM-ID :col-type integer :initarg :realm-id :accessor realm-id))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-auth-group
  (!dao-def)
  (!foreign 'rsn-auth-realm 'realm-id :primary-key :on-delete :cascade :on-update :cascade))

(defprepared get-group-id-by-name
  (:select 'id :from 'rsn-auth-group :where (:= 'name '$1))
  :single!)

;;;; USER TABLE (new, ignore the one further down...)

(defclass rsn-auth-user ()
  ((ID :col-type serial :initarg :id :reader id)
   (USERNAME :col-type string :initarg :username :accessor username)
   (FIRST-NAME :col-type string :initarg :first-name :accessor first-name)
   (LAST-NAME :col-type string :initarg :last-name :accessor last-name)
   (EMAIL :col-type string :initarg :email :accessor email)
   (GROUP-ID :col-type integer :initarg :group-id :accessor group-id)
   (PASSWORD :col-type string :initarg :password :accessor password)
   (IS-ACTIVE :col-type boolean :initarg :is-active :accessor is-active)
   (IS-ADMIN :col-type boolean :initarg :is-admin :accessor is-admin)
   (LAST-MODIFIED :col-type timestamp :initarg :last-modified :accessor last-modified))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-auth-user
    (!dao-def)
    (!foreign 'rsn-auth-group 'group-id :primary-key :on-delete :cascade :on-update :cascade))

(defprepared get-user-id-by-username
  (:select 'id :from 'rsn-auth-user :where (:= 'username '$1))
  :single!)

;;;; SESSION TABLE

(defclass rsn-auth-session ()
  ((ID :col-type serial :initarg :id :reader id)
   (TOKEN :col-type string :initarg :token :accessor token)
   (USER-ID :col-type integer :initarg :user-id :accessor user-id)
   (EXPIRY-DATE :col-type timestamp :initarg :expiry-date :accessor expiry-date)
   (REMOTE-ADDRESS :col-type string :initarg :remote-address :accessor remote-address)
   (USER-AGENT :col-type string :initarg :user-agent :accessor session-user-agent))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-auth-session
  (!dao-def) ;; Import the existing info from the dao-class definition.
  (!foreign 'rsn-auth-user 'user-id :primary-key :on-delete :cascade :on-update :cascade))

(defprepared get-session-id-by-token
  (:select 'id :from 'rsn-auth-session :where (:= 'token '$1))
  :single!)

(defprepared get-user-id-by-session-token
  (:select 'user-id :from 'rsn-auth-session :where (:= 'token '$1))
  :single!)

;; Rewrite for DAO as generic function and method
; (defprepared update-public-session-exp-date
;   (:update 'public-session :set 'exp-date '$1 :where (:= 'id '$2)))
;(query (:update 'countries :set 'text '$1 :where (:= 'id 284)) "now")
; (create-table 'public-session)

;; EOF
