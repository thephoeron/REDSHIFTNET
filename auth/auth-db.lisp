;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth-db.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;;; REALM TABLE

(defclass rsn-auth-realm ()
  ((ID :col-type integer :initarg :id :accessor id)
   (NAME :col-type string :initarg :name :accessor name))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-auth-realm
  (!dao-def))

; (defprepared public-realms-name
;   (:select 'name :from 'public-realms :where (:= 'name '$1))
;   :single!)
; (defprepared public-realms-last-modified
;   (:select 'last-modified :from 'public-realms :where (:= 'name '$1))
;   :single!)
; (create-table 'public-realms)

;;;; GROUP TABLE

(defclass rsn-auth-group ()
  ((ID :col-type integer :initarg :id :accessor id)
   (NAME :col-type string :initarg :name :accessor name)
   (REALM-ID :col-type integer :initarg :realm :accessor realm-id))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-auth-group
  (!dao-def)
  (!foreign 'rsn-auth-realm 'realm-id :primary-key :on-delete :cascade :on-update :cascade))

; (defprepared public-groups-name
;   (:select 'name :from 'public-groups :where (:= 'name '$1))
;   :single!)
; (defprepared public-groups-realm
;   (:select 'realm :from 'public-groups :where (:= 'name '$1))
;   :single!)

; (create-table 'public-groups)

;;;; USER TABLE (new, ignore the one further down...)

(defclass rsn-auth-user ()
  ((ID :col-type integer :initarg :id :accessor id)
   (USERNAME :col-type string :initarg :username :accessor username)
   (FIRST-NAME :col-type string :initarg :first-name :accessor first-name)
   (LAST-NAME :col-type string :initarg :last-name :accessor last-name)
   (EMAIL :col-type string :initarg :email :accessor email)
   (GROUP-ID :col-type integer :initarg :group :accessor group-id)
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

; (defprepared public-user-username
;   (:select 'username :from 'public-user :where (:= 'username '$1))
;   :single!)
; (defprepared public-user-first-name
;   (:select 'first-name :from 'public-user :where (:= 'username '$1))
;   :single!)
; (defprepared public-user-last-name
;   (:select 'last-name :from 'public-user :where (:= 'username '$1))
;   :single!)
; (defprepared public-user-email
;   (:select 'email :from 'public-user :where (:= 'username '$1))
;   :single!)
; (defprepared public-user-group
;   (:select 'group :from 'public-user :where (:= 'username '$1))
;   :single!)
; (defprepared public-user-password
;   (:select 'password :from 'public-user :where (:= 'username '$1))
;   :single!)
; (defprepared public-user-is-active
;   (:select 'is-active :from 'public-user :where (:= 'username '$1))
;   :single!)
; (defprepared public-user-last-modified
;   (:select 'last-modified :from 'public-user :where (:= 'username '$1))
;   :single!)

; (create-table 'public-user)

;;;; SESSION TABLE

(defclass rsn-auth-session ()
  ((ID :col-type integer :initarg :id :accessor id)
   (TOKEN :col-type string :initarg :token :accessor token)
   (USER-ID :col-type integer :initarg :user-id :accessor user-id)
   (EXPIRY-DATE :col-type date :initarg :exp-date :accessor expiry-date)
   (REMOTE-ADDRESS :col-type string :initarg :remote-addr :accessor remote-address)
   (USER-AGENT :col-type string :initarg :user-agent :accessor user-agent))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-auth-session
  (!dao-def) ;; Import the existing info from the dao-class definition.
  (!foreign 'rsn-auth-user 'user-id :primary-key :on-delete :cascade :on-update :cascade))

; (defprepared public-session-id
;   (:select 'id :from 'public-session :where (:= 'id '$1))
;   :single)
; (defprepared public-session-user-id
;   (:select 'user-id :from 'public-session :where (:= 'id '$1))
;   :single)
; (defprepared public-session-exp-date
;   (:select 'exp-date :from 'public-session :where (:= 'id '$1))
;   :single)
; (defprepared public-session-remote-addr
;   (:select 'remote-addr :from 'public-session :where (:= 'id '$1))
;   :single)
; (defprepared public-session-user-agent
;   (:select 'user-agent :from 'public-session :where (:= 'id '$1))
;   :single)

;; Rewrite for DAO as generic function and method
(defprepared update-public-session-exp-date
	(:update 'public-session :set 'exp-date '$1 :where (:= 'id '$2)))
;(query (:update 'countries :set 'text '$1 :where (:= 'id 284)) "now")
; (create-table 'public-session)

;; EOF
