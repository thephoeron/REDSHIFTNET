;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth-db.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;;; REALM TABLE

(defclass public-realms ()
    ((name :col-type string :initarg :name :accessor :public-realms-name)
     (last-modified :col-type string :initarg :last-modified :accessor :public-realms-last-modified))
    (:metaclass dao-class)
    (:keys name))

(deftable public-realms
    (!dao-def))

(defprepared public-realms-name
  (:select 'name :from 'public-realms :where (:= 'name '$1))
  :single!)
(defprepared public-realms-last-modified
  (:select 'last-modified :from 'public-realms :where (:= 'name '$1))
  :single!)
; (create-table 'public-realms)

;;;; GROUP TABLE

(defclass public-groups ()
    ((name :col-type string :initarg :name :accessor :public-groups-name)
     (realm :col-type string :initarg :realm :accessor :public-groups-realm))
    (:metaclass dao-class)
    (:keys name))

(deftable public-groups
    (!dao-def)
    (!foreign 'public-realms 'realm 'name :on-delete :cascade :on-update :cascade))

(defprepared public-groups-name
  (:select 'name :from 'public-groups :where (:= 'name '$1))
  :single!)
(defprepared public-groups-realm
  (:select 'realm :from 'public-groups :where (:= 'name '$1))
  :single!)

; (create-table 'public-groups)

;;;; USER TABLE (new, ignore the one further down...)

(defclass public-user ()
    ((username :col-type string :initarg :username :accessor :public-user-username)
     (first-name :col-type string :initarg :first-name :accessor :public-user-first-name)
     (last-name :col-type string :initarg :last-name :accessor :public-user-last-name)
     (email :col-type string :initarg :email :accessor :public-user-email)
     (group :col-type string :initarg :group :accessor :public-user-group)
     (password :col-type string :initarg :password :accessor :public-user-password)
     (is-active :col-type boolean :initarg :is-active :accessor :public-user-is-active)
     (last-modified :col-type string :initarg :last-modified :accessor :public-user-last-modified))
    (:metaclass dao-class)
    (:keys username))

(deftable public-user
    (!dao-def)
    (!foreign 'public-groups 'group 'name :on-delete :cascade :on-update :cascade))

(defprepared public-user-username
  (:select 'username :from 'public-user :where (:= 'username '$1))
  :single!)
(defprepared public-user-first-name
  (:select 'first-name :from 'public-user :where (:= 'username '$1))
  :single!)
(defprepared public-user-last-name
  (:select 'last-name :from 'public-user :where (:= 'username '$1))
  :single!)
(defprepared public-user-email
  (:select 'email :from 'public-user :where (:= 'username '$1))
  :single!)
(defprepared public-user-group
  (:select 'group :from 'public-user :where (:= 'username '$1))
  :single!)
(defprepared public-user-password
  (:select 'password :from 'public-user :where (:= 'username '$1))
  :single!)
(defprepared public-user-is-active
  (:select 'is-active :from 'public-user :where (:= 'username '$1))
  :single!)
(defprepared public-user-last-modified
  (:select 'last-modified :from 'public-user :where (:= 'username '$1))
  :single!)

; (create-table 'public-user)

;;;; SESSION TABLE

(defclass public-session ()
  ((id :col-type string :initarg :id :accessor :public-session-id)
   (user-id :col-type string :initarg :user-id :accessor :public-session-user)
   (exp-date :col-type string :initarg :exp-date :accessor :public-session-exp-date)
   (remote-addr :col-type string :initarg :remote-addr :accessor :public-session-remote-addr)
   (user-agent :col-type string :initarg :user-agent :accessor :public-session-user-agent))
  (:metaclass dao-class)
  (:keys id))

(deftable public-session
  (!dao-def) ;; Import the existing info from the dao-class definition.
  (!foreign 'public-user 'user-id 'username :on-delete :cascade :on-update :cascade))

(defprepared public-session-id
  (:select 'id :from 'public-session :where (:= 'id '$1))
  :single)
(defprepared public-session-user-id
  (:select 'user-id :from 'public-session :where (:= 'id '$1))
  :single)
(defprepared public-session-exp-date
  (:select 'exp-date :from 'public-session :where (:= 'id '$1))
  :single)
(defprepared public-session-remote-addr
  (:select 'remote-addr :from 'public-session :where (:= 'id '$1))
  :single)
(defprepared public-session-user-agent
  (:select 'user-agent :from 'public-session :where (:= 'id '$1))
  :single)

(defprepared update-public-session-exp-date
	(:update 'public-session :set 'exp-date '$1 :where (:= 'id '$2)))
;(query (:update 'countries :set 'text '$1 :where (:= 'id 284)) "now")
; (create-table 'public-session)

;; EOF
