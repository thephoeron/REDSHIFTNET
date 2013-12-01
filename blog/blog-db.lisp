;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: blog-db.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(defclass rsn-blog-post ()
  ((ID :col-type serial :initarg :id :reader id)
   (TITLE :col-type string :initarg :title :accessor title)
   (PERMALINK :col-type string :initarg :permalink :accessor permalink)
   (POST-CONTENT :col-type string :initarg :post-content :accessor post-content)
   (AUTHOR-ID :col-type integer :initarg :author-id :accessor author-id)
   (IS-PUBLISHED :col-type boolean :initarg :is-published :accessor is-published)
   (LAST-USER-ID :col-type integer :initarg :last-user-id :accessor last-user-id)
   (LAST-MODIFIED :col-type timestamp :initarg :last-modified :accessor last-modified))
  (:metaclass dao-class)
  (:keys id))

(deftable rsn-blog-post
  (!dao-def)
  (!foreign 'rsn-auth-user 'author-id :primary-key :on-delete :cascade :on-update :cascade)
  (!foreign 'rsn-auth-user 'last-user-id :primary-key))

(defprepared get-post-id-by-permalink
  (:select 'id :from 'rsn-blog-post :where (:= 'permalink '$1))
  :single!)

;; EOF
