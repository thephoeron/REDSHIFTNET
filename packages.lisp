;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: packages.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:redshiftnet
    (:nicknames #:rsn)
    (:use :cl
          :cl-who
          :css-lite
          :parenscript
          :cl-fad
          :split-sequence
          :cl-ppcre
          :local-time
          :postmodern
          :hunchentoot)
    (:shadowing-import-from :parenscript #:%)
    (:export #:web-start #:web-stop
             #:server-type #:server-version
             ;; REDSHIFTNET Web
             #:app-page #:basic-app-page
             ;; REDSHIFTNET Utilities
             #:defrequest #:define-constant
             #:read-and-return-html-from-rst
             #:ordnum #:replace-all #:get-month-by-number
             #:push-error-msg #:push-success-msg #:push-info-msg
             #:show-error-messages #:show-success-messages #:show-info-messages
             #:show-all-messages #:trim-or-nil #:filter
             #:parse-int-or-force-value #:parse-int-force-pos-or-zero
             ;; REDSHIFTNET Config
             #:*primary-db* #:*primary-db-user* #:*primary-db-pass* #:*primary-db-host* #:*primary-db-port*
             #:*default-directory* #:*static-folder*
             #:*acceptor* #:*ssl-acceptor*
             #:*rsn-version*
             ;; REDSHIFTNET Auth
             #:public-realms #:public-realms-name #:public-realms-last-modified
             #:public-groups #:public-groups-name #:public-groups-realm
             #:public-user #:public-user-username #:public-user-first-name #:public-user-last-name
             #:public-user-email #:public-user-group #:public-user-password
             #:public-user-is-active #:public-user-last-modified
             #:public-session #:update-public-session-exp-date
             #:hardened-password #:validate-credentials #:update-password
             #:generate-new-session-token #:validate-session #:create-new-session
             #:create-new-user #:create-new-realms #:create-new-group
             ;; REDSHIFTNET Forms
             #:rsn-form #:rsn-form-field 
             #:hidden #:text #:textarea #:password #:file
             #:checkbox #:select #:radio-set #:checkbox-set #:multi-select
             #:*public-key* #:*private-key* #:recaptcha
             #:validate #:show #:post-value #:show-rsn-form #:define-rsn-form
             #:longer-than? #:shorter-than? #:matches? #:mismatches?
             #:file-type? #:file-smaller-than?
             #:not-blank? #:same-as?
             #:picked-more-than? #:picked-fewer-than? #:picked-exactly?
             ))

;; see asdf system definition
(defvar redshiftnet:*rsn-version*
  #.redshiftnet-asd::*rsn-version*)

;; EOF
