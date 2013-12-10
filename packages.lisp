;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: packages.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:redshiftnet
    (:nicknames #:rsn)
    (:use :cl
          :cl-who
          :cl-css
          :parenscript
          :cl-fad
          :split-sequence
          :cl-ppcre
          :local-time
          :postmodern
          :hunchentoot)
    ;(:shadowing-import-from :parenscript #:%)
    (:export #:rsn-start #:rsn-stop #:rsn-restart
             #:server-type #:server-version
             ;; REDSHIFTNET Templates
             #:login-styles #:login-scripts
             #:*header-logo* #:*login-logo*
             #:%app-header #:%app-menu #:%app-footer #:%breadcrumb
             #:%app-page #:%basic-app-page
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
             #:rsn-auth-realm #:get-realm-id-by-name
             #:rsn-auth-group #:get-group-id-by-name
             #:rsn-auth-user #:get-user-id-by-name
             #:rsn-auth-session #:get-session-id-by-token
             #:create-new-realm #:update-realm #:create-or-update-realm
             #:create-new-group #:update-group #:create-or-update-group
             #:hardened-password #:validate-credentials #:update-password
             #:create-new-user #:update-user #:create-or-update-user
             #:generate-new-session-token #:validate-session
             #:create-new-session #:update-session #:create-or-update-session
             #:auth-login-form #:auth-forgot-password-form
             #:auth-login #:%auth-page #:auth-page
             ;; REDSHIFTNET Forms
             #:rsn-form #:rsn-form-field 
             #:hidden #:text #:textarea #:password #:file #:tag-set
             #:checkbox #:select #:filter-select #:radio-set #:checkbox-set 
             #:multi-select #:filter-multi-select
             #:*public-key* #:*private-key* #:recaptcha
             #:validate #:show #:post-value #:show-rsn-form #:define-rsn-form
             #:longer-than? #:shorter-than? #:matches? #:mismatches? #:is-email?
             #:file-type? #:file-smaller-than?
             #:not-blank? #:same-as?
             #:picked-more-than? #:picked-fewer-than? #:picked-exactly?
             ;; REDSHIFTNET Admin
             ;#:%basic-admin-app-page #:%admin-app-page
             #:admin-page #:admin-header #:admin-menu #:admin-footer
             #:admin-login #:admin-login-styles #:admin-login-scripts
             #:admin-login-form #:admin-forgot-password-form
             #:admin-dashboard #:admin-dashboard-styles #:admin-dashboard-scripts
             ))

;; see asdf system definition
(defvar redshiftnet:*rsn-version*
  #.redshiftnet-asd::*rsn-version*)

;; EOF
