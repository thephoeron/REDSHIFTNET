;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: forms-utils.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; general shortcuts
(defmacro html-to-stout (&body body)
  `(cl-who:with-html-output (hunchentoot::*standard-output* nil :indent t) ,@body))

(defmacro html-to-str (&body body)
  "Returns HTML as a string, as well as printing to standard-out"
  `(cl-who:with-html-output-to-string (hunchentoot::*standard-output*) ,@body))

(defun split-validation-list (validation-list)
  (loop for (fn msg) on validation-list by #'cddr
    collect fn into list-of-fn
    collect msg into list-of-msg
    finally (return (values list-of-fn list-of-msg))))

(defun file-size (f-name)
  (with-open-file (stream f-name :direction :input :if-does-not-exist nil) (file-length stream)))

(defun symbol-to-label (field-name)
  "Returns a string of a field-name formatted for labels"
  (string-capitalize (regex-replace-all "-" (name field-name) " ")))

(defun string-symbol-to-label (symbol-name)
  "Returns a string of a symbol-name formatted for labels"
  (string-capitalize (regex-replace-all "-" symbol-name " ")))

;; EOF
