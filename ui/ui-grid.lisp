;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET-UI; Base: 10 -*-
;;;; file: ui-grid.lisp

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet-ui)

(defmacro container ((&key (fluid nil)) &body body)
  "Bootstrap Grid Container. Set :fluid t for fluid layout."
  `(cl-who:with-html-output (hunchentoot::*standard-output*)
     (:div :class ,(if fluid "container-fluid" "container")
       ,@body)))

(defmacro row (&body body)
  "Bootstrap Grid Row."
  `(cl-who:with-html-output (hunchentoot::*standard-output*)
     (:div :class "row"
       ,@body)))

(defmacro col ((&key (vp "xs") (w 1) (offset nil)) &body body)
  "Bootstrap Grid Column. Set viewport :vp to xs, sm, md, or lg; width :w to 1 -- 12. If you need to add an offset, set :offset <column offset number>."
  `(cl-who:with-html-output (hunchentoot::*standard-output*)
     ,(if offset
          `(:div :class ,(format nil "col-~A-~S col-~A-offset-~S" vp w vp offset)
             ,@body)
          ; else
          `(:div :class ,(format nil "col-~A-~S" vp w)
             ,@body))))

(defmacro grid ((&key (fluid nil) (vp "xs")) &rest rest)
  "Define a Bootstrap grid with minimal syntax."
  `(container :fluid ,fluid
     ,@(loop for row in rest
             collect (row (loop for column in row
                                with l = (length row)
                                with w = (floor 12 l)
                                collect (col (:vp vp :w w)
                                          column))))))

;; EOF
