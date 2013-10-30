;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: forms.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; CLASS DECLARATIONS
(defclass rsn-form ()
  ((name :reader name :initarg :name)
   (fields :reader fields :initarg :fields)
   (validation-functions :accessor validation-functions :initarg :validation-functions :initform nil)
   (error-messages :reader error-messages :initarg :error-messages :initform nil)
   (submit-caption :reader submit :initarg :submit :initform "Submit")
   (enctype :accessor enctype :initarg :enctype :initform "application/x-www-form-urlencoded")
   (on-success :reader on-success :initarg :on-success)))

(defclass rsn-form-field ()
  ((name :reader name :initarg :name)
   (validation-functions :accessor validation-functions :initarg :validation-functions :initform nil)
   (default-value :reader default-value :initarg :default-value :initform nil)
   (icon :accessor icon :initarg :icon :initform nil)
   (error-messages :accessor error-messages :initarg :error-messages :initform nil)))
(defclass hidden (rsn-form-field) ())
(defclass text (rsn-form-field) ())
(defclass textarea (rsn-form-field) ())
(defclass password (rsn-form-field) ())
(defclass file (rsn-form-field) ())
(defclass checkbox (rsn-form-field) ())

(defclass rsn-form-field-set (rsn-form-field)
  ((value-set :accessor value-set :initarg :value-set :initform nil))
  (:documentation "This class is for fields that show the user a list of options"))
(defclass select (rsn-form-field-set) ())
(defclass radio-set (rsn-form-field-set) ())

(defclass rsn-form-field-return-set (rsn-form-field-set) ()
  (:documentation "This class is specifically for fields that return multiple values from the user"))
(defclass multi-select (rsn-form-field-return-set) ())
(defclass checkbox-set (rsn-form-field-return-set) ())

;; METHODS
;; post-value
;; NOTE: This section exists because Hunchentoots' (post-parameter [field-name])
;; returns a single value. This is problematic for multi-select boxes and checkbox sets
;; (both of which potentially return multiple values from the user).
;; post-value is not necessarily Hunchentoot specific, but it does expect values in the form of an alist

(defmethod post-value ((rsn-form rsn-form) post-alist)
  (mapcar (lambda (f) (post-value f post-alist)) (fields rsn-form)))

(defmethod post-value ((field rsn-form-field) post-alist)
  (cdr (assoc (name field) post-alist :test #'string=)))

(defmethod post-value ((field rsn-form-field-return-set) post-alist)
  (loop for (k . v) in post-alist
    if (string= k (name field)) collect v))
  
;; validate
;; NOTE: The validate methods each return (values [validation result] [errors]). 
;; [validation result] is a boolean
;; [errors] can be either a list or tree of strings
(defmethod validate ((rsn-form rsn-form) form-values)
  (let ((errors (if (validation-functions rsn-form)
            (make-list (length (fields rsn-form)) ;;so that elements don't get cut off
                   :initial-element
                   (loop for f in (validation-functions rsn-form)
                     for msg in (error-messages rsn-form)
                     unless (apply f form-values) collect msg))
            (loop for f in (fields rsn-form)
              for v in form-values
              collect (multiple-value-bind (result error) (validate f v) (unless result error))))))
    (values (every #'null errors) errors)))

(defmethod validate ((field rsn-form-field) value)
  "Returns (values T NIL) if there are no errors, and (values NIL list-of-errors). 
   By default, a rsn-form-field passes only its own value to its validation functions."
  (let ((errors (loop for fn in (validation-functions field)
              for msg in (error-messages field)
              unless (funcall fn value) collect msg)))
    (values (every #'null errors) errors)))

;; show
;; The show functions just take a rsn-form/(-field)?/ (along with its value/s?/ and error/s?/)
;; and output the corresponding HTML. This part is cl-who specific, but it could be easily made portable
;; by replacing html-to-stout and html-to-str with raw format calls

(defmethod show ((rsn-form rsn-form) &optional values errors)
  (with-slots (error-messages name enctype) rsn-form
    (html-to-stout
      (when (and (not (every #'null errors)) error-messages)
    (htm (:span :class "general-error" 
            (dolist (s (car errors)) 
                  (htm (:p (str s)))))))
      (:form :name (string-downcase name) :id (string-downcase name) :action (format nil "/validate-~(~a~)" name) :enctype enctype :method "post"
         (:div :class "row"
          (loop for a-field in (fields rsn-form)
            for e in errors
            for v in values
            do (str (show a-field v (when (and e (not error-messages)) e))))
          (:div :class "form-group relative" (:span :class "label") (:button :type "submit" :class "btn btn-primary pull-right col-lg-5" (str (submit rsn-form)))))))))

(defmethod show ((field hidden) &optional value error)
  (html-to-str (:input :name (name field) :value value :type "hidden")))

(defmacro define-show (field-type &body body)
  `(defmethod show ((field ,field-type) &optional value error)
     (html-to-str 
       (:div :class "form-group relative" (string-downcase (name field))
        (:div :class "icon" (:i :style "" :class (format nil "icon20 ~a" (icon field))))
        ,@body
        (when error (htm (:div :class "alert alert-error"
                            (:button :type "button" :class "close" :data-dismiss "alert" "×")
                            (:i :class "icon24 i-close-4" :style "float: left;")
                            (:h3 "Error")
                            (dolist (s error) 
                              (htm (:p (str s)))))))))))

(define-show rsn-form-field (:input :name (name field) :type "text" :value value :class "form-control" :placeholder (symbol-to-label field)))
(define-show textarea (:textarea :name (name field) :placeholder (symbol-to-label field) (str value)))
(define-show password (:input :name (name field) :type "password" :placeholder (symbol-to-label field) :class "form-control"))
(define-show file (:input :name (name field) :type "file" :placeholder (symbol-to-label field) :class "file"))

(define-show select 
  (:select :name (name field)
       (loop for v in (value-set field) 
         do (htm (:option :value v :selected (when (string= v value) "selected") (str v))))))

(define-show checkbox
  (:input :type "checkbox" :name (name field) :value (name field) 
      :checked (when (string= (name field) value) "checked")))

(define-show radio-set 
  (loop for v in (value-set field)
    do (htm (:span :class "input+label" 
                  (:input :type "radio" :name (name field) :value v 
                      :checked (when (string= v value) "checked"))
                  (str v)))))

(define-show multi-select
  (:select :name (name field) :multiple "multiple" :size 5
       (loop for v in (value-set field)
         do (htm (:option :value v 
                  :selected (when (member v value :test #'string=) "selected")
                        (str v))))))

(define-show checkbox-set
  (loop for v in (value-set field)
    do (htm (:span :class "input+label"
                  (:input :type "checkbox" :name (name field) :value v 
                      :checked (when (member v value :test #'string=) "checked"))
                  (str v)))))

;; PREDICATES
(defmacro define-predicate (name (&rest args) &body body)
  `(defun ,name ,args (lambda (val) ,@body)))

;; basic field predicates
(define-predicate longer-than? (num) (> (length val) num))
(define-predicate shorter-than? (num) (< (length val) num))
(define-predicate matches? (regex) (scan regex val))
(define-predicate mismatches? (regex) (not (scan regex val)))
(define-predicate not-blank? () (or (null val) (and (stringp val) (not (string= "" val)))))
(define-predicate same-as? (field-name-string) (string= val (post-parameter field-name-string)))
(define-predicate is-email? () (scan "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$" val))

;; file-related
;; a hunchentoot file tuple is '([temp filename] [origin filename] [file mimetype])
(define-predicate file-type? (&rest accepted-types) (member (third val) accepted-types :test #'equal))
(define-predicate file-smaller-than? (byte-size) (and (car val) (> byte-size (file-size (car val)))))

;; set-related
(define-predicate picked-more-than? (num) (> (length val) num))
(define-predicate picked-fewer-than? (num) (< (length val) num))
(define-predicate picked-exactly? (num) (= (length val) num))

;; EOF
