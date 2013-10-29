;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: forms-macros.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; HELPER MACROS
;; NOTE: If you REALLY want, you can define your own raw rsn-form instance and validator as in define-rsn-form.
;; The same can be said about show-rsn-form. If you go this route, pay attention to how they communicate;
;; currently they use Hunchentoots' session
(defun define-field (field-name field-type &key size value-set default-value validation)
  "Takes a terse declaration and expands it into a make-instance for macro purposes"
  (let ((final-value-set (when value-set `(:value-set ,value-set)))
    (final-size (when size `(:size ,size))))
    (multiple-value-bind (functions messages) (split-validation-list validation)
      `(make-instance ',field-type :name ,(format nil "~(~a~)" field-name) 
              :default-value ,default-value ,@final-value-set ,@final-size
              :validation-functions (list ,@functions) :error-messages (list ,@messages)))))

(defmacro define-rsn-form ((name &key general-validation (submit "Submit")) (&rest fields) &rest on-success)
  "Converts a terse declaration form into the corresponding object and validation handler."
  ; the flet function converts a terse declaration into the corresponding make-instance declaration
  (let* ((field-names (mapcar #'car fields))
         (field-objects (mapcar (lambda (f) (apply #'define-field f)) fields))
         (enctype (if (every (lambda (f) (not (eq (cadr f) 'file))) fields)
                      "application/x-www-form-urlencoded" 
                      "multipart/form-data")))
    (multiple-value-bind (gen-val-fn gen-err-msg) (split-validation-list general-validation) 
      `(progn 
         ; declare rsn-form instance
         (defparameter ,name
           (make-instance 'rsn-form
                          :name ',name :submit ,submit :enctype ,enctype
                          :validation-functions ,(when general-validation `(list ,@gen-val-fn)) 
                          :error-messages ,(when general-validation `(list ,@gen-err-msg))
                          :fields (list ,@field-objects)
                          :on-success (lambda ,field-names (progn ,@on-success))))
         ; declare validation handler
         (define-easy-handler (,(intern (format nil "VALIDATE-~a" name)) :uri ,(format nil "/validate-~(~a~)" name)) ()
           (let* ((rsn-form-values (post-value ,name (post-parameters*)))
                  (rsn-form-return-values (loop for f in (rsn::fields ,name) ;;the values list, less password values
                                                for v in rsn-form-values
                                                unless (eq (type-of f) 'password) collect v
                                                else collect nil)))
             (multiple-value-bind (result errors) (validate ,name rsn-form-values)
             (if result
               (apply (rsn::on-success ,name) rsn-form-values) ;; if everything's ok, send the user on
               (progn
                 (setf (session-value :rsn-form-values) rsn-form-return-values
                       (session-value :rsn-form-errors) errors
                       (session-value :rsn-form-name) ',name)
                 (redirect (referer)))))))))))

(defun ensure-list-length (list desired-length)
  (assert (and (integerp desired-length) (< 0 desired-length)))
  (cond ((= (length list) desired-length) list)
    ((> (length list) desired-length) (butlast list (- (length list) desired-length)))
    ((< (length list) desired-length)
     (append list (make-list (- desired-length (length list)))))))

(defmacro show-rsn-form (rsn-form-name &key default-values)
  "Shortcut for displaying a rsn-form.
   It outputs the rsn-form HTML to standard-out (with indenting).
   If this is the last submitted rsn-form in session, display field values and errors, then clear out the rsn-form-related session information."
  `(let* ((default-val ,default-values)
      (val (cond ((eq (session-value :rsn-form-name) ',rsn-form-name) 
              (session-value :rsn-form-values))
             (default-val (ensure-list-length default-val (length (rsn::fields ,rsn-form-name))))
             (t (make-list (length (rsn::fields ,rsn-form-name))))))
      (err (if (eq (session-value :rsn-form-name) ',rsn-form-name)
           (session-value :rsn-form-errors)
           (make-list (length (rsn::fields ,rsn-form-name))))))
     (show ,rsn-form-name val err)
     (when (eq (session-value :rsn-form-name) ',rsn-form-name)
       (delete-session-value :rsn-form-name)
       (delete-session-value :rsn-form-values)
       (delete-session-value :rsn-form-errors))))

;; EOF
