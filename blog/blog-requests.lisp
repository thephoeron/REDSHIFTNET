;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: blog-requests.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter default-scripts (list "/js/app.js"))
  (defparameter default-styles (list "/css/app.css")))

(defmacro blog-page ((title &key (template nil)) &body body)
  "Blog Page generator macro."
  `(%app-page (:title ,title :header #'%app-header
              :menu #'%app-menu :footer #'%app-footer
              :scripts ,(intern (format nil "~:@(~A~)-SCRIPTS" template))
              :styles ,(intern (format nil "~:@(~A~)-STYLES" template)))
    (cl-who:with-html-output (hunchentoot::*standard-output*)
      ,@body)))

;; Should redirect to 404 (not found) if can't find database record
;; for current permalink
(defun generate-blog-page-for-post ()
  "Automatically generates a blog post page from a database record based on the current uri."
  (postmodern:with-connection *db*
    (let* ((permalink (hunchentoot:script-name*))
           (post-id (get-post-id-by-permalink permalink))
           (the-post (postmodern:get-dao 'rsn-blog-post post-id)))
      (blog-page ((title the-post) :template "default")
        (post-content the-post)))))

;; REQUEST-GEN macro does not work as expected
;; replacing with regex dispatcher that matches expected blog post uri's

; (postmodern:with-connection (list *primary-db* *primary-db-user* *primary-db-pass* *primary-db-host*)
;   (let ((reqs (postmodern:query (:select 'permalink :from 'rsn-blog-post) :column)))
;     `(request-gen (,reqs :vhost vhost-web)
;       (generate-blog-page-for-post))))

;; Regex dispatcher matches any formatted blog post uri, such as:
;; /2013/12/23/slug-of-blog-post/ and then creates the post page
;; with generate-blog-page-for-post
;; This should be copied to the new-app-templates/config.lisp with
;; a note that it activates the blog module
; (push (hunchentoot:create-regex-dispatcher "^/\d{4}/\d{2}/\d{2}/[\w-]+/$" 'generate-blog-page-for-post)
;       (dispatch-table vhost-web))

;; EOF
