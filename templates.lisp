;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: templates.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(defparameter login-styles 
    (list "/static/css/app.css"
          "/static/css/custom.css"
          "/static/css/pages/login.css"))

(defparameter login-scripts
    (list "/static/js/plugins/forms/uniform/jquery.uniform.min.js"
          "/static/js/plugins/forms/validation/jquery.validate.js"
          "/static/js/app.js"
          "/static/js/pages/login.js"))

(defparameter *header-logo* (string "/static/images/redshiftnet_header_logo.png"))
(defparameter *login-logo* (string "/static/images/redshiftnet_text_logo.png"))

;; Breadcrumb generator function
(defun %breadcrumb (script-name &optional get-parameters)
  "Creates bootstrapped navigation breadcrumbs from current request's script-name* and optional get-parameters"
  (let* ((script-list (split-sequence #\/ script-name :remove-empty-subseqs t)))
    (html-to-stout
      (:div :class "crumb"
        (:ul :class "breadcrumb"
          (:li (:a :href "/" (:i :class "icon16 i-home-4") "Home"))
          (if get-parameters
              (progn
                (loop for page in script-list
                      do (htm
                           (:li 
                             (:a :href (str (inclusive-search-and-return-string script-name page))
                               (str (string-symbol-to-label (cl-who:escape-string page)))))))
                (htm
                  (:li :class "active"
                    (loop for res in get-parameters
                          do (htm 
                               (str (string-symbol-to-label 
                                      (format nil "~A" (car res)))) 
                               (str ": ")
                               (str (string-symbol-to-label 
                                      (format nil "~A" (cdr res)))))))))
              ; else
              (progn
                (loop for page in script-list
                      for i upto (length script-list)
                      when (< i (1- (length script-list)))
                        do (htm
                          (:li (:a :href (str (inclusive-search-and-return-string script-name page))
                            (str (string-symbol-to-label (cl-who:escape-string page))))))
                      when (eq i (1- (length script-list)))
                        do (htm (:li :class "active"
                        (str (string-symbol-to-label (cl-who:escape-string page)))))))))))))

;; Basic page generator macro
(defmacro %basic-app-page ((&key (title "REDSHIFTNET") (styles nil) (scripts nil)) &body body)
  "Basic, no frills app page macro, useful for error pages, system notifications, login pages, wrapping AJAX html content, etc."
  `(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
      (:head
        (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
        (:meta :name "application-name" :content "REDSHIFTNET")
        (:meta :name "author" :content "the Phoeron <//thephoeron.com/>")
        (:link :rel "stylesheet" :href "/static/css/bootstrap/bootstrap.min.css" :type "text/css" :media "screen")
        (:link :rel "stylesheet" :type "text/css" :href "/static/css/bootstrap/bootstrap-theme.css")
        (:link :rel "stylesheet" :type "text/css" :href "/static/css/icons.css")
        (:link :rel "stylesheet" :type "text/css" :href "/static/js/plugins/ui/jgrowl/jquery.jgrowl.css")
        "<!--[if IE 8]><link href=\"css/ie8.css\" rel=\"stylesheet\" type=\"text/css\" /><![endif]-->"
        (:link :rel "apple-touch-icon-precomposed" :sizes "144x144" :href "/static/images/ico/apple-touch-icon-144-precomposed.png")
        (:link :rel "apple-touch-icon-precomposed" :sizes "114x114" :href "/static/images/ico/apple-touch-icon-114-precomposed.png")
        (:link :rel "apple-touch-icon-precomposed" :sizes "72x72" :href "/static/images/ico/apple-touch-icon-72-precomposed.png")
        (:link :rel "apple-touch-icon-precomposed" :href "/static/images/ico/apple-touch-icon-57-precomposed.png")
        (:link :rel "shortcut icon" :href "/static/images/ico/favicon.png")
        ;(:link :rel "stylesheet" :href "/redshiftnet.css" :type "text/css" :media "screen")
        ,@(mapcar (lambda (file)
                    `(:link :type "text/css" :rel "stylesheet" :media "screen"
                            :href ,(format nil "~A" file)))
                      (lol:flatten (list styles)))
        (:title ,title)
        "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
      (:body
        ,@body
        (:script :type "text/javascript" :src "/static/js/jquery-1.10.2.min.js")
        (:script :type "text/javascript" :src "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.2/jquery-ui.min.js")
        (:script :type "text/javascript" :src "/static/js/bootstrap/bootstrap.min.js")
        (:script :type "text/javascript" :src "/static/js/conditionizr.min.js")
        (:script :type "text/javascript" :src "/static/js/plugins/core/nicescroll/jquery.nicescroll.min.js")
        (:script :type "text/javascript" :src "/static/js/plugins/core/jrespond/jRespond.min.js")
        (:script :type "text/javascript" :src "/static/js/jquery.redshiftnetAdmin.js")
        (:script :type "text/javascript" :src "/static/js/plugins/ui/jgrowl/jquery.jgrowl.min.js")
        ;(:script :type "text/javascript" :src "/redshiftnet.js")
        ,@(mapcar (lambda (file)
                    `(:script :type "text/javascript"
                              :src ,(format nil "~A" file)))
                  (lol:flatten (list scripts)))))))

;; App page generator macro
(defmacro %app-page ((&key (title "REDSHIFTNET") (styles nil) (scripts nil) header menu footer) &body body)
  "Standard app page template, with header navbar, built-in breadcrumbs, sidebar menu, and footer."
  `(%basic-app-page (:title ,title :styles ,(eval styles) :scripts ,(eval scripts))
    (cl-who:with-html-output (hunchentoot::*standard-output*)
      (:header :id "header"
        (funcall ,header ,title (hunchentoot:session-value 'token)))
      (:div :class "main"
        (:aside :id "sidebar" (funcall ,menu))
        (:section :id "content"
          (:div :class "wrapper"
            (%breadcrumb (hunchentoot:script-name*) (hunchentoot:get-parameters*))
            (:div :class "container-fluid"
              ,@body))))
      (:footer :id "footer"
        (funcall ,footer)))))

;; EOF
