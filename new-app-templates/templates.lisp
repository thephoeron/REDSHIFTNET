;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: templates.lisp

(in-package :{APPNAME})

;; Customize these macros to implement your desired template
(defmacro %basic-{APPNAME}-app-page ((&key (title "{APPNAME}") (styles nil) (scripts nil)) &body body)
  "Basic, no frills {APPNAME} page function, useful for error pages, system notifications, login pages, wrapping AJAX html content, etc."
  `(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
      (:head
        (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
        (:link :rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css")
        (:link :rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap-theme.min.css")
        (:link :rel "stylesheet" :href "/redshiftnet.css" :type "text/css" :media "screen")
        (:link :rel "stylesheet" :href "/{APPNAME}.css" :type "text/css" :media "screen")
        ,@(mapcar (lambda (file)
                    `(:link :type "text/css" :rel "stylesheet" :media "screen"
                            :href ,(format nil "~A" file)))
                     (lol:flatten (list styles)))
        (:title ,title)
        "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
      (:body
        ,@body
        (:script :type "text/javascript" :src "//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js")
        (:script :type "text/javascript" :src "//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js")
        (:script :type "text/javascript" :src "/redshiftnet.js")
        (:script :type "text/javascript" :src "/{APPNAME}.js")
        ,@(mapcar (lambda (file)
                    `(:script :type "text/javascript"
                              :src ,(format nil "~A" file)))
                  (lol:flatten (list scripts)))))))

(defmacro %{APPNAME}-app-page ((&key (title "{APPNAME}") (styles nil) (scripts nil) header menu footer) &body body)
  "Standard app page template."
  `(%basic-{APPNAME}-app-page (:title ,title :styles ,(eval styles) :scripts ,(eval scripts))
    (cl-who:with-html-output (hunchentoot::*standard-output*)
      (:header :id "header"
        (funcall ,header ,title (hunchentoot:session-value 'token)))
      (:div :class "main"
        (:aside :id "sidebar" (funcall ,menu))
        (:div :id "content"
          (:div :class "wrapper"
            (rsn::admin-breadcrumb (hunchentoot:script-name*) (hunchentoot:get-parameters*))
            (:div :class "container-fluid" 
              ,@body))))
      (:footer :id "footer"
        (funcall ,footer)))))

;; WWW-INDEX Styles & Scripts
(defparameter www-index-styles (list "/static/css/custom.css"
                                     "/static/css/app.css"))
(defparameter www-index-scripts (list "/static/js/pages/dashboard.js"
                                      "/static/js/app.js"))

;; SSL-INDEX Styles & Scripts
(defparameter ssl-index-styles (list "/static/css/custom.css"
                                     "/static/css/app.css"))
(defparameter ssl-index-scripts (list "/static/js/pages/dashboard.js"
                                      "/static/js/app.js"))

;; EOF
