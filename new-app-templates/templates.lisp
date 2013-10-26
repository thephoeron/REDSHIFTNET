;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: templates.lisp

(in-package :{APPNAME})

;; Customize these functions to implement your desired template
(defmacro %basic-{APPNAME}-app-page ((&key (title "{APPNAME}") (styles nil) (scripts nil)) &body body)
  "Basic, no frills {APPNAME} page function, useful for error pages, system notifications, login pages, wrapping AJAX html content, etc."
  `(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
      (:head
        (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
        (:link :rel "stylesheet" :href "/static/css/bootstrap.min.css" :type "text/css" :media "screen")
        (:link :rel "stylesheet" :href "/redshiftnet.css" :type "text/css" :media "screen")
        (:link :rel "stylesheet" :href "/{APPNAME}.css" :type "text/css" :media "screen")
        ,@(mapcar (lambda (file)
                    `(:link :type "text/css" :rel "stylesheet" :media "screen"
                            :href ,(format nil "~A" file)))
                     styles)
        (:title ,title)
        "<!--[if IE]><script src=\"http://html5shiv.googlecode.com/svn/trunk/html5.js\"></script><![endif]-->")
      (:body
        ,@body
        (:script :type "text/javascript" :src "/static/js/jquery-1.9.1.min.js")
        (:script :type "text/javascript" :src "/static/js/bootstrap.min.js")
        (:script :type "text/javascript" :src "/redshiftnet.js")
        (:script :type "text/javascript" :src "/{APPNAME}.js")
        ,@(mapcar (lambda (file)
                    `(:script :type "text/javascript"
                              :src ,(format nil "~A" file)))
                  scripts)))))

(defmacro %{APPNAME}-app-page ((&key (title "{APPNAME}") (styles nil) (scripts nil) header menu footer) &body body)
  "Standard app page template."
  `(%basic-{APPNAME}-app-page (:title ,title :styles ,@styles :scripts ,@scripts)
    (cl-who:with-html-output (hunchentoot::*standard-output*)
      (:header :id "header"
        (,@header))
      (:div :class "main"
        (:aside :id "" (,@menu))
        (:div :id "content" ,@body))
      (:footer :id "footer"
        (,@footer)))))

;; EOF
