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

;; Admin header, footer, and menu template elements
(defun %app-header (title session-token)
  "Template block for site header."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:nav :class "navbar navbar-default navbar-fixed-top" :role "navigation"
      (:a :class "navbar-brand" :href "/index/"
        (:img :class "img-responsive" :src (str (format nil "~A" *header-logo*)) :alt "REDSHIFTNET" :style "height: 40px; width: auto;"))
      (when session-token
            (htm
      (:button :type "button" :class "navbar-toggle btn-danger" :data-toggle "collapse" :data-target ".navbar-to-collapse"
        (:span :class "sr-only" "Toggle Menu")
        (:i :class "icon16 i-arrow-8"))
      (:div :class "collapse navbar-collapse navbar-to-collapse"
        (:ul :class "nav navbar-nav pull-right"
          (:li :class "divider-vertical")
          (:li :class "dropdown"
            (:a :href "#" :class "dropdown-toggle" :data-toggle "dropdown"
              (:i :class "icon24 i-bell-2")
              (:span :class "notification red" "1"))
            (:ul :class "dropdown-menu" :role "menu"
              (:li :role "presentation"
                (:a :href "#" :class ""
                  (:i :class "icon16 i-file-zip")
                  "User NonnyA attached 3 files"))))
          (:li :class "divider-vertical")
          (:li :class "dropdown"
            (:a :href "#" :class "dropdown-toggle" :data-toggle "dropdown"
              (:i :class "icon24 i-envelop-2")
              (:span :class "notification red" "3"))
            (:ul :class "dropdown-menu messages" :role "menu"
              (:li :class "head" :role "presentation"
                (:h4 "Inbox")
                (:span :class "count" "3 Messages")
                (:span :class "new-msg"
                  (:a :href "#" :class "tipB" :title "Compose new message"
                    (:i :class "icon16 i-pencil-5"))))))
          (:li :class "divider-vertical")
          (:li :class "dropdown user"
            (:a :href "#" :class "dropdown-toggle avatar" :data-toggle "dropdown"
              (:img :src "/static/images/avatars/anonymous.jpg" :alt "The User")
              (:span :class "more"
                (:i :class "icon16 i-arrow-down-2")))
            (:ul :class "dropdown-menu" :role "menu"
              (:li :role "presentation"
                (:a :href "#" :class "" (:i :class "icon16 i-cogs") " Settings"))
              (:li :role "presentation"
                (:a :href "#" :class "" (:i :class "icon16 i-user") " Profile"))
              (:li :role "presentation"
                (:a :href "#" :class "" (:i :class "icon16 i-exit") " Logout"))))
          (:li :class "divider-vertical"))))))))

(defun %app-footer ()
  "Template block for admin site footer."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:div :class "push" :style "display: inline-block; height: 100px; position: relative;" "&nbsp;")
    (:div :class "container-fluid" :style "position: fixed; bottom: 0; height: 80px; width: 100%; padding: 15px 0; background-color: rgba(238,238,238,0.5); border-top: 1px solid rgba(85,85,85,0.5);"
      (:div :class "row"
        (:div :class "col-lg-12"
          (:p :class "muted credit center"
            (fmt "Crafted in (~C) Common Lisp" #\GREEK_SMALL_LETTER_LAMDA)
            (:br)
            (fmt "Powered by ~A v~A and ~A v~A."
              (lisp-implementation-type)
              (lisp-implementation-version)
              (server-type)
              (server-version))))))))

(defun %app-menu ()
  "Admin Site menu generator function."
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:div :class "side-options"
      (:ul (:li (:a :href "#" :id "collapse-nav" :class "act act-primary tip" :title "Collapse Navigation" (:i :class "icon16 i-arrow-left-7")))))
    (:div :class "sidebar-wrapper"
      (:nav :id "mainnav"
        (:ul :class "nav nav-list"
          (:li (:a :href "/admin/dashboard/"
            (:span :class "icon" (:i :class "icon20 i-screen"))
            (:span :class "txt" "Dashboard")))
          (:li (:a :href "/admin/webmail/"
            (:span :class "icon" (:i :class "icon20 i-envelop-2"))
            (:span :class "txt" "Webmail")))
          (:li (:a :href "/admin/calendar/"
            (:span :class "icon" (:i :class "icon20 i-calendar"))
            (:span :class "txt" "Calendar")))
          (:li (:a :href "#about"
            (:span :class "icon" (:i :class "icon20 i-notification"))
            (:span :class "txt" "About")
            (:ul :class "sub"
              ;; credits
              (:li (:a :href "#"
                (:span :class "icon" (:i :class "icon20 i-quill-3"))
                (:span :class "txt" "Credits")))
              ;; documentation
              (:li (:a :href "#"
                (:span :class "icon" (:i :class "icon20 i-stack-list"))
                (:span :class "txt" "Documentation")))
              ;; help
              (:li (:a :href "#"
                (:span :class "icon" (:i :class "icon20 i-question"))
                (:span :class "txt" "Help")))))))))))

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
        "<!--[if IE 8]><link href=\"/static/css/ie8.css\" rel=\"stylesheet\" type=\"text/css\" /><![endif]-->"
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
        (:script :type "text/javascript" :src "https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js")
        (:script :type "text/javascript" :src "https://ajax.googleapis.com/ajax/libs/jqueryui/1.10.2/jquery-ui.min.js")
        (:script :type "text/javascript" :src "/static/js/bootstrap/bootstrap.min.js")
        (:script :type "text/javascript" :src "/static/js/conditionizr.min.js")
        (:script :type "text/javascript" :src "/static/js/plugins/core/nicescroll/jquery.nicescroll.min.js")
        (:script :type "text/javascript" :src "/static/js/plugins/core/jrespond/js/jRespond.min.js")
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
