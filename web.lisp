;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: web.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; REDSHIFTNET Master Stylesheet
(define-easy-handler (redshiftnet-css :uri "/redshiftnet.css") ()
    (setf (content-type*) "text/css")
    (css-lite:css
        (("body")
            (:padding-top "60px")
            (("> .navbar .brand")
                (:padding-right "0"
                 :padding-left "5px"
                 :font-weight "bold"
                 :color "#000"
                 :text-shadow "0 1px 0 rgba(255,255,255,.1), 0 0 30px rgba(255,255,255,.125)"
                 :-webkit-transition "all .2s linear"
                 :-moz-transition "all .2s linear"
                 :transition "all .2s linear"))
            (("> .navbar .brand:hover")
                (:text-decoration "none"
                 :text-shadow "0 1px 0 rgba(255,255,255,.1), 0 0 30px rgba(255,255,255,.4)")))))

;; REDSHIFTNET Master jQuery functions
(define-easy-handler (redshiftnet-js :uri "/redshiftnet.js") ()
  (setf (content-type*) "text/javascript")
  (parenscript:ps
    ((@ ($ document) ready)
        (lambda ()
            ((@ ($ "#ajax-link") click)
                (lambda ()
                    ((@ ($ "#ajax-box") load) "/ajax-page")
                    (return false)))
            (return false)))))

;;;; Default Splash Page
(define-easy-handler (splash-page :uri "/") ()
    (with-html-output-to-string (s)
        (:html 
            (:head 
                (:title "REDSHIFTNET")
                (:link :rel "stylesheet" :href "/redshiftnet.css" :type "text/css" :media "screen")
                (:script :type "text/javascript" :src "/redshiftnet.js"))
            (:body :onLoad "init();"
                (:canvas :id "theMatrix"
                    (:p "This site requires an HTML5, CSS3, and JavaScript compatible browser."))
                (:div :id "fullxy"
                    (:table :id "logo" :cellpadding "0" :cellspacing "0" :width "100%" :height "100%"
                        (:tr
                            (:td :align "center" :valign "middle"
                                (:a :href "/index/" :target "_top"
                                    (:img :src "/static/img/rsn-logo.png" :border "0" :width "420" :height "420"))))))
                (:div :id "copyright"
                    (:p :align "center"
                        "Copyright &copy; 2012 &mdash; 2013, \"the Phoeron\" (//thephoeron.com/) &mdash; All Rights Reserved." :br
                        "Powered by "
                        (:a :href "http://www.webfaction.com/?affiliate=thephoeron" :target "_blank" "WebFaction") " "
                        "(Smarter Web Hosting), "
                        (:a :href "http://www.sbcl.org/" :target "_blank" (fmt "~A" (lisp-implementation-type))) " " (fmt "v~A, " (lisp-implementation-version))
                        "and " (:a :href "http://redshiftnet.com/" :target "_blank" (fmt "~A" (server-type))) " " (fmt "v~A." (server-version))))))))

;; Standard app-page template...
(defmacro app-page ((&key title) &body body)
 "A default template for any application html page"
 `(with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head 
             (:meta :http-equiv "Content-Type" 
                    :content    "text/html;charset=utf-8")
             (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
             (:link :rel "stylesheet" :href "/static/css/bootstrap.min.css" :type "text/css" :media "screen")
             (:link :rel "stylesheet" :href "/redshiftnet.css" :type "text/css" :media "screen")
             (:title ,title))
           (:body 
              (:div :class "navbar navbar-inverse navbar-fixed-top"
                (:div :class "navbar-inner"
                    (:div :class "container"
                        (:a :class "brand" :href "/" ,title)
                        (:div :class "nav-collapse collapse"
                            (:ul :class "nav"
                                (:li (:a :href "/index" "Home"))
                                (:li (:a :href "/dev" "Development"))
                                (:li (:a :href "/books" "Books"))
                                (:li (:a :href "/music" "Music"))
                                (:li (:a :href "/art" "Art"))
                                (:li (:a :href "/linguistics" "Linguistics"))
                                (:li (:a :href "/occult" "Occultism"))
                                (:li :class "dropdown" 
                                    (:a :href "#" :class "dropdown-toggle" :data-toggle "dropdown" 
                                        "About"
                                        (:b :class "caret"))
                                    (:ul :class "dropdown-menu"
                                        (:li (:a :href "/impressum" "Impressum"))
                                        (:li (:a :href "/portfolio" "Web Portfolio")))))))))
              (:div :id "wrap" ,@body
                (:div :id "push"))
              (:hr)
              (:div :id "footer"
                (:div :class "container"
                    (:p :class "muted credit" 
                        (fmt "Copyright &copy; 2004 &ndash; 2013 \"the Phoeron\" (//thephoeron.com/).  Powered by ~A v~A and ~A v~A."
                            (lisp-implementation-type)
                            (lisp-implementation-version)
                            (server-type)
                            (server-version)))))
              (:script :type "text/javascript" :src "/static/js/jquery-1.9.1.min.js")
              (:script :type "text/javascript" :src "/static/js/bootstrap.min.js")
              (:script :type "text/javascript" :src "/redshiftnet.js")))))

;; basic app-page template wrapper suitable for logins and status messages
(defmacro basic-app-page ((&key title) &body body)
    "A bare bones bootstrap-powered page useful for logins and status messages."
    `(with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
        (:html :lang "en"
            (:head 
                 (:meta :http-equiv "Content-Type" 
                        :content    "text/html;charset=utf-8")
                 (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
                 (:link :rel "stylesheet" :href "/static/css/bootstrap.min.css" :type "text/css" :media "screen")
                 (:link :rel "stylesheet" :href "/rsn-auth.css" :type "text/css" :media "screen")
                 (:title ,title))
            (:body 
                (:div :class "container"
                    (:h1 ,title)
                    (:div :id "wrap" ,@body)
                    (:div :id "push"))
                (:hr)
                (:div :id "footer"
                    (:div :class "container"
                        (:p :class "muted credit" 
                            (fmt "Copyright &copy; 2004 &ndash; 2013 \"the Phoeron\" (//thephoeron.com/).  Powered by ~A v~A and ~A v~A."
                                (lisp-implementation-type)
                                (lisp-implementation-version)
                                (server-type)
                                (server-version)))))
                (:script :type "text/javascript" :src "/static/js/jquery-1.9.1.min.js")
                (:script :type "text/javascript" :src "/static/js/bootstrap.min.js")
                (:script :type "text/javascript" :src "/redshiftnet.js")))))

;; jQuery convenience macro for Parenscript
(defmacro $$ ((selector event-binding) &body body)
  `((@ ($ ,selector) ,event-binding) (lambda () ,@body)))

(ps:import-macros-from-lisp '$$)

;; EOF
