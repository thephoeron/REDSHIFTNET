;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: web.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; REDSHIFTNET Master Stylesheet
(define-easy-handler (splash-page-css :uri "/rsn-splash-page.css") ()
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
         :text-shadow "0 1px 0 rgba(255,255,255,.1), 0 0 30px rgba(255,255,255,.4)")))
    (("@font-face")
        (:font-family "ProFont Windows tweaked"
         :src "url('/static/css/fonts/ProFontWindows.ttf') format('opentype')"))
    (("html, body")
        (:background "#000"
         :margin "0"
         :padding "0"
         :overflow "hidden"))
    (("#fullxy")
        (:position "absolute"
         :top "0"
         :left "0"
         :background "transparent"
         :width "100%"
         :height "100%"
         :max-height "100%"
         :text-align "center"
         :vertical-align "middle"))
    (("canvas")
        (:font-family "ProFont Windows tweaked"
         :color "#db0000"
         :font-size "18px"
         :min-height "1800px"
         :min-width "2600px"
         :height "120%"
         :width "120%"
         :position "absolute"
         :top "0"
         :left "0"
         :margin-top "-10%"
         :margin-left "-10%"
         :background "url(/static/images/pattern.png) repeat center center"
         :opacity "0.4"))
    (("#logo")
        (:position "relative"
         :margin "auto"
         :padding "inherit"))
    (("#emblem")
      (:height "15px"
       :max-height "15px"
       :width "80px"
       :max-width "80px"
       :display "inline-block"))
    (("#starfield")
        (:opacity "0.5"
         :width "120%"
         :height "auto"
         :margin-top "-10%"
         :margin-left "-10%"))
    (("canvas:after")
        (:content "''"
         :display "block"
         :position "absolute"
         :top "0"
         :right "0"
         :bottom "0"
         :left "0"
         :background "url(/static/images/pattern.png) repeat center center"
         :opacity "0.4"))
    (("#copyright")
        (:position "absolute"
         :width "100%"
         :padding "inherit"
         :bottom "0"
         :left "0"
         :text-align "center")
        (("p")
            (:font-family "ProFont Windows tweaked"
             :color "#db0000"
             :font-size "18px")
            (("a, a:visited, a:active")
                (:color "red"
                 :text-decoration "none"))
            (("a:hover")
                (:text-decoration "underline"))))))

;; REDSHIFTNET Master jQuery functions
(define-easy-handler (redshiftnet-js :uri "/redshiftnet.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    ((@ ($ document) ready)
        (lambda ()
            ((@ ($ "#ajax-link") click)
                (lambda ()
                    ((@ ($ "#ajax-box") load) "/ajax-page")
                    (return false)))
            (return false)))))

;;;; Default Splash Page
(define-easy-handler (splash-page :uri "/") ()
  (cl-who:with-html-output-to-string (s)
    (:html 
      (:head 
        (:title "REDSHIFTNET")
        (:link :rel "stylesheet" :href "/static/css/parallax.styles.css")
        (:link :rel "stylesheet" :href "/rsn-splash-page.css" :type "text/css" :media "screen"))
      (:body :style "border: 0; padding: 0; margin: 0; overflow: hidden;" ;:onLoad "init();"
        (:div :id "fullxy"
          (:ul :id "scene"
               (:li :class "layer" :data-depth "0.20" (:img :id "starfield" :src "/static/images/red-starfield-1920x1200.jpg"))
               (:li :class "layer" :data-depth "0.40"
                (:canvas :id "theMatrix"))
               (:li :class "layer" :data-depth "0.60" 
                (:table :style "width: 100%; height: 100%; display: block; margin-top: 10%; margin-left: 0;"
                  (:tr
                    (:td :style "width: 100%; height: 100%; text-align: center; vertical-align: middle;"
                      (:img :id "logo" :src "/static/images/sunburst.png" :align "center" :style "height: 400px; width: auto; margin: auto; opacity: 0.75;")))))
               (:li :class "layer" :data-depth "0.80"
                (:table :style "width: 100%; height: 100%; display: block; margin-top: 10%; margin-left: 0;"
                  (:tr
                    (:td :style "width: 100%; height: 100%; text-align: center; vertical-align: middle;"
                      (:img :src "/static/images/redshiftnet_text_logo_big.png" :align "center" :style "width: 960px; height: auto; margin: auto;")))))))
        (:div :id "copyright" :style "position: fixed; bottom: 0; height: 75px; width: 100%; color: #AF0000;"
          (:p :align "center"
            (:a :href "http://common-lisp.net" :class "tip" :title "Crafted in Common Lisp" (str (format nil "(~C)" #\GREEK_SMALL_LETTER_LAMDA))) :br
            "Copyright &copy; 2012 &mdash; 2013, \"the Phoeron\" (//thephoeron.com/) &mdash; All Rights Reserved. "
            "Powered by "
            (:a :href "http://www.webfaction.com/?affiliate=thephoeron" :target "_blank" "WebFaction") " "
            "(Smarter Web Hosting), "
            (:a :href "http://www.sbcl.org/" :target "_blank" (fmt "~A" (lisp-implementation-type))) " " (fmt "v~A, " (lisp-implementation-version))
            "and " (:a :href "http://redshiftnet.com/" :target "_blank" (fmt "~A" (server-type))) " " (fmt "v~A." (server-version)) :br
            (:a :href "http://www.catb.org/hacker-emblem/" (:img :src "http://www.catb.org/hacker-emblem/hacker.png" :alt "hacker emblem" :id "emblem"))
            ))
        (:script :type "text/javascript" :src "https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js")
        (:script :type "text/javascript" :src "//jqueryrotate.googlecode.com/svn/trunk/jQueryRotate.js")
        (:script :type "text/javascript" :src "/static/js/parallax/deploy/jquery.parallax.min.js")
        (:script :type "text/javascript" :src "/redshiftnet.js")
        (:script :type "text/javascript" :src "/static/js/redbinarymatrix.js")
        (:script :type "text/javascript"
          "$('#scene').parallax();
          var rotation = function (){
             $(\"#logo\").rotate({
                angle:0, 
                animateTo:360, 
                callback: rotation,
                duration: 49000,
                easing: function (x,t,b,c,d){ // t: current time, b: begInnIng value, c: change In value, d: duration
                    return c*(t/d)+b;
                }
             });
          }
          rotation();")))))

;; Standard app-page template...
; (defmacro app-page ((&key title) &body body)
;  "A default template for any application html page"
;  `(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
;     (:html :lang "en"
;     (:head 
;       (:meta :http-equiv "Content-Type" 
;             :content    "text/html;charset=utf-8")
;       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
;       (:link :rel "stylesheet" :href "/static/css/bootstrap.min.css" :type "text/css" :media "screen")
;       (:link :rel "stylesheet" :href "/redshiftnet.css" :type "text/css" :media "screen")
;     (:title ,title))
;       (:body 
;         (:div :class "navbar navbar-inverse navbar-fixed-top"
;           (:div :class "navbar-inner"
;             (:div :class "container"
;               (:a :class "brand" :href "/" ,title)
;               (:div :class "nav-collapse collapse"
;                 (:ul :class "nav"
;                   (:li (:a :href "/index" "Home"))
;                   (:li (:a :href "/dev" "Development"))
;                   (:li (:a :href "/books" "Books"))
;                   (:li (:a :href "/music" "Music"))
;                   (:li (:a :href "/art" "Art"))
;                   (:li (:a :href "/linguistics" "Linguistics"))
;                   (:li (:a :href "/occult" "Occultism"))
;                   (:li :class "dropdown" 
;                     (:a :href "#" :class "dropdown-toggle" :data-toggle "dropdown" 
;                       "About"
;                       (:b :class "caret"))
;                     (:ul :class "dropdown-menu"
;                       (:li (:a :href "/impressum" "Impressum"))
;                       (:li (:a :href "/portfolio" "Web Portfolio")))))))))
;         (:div :id "wrap" ,@body
;           (:div :id "push"))
;         (:hr)
;         (:div :id "footer"
;           (:div :class "container"
;             (:p :class "muted credit" 
;               (fmt "Copyright &copy; 2004 &ndash; 2013 \"the Phoeron\" (//thephoeron.com/).  Powered by ~A v~A and ~A v~A."
;                   (lisp-implementation-type)
;                   (lisp-implementation-version)
;                   (server-type)
;                   (server-version)))))
;         (:script :type "text/javascript" :src "/static/js/jquery-1.9.1.min.js")
;         (:script :type "text/javascript" :src "/static/js/bootstrap.min.js")
;         (:script :type "text/javascript" :src "/redshiftnet.js")))))

; ;; basic app-page template wrapper suitable for logins and status messages
; (defmacro basic-app-page ((&key title) &body body)
;   "A bare bones bootstrap-powered page useful for logins and status messages."
;   `(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
;         (:html :lang "en"
;             (:head 
;                  (:meta :http-equiv "Content-Type" 
;                         :content    "text/html;charset=utf-8")
;                  (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
;                  (:link :rel "stylesheet" :href "/static/css/bootstrap.min.css" :type "text/css" :media "screen")
;                  (:link :rel "stylesheet" :href "/rsn-auth.css" :type "text/css" :media "screen")
;                  (:title ,title))
;             (:body 
;                 (:div :class "container"
;                     (:h1 ,title)
;                     (:div :id "wrap" ,@body)
;                     (:div :id "push"))
;                 (:hr)
;                 (:div :id "footer"
;                     (:div :class "container"
;                         (:p :class "muted credit" 
;                             (fmt "Copyright &copy; 2004 &ndash; 2013 \"the Phoeron\" (//thephoeron.com/).  Powered by ~A v~A and ~A v~A."
;                                 (lisp-implementation-type)
;                                 (lisp-implementation-version)
;                                 (server-type)
;                                 (server-version)))))
;                 (:script :type "text/javascript" :src "/static/js/jquery-1.9.1.min.js")
;                 (:script :type "text/javascript" :src "/static/js/bootstrap.min.js")
;                 (:script :type "text/javascript" :src "/redshiftnet.js")))))

;; jQuery convenience macro for Parenscript
(defmacro $$ ((selector event-binding) &body body)
  `((@ ($ ,selector) ,event-binding) (lambda () ,@body)))

(ps:import-macros-from-lisp '$$)

;; EOF
