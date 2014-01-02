;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;; file: web.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

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
            (:li :class "layer" :data-depth "0.10" (:img :id "starfield" :src "/static/images/red-starfield-1920x1200.jpg"))
            (:li :class "layer" :data-depth "0.40"
            (:canvas :id "theMatrix"))
            (:li :class "layer" :data-depth "0.50" 
              (:table :style "width: 100%; height: 100%; display: block; margin-top: 10%; margin-left: 0;"
                (:tr
                  (:td :style "width: 100%; height: 100%; text-align: center; vertical-align: middle;"
                    (:img :id "logo" :src "/static/images/black-sun.png" :align "center" :style "height: 640px; width: auto; margin: auto; opacity: 0.75;")))))
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
            (:a :href "http://www.sbcl.org/" :target "_blank" (fmt "~A" (lisp-implementation-type))) " " (fmt "v~A, " (lisp-implementation-version))
            "and " (:a :href "http://redshiftnet.com/" :target "_blank" (fmt "~A" (server-type))) " " (fmt "v~A." (server-version)) :br
            (:a :href "http://www.catb.org/hacker-emblem/" (:img :src "/static/images/hacker.png" :alt "hacker emblem web badge" :class "emblem"))
            (:a :href "http://www.lisp.org/" (:img :src "/static/images/lambda-lisp.png" :alt "lambda lisp web badge" :class "emblem"))
            (:a :href "http://www.common-lisp.org/" (:img :src "/static/images/lisp-lizard.png" :alt "lizard lisp web badge" :class "emblem"))
            (:a :href "http://www.webfaction.com/?affiliate=thephoeron" :target "_blank" (:img :src "/static/images/webfaction.png" :class "emblem" :alt "WebFaction -- Smarter Web Hosting"))
            ))
        (:script :type "text/javascript" :src "https://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js")
        (:script :type "text/javascript" :src "//jqueryrotate.googlecode.com/svn/trunk/jQueryRotate.js")
        (:script :type "text/javascript" :src "/static/js/parallax/deploy/jquery.parallax.min.js")
        (:script :type "text/javascript" :src "/js/redshiftnet.js")
        (:script :type "text/javascript" :src "/static/js/redbinarymatrix.js")
        (:script :type "text/javascript"
         "$('#scene').parallax();
          var rotation = function (){
            $('#logo').rotate({
              angle:0, 
              animateTo:360, 
              callback: rotation,
              duration: 49000,
              easing: function (x,t,b,c,d){
                // t: current time, b: begInnIng value, c: change In value, d: duration
                return c*(t/d)+b;
              }
            });
          }
          rotation();")))))

;; EOF
