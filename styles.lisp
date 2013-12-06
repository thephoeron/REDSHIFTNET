;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: styles.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;;;; Contents

;;;; Default Splash Page Stylesheet -- 24

;;;; Main App Stylesheet -- 121
;;;; * Shared Styles -- 125
;;;; * Typography
;;;; * Bootstrap custom styles
;;;; * Page structure
;;;; * Off-canvas styles
;;;; * Custom Panels
;;;; * Plugin custom styles
;;;; * CSS Animations
;;;; * Login Page
;;;; * Error & Offline pages
;;;; * Media Queries

;; REDSHIFTNET Default Splash Page Stylesheet
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
    ((".emblem")
      (:height "15px"
       :max-height "15px"
       :width "80px"
       :max-width "80px"
       :display "inline-block"
       :border "0"
       :margin-left "4px"
       :margin-top "4px"))
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

;;;; REDSHIFTNET Main App Stylesheet
(define-easy-handler (rsn-app-css :uri "/css/app.css") ()
  (setf (content-type*) "text/css")
  (cl-css:css
    ;; Shared styles
    '(("html"
       :background "url(/static/images/patterns/carbon_fibre_v2.png) repeat"
       :height "100%")
      ("body"
       :min-height "100%"
       :display "inline-block"
       :overflow-x "hidden")
      ("*"
       :-webkit-box-sizing "border-box"
       :-moz-box-sizing "border-box"
       :box-sizing "border-box"
       :outline "0 !important")
      (".clear" :clear "both")
      (".center" :text-align "center")
      ; Colours
      (".white" :color "#fff")
      (".dark" :color "#6f7a8a")
      (".red" :color "#f40a0a")
      (".red-smooth" :color "#d8605f")
      (".blue" :color "#62aeef")
      (".green" :color "#72b110")
      (".yellow" :color "#f7cb38")
      (".orange" :color "#f88c00")
      ; Shadows
      (".tshadow"
        :text-shadow "0px 1px 0px #fff"
        :filter "dropshadow(color=#ffffff, offx=0, offy=1)")
      (".tshadow1"
        :text-shadow "0px 1px 0px #000"
        :filter "dropshadow(color=#000000, offx=0, offy=1)")
      (".bshadow"
        :-webkit-box-shadow "0px 1px 0px 0px rgba(255,255,255,1)"
        :box-shadow "0px 1px 0px 0px rgba(255,255,255,1)")
      (".bshadow2"
        :-webkit-box-shadow "inset 0px 1px 0px 0px rgba(255,255,255,1), 0px 1px 0px 0px rgba(255,255,255,1)"
        :box-shadow "inset 0px 1px 0px 0px rgba(255,255,255,1), 0px 1px 0px 0px rgba(255,255,255,1)")
      ; Gaps
      (".gap0" :margin "0")
      (".gap5" :margin "5px")
      (".gap10" :margin "10px")
      (".gap15" :margin "15px")
      (".gap20" :margin "20px")
      (".gap-top0" :margin-top "0")
      (".gap-top5" :margin-top "5px")
      (".gap-top10" :margin-top "10px")
      (".gap-top15" :margin-top "15px")
      (".gap-top20" :margin-top "20px")
      (".gap-right0" :margin-right "0")
      (".gap-right5" :margin-right "5px")
      (".gap-right10" :margin-right "10px")
      (".gap-right15" :margin-right "15px")
      (".gap-right20" :margin-right "20px")
      (".gap-left0" :margin-left "0")
      (".gap-left5" :margin-left "5px")
      (".gap-left10" :margin-left "10px")
      (".gap-left15" :margin-left "15px")
      (".gap-left20" :margin-left "20px")
      (".gap-bottom0" :margin-bottom "0")
      (".gap-bottom5" :margin-bottom "5px")
      (".gap-bottom10" :margin-bottom "10px")
      (".gap-bottom15" :margin-bottom "15px")
      (".gap-bottom20" :margin-bottom "20px")
      ; Pads
      (".pad0" :padding "0")
      (".pad5" :padding "5px")
      (".pad10" :padding "10px")
      (".pad15" :padding "15px")
      (".pad20" :padding "20px")
      (".pad-top0" :padding-top "0")
      (".pad-top5" :padding-top "5px")
      (".pad-top10" :padding-top "10px")
      (".pad-top15" :padding-top "15px")
      (".pad-top20" :padding-top "20px")
      (".pad-right0" :padding-right "0")
      (".pad-right5" :padding-right "5px")
      (".pad-right10" :padding-right "10px")
      (".pad-right15" :padding-right "15px")
      (".pad-right20" :padding-right "20px")
      (".pad-left0" :padding-left "0")
      (".pad-left5" :padding-left "5px")
      (".pad-left10" :padding-left "10px")
      (".pad-left15" :padding-left "15px")
      (".pad-left20" :padding-left "20px")
      (".pad-bottom0" :padding-bottom "0")
      (".pad-bottom5" :padding-bottom "5px")
      (".pad-bottom10" :padding-bottom "10px")
      (".pad-bottom15" :padding-bottom "15px")
      (".pad-bottom20" :padding-bottom "20px")
    ;; Typography
    ;; Bootstrap custom styles
    ;; Page structure
    ;; Off-canvas styles
    ;; Custom panels
    ;; Plugin custom styles
    ;; CSS Animations
      ; rotateIn
      ("@-webkit-keyframes rotateIn"
        ("0%" :-webkit-transform-origin "center center"
              :-webkit-transform "rotate(-180deg)")
        ("100%" :-webkit-transform-origin "center center"
                :-webkit-transform "rotate(0)"))
      ("@-moz-keyframes rotateIn"
        ("0%" :-moz-transform-origin "center center"
              :-moz-transform "rotate(-180deg)")
        ("100%" :-moz-transform-origin "center center"
                :-moz-transform "rotate(0)"))
      ("@-o-keyframes rotateIn"
        ("0%" :-o-transform-origin "center center"
              :-o-transform "rotate(-180deg)")
        ("100%" :-o-transform-origin "center center"
                :-o-transform "rotate(0)"))
      ("@keyframes rotateIn"
        ("0%" :transform-origin "center center"
              :transform "rotate(-180deg)")
        ("100%" :transform-origin "center center"
                :transform "rotate(0)"))
      (".rotateIn"
        :-webkit-animation-name "rotateIn"
        :-moz-animation-name "rotateIn"
        :-o-animation-name "rotateIn"
        :animation-name "rotateIn")
      ; bounceInUp
      ("@-webkit-keyframes bounceInUp"
        ("0%" :-webkit-transform "translateY(15px)")
        ("30%" :-webkit-transform "translateY(-15px)")
        ("60%" :-webkit-transform "translateY(-10px)")
        ("80%" :-webkit-transform "translateY(10px)")
        ("100%" :-webkit-transform "translateY(0)"))
      ("@-moz-keyframes bounceInUp"
        ("0%" :-moz-transform "translateY(15px)")
        ("30%" :-moz-transform "translateY(-15px)")
        ("60%" :-moz-transform "translateY(-10px)")
        ("80%" :-moz-transform "translateY(10px)")
        ("100%" :-moz-transform "translateY(0)"))
      ("@-o-keyframes bounceInUp"
        ("0%" :-o-transform "translateY(15px)")
        ("30%" :-o-transform "translateY(-15px)")
        ("60%" :-o-transform "translateY(-10px)")
        ("80%" :-o-transform "translateY(10px)")
        ("100%" :-o-transform "translateY(0)"))
      ("@keyframes bounceInUp"
        ("0%" :transform "translateY(15px)")
        ("30%" :transform "translateY(-15px)")
        ("60%" :transform "translateY(-10px)")
        ("80%" :transform "translateY(10px)")
        ("100%" :transform "translateY(0)"))
      (".bounceInUp"
        :-webkit-animation-name "bounceInUp"
        :-moz-animation-name "bounceInUp"
        :-o-animation-name "bounceInUp"
        :animation-name "bounceInUp")
      ; bounceInDown
      ("@-webkit-keyframes bounceInDown"
        ("0%" :-webkit-transform "translateY(-20px)")
        ("30%" :-webkit-transform "translateY(20px)")
        ("60%" :-webkit-transform "translateY(10px)")
        ("80%" :-webkit-transform "translateY(-10px)")
        ("100%" :-webkit-transform "translateY(0)"))
      ("@-moz-keyframes bounceInDown"
        ("0%" :-moz-transform "translateY(-20px)")
        ("30%" :-moz-transform "translateY(20px)")
        ("60%" :-moz-transform "translateY(10px)")
        ("80%" :-moz-transform "translateY(-10px)")
        ("100%" :-moz-transform "translateY(0)"))
      ("@-o-keyframes bounceInDown"
        ("0%" :-o-transform "translateY(-20px)")
        ("30%" :-o-transform "translateY(20px)")
        ("60%" :-o-transform "translateY(10px)")
        ("80%" :-o-transform "translateY(-10px)")
        ("100%" :-o-transform "translateY(0)"))
      ("@keyframes bounceInDown"
        ("0%" :transform "translateY(-20px)")
        ("30%" :transform "translateY(20px)")
        ("60%" :transform "translateY(10px)")
        ("80%" :transform "translateY(-10px)")
        ("100%" :transform "translateY(0)"))
      (".bounceInDown"
        :-webkit-animation-name "bounceInDown"
        :-moz-animation-name "bounceInDown"
        :-o-animation-name "bounceInDown"
        :animation-name "bounceInDown")
    ;; Search Page
      ("#search #search-wrapper form .form-group"
        :border-bottom "none"
        :margin-bottom "10px"
        :position "relative")
      ("#search #search-wrapper form input.form-control"
        :padding-left "46px !important")
      ("#search #search-wrapper form label"
        :margin-bottom "0")
      ("#search #search-wrapper .icon"
        :position "absolute"
        :top "1px"
        :left "1px"
        :height "34px"
        :width "36px"
        :border-right "1px solid #c9c9c9")
      ("#search #search-wrapper .icon i"
        :margin-left "8px"
        :margin-top "6px")
    ;; Login Page
      ("html.loginPage"
        :background "url(/static/images/patterns/carbon_fibre_v2.png) repeat"
        :height "auto")
      ("#login"
        :width "480px"
        :margin-left "-240px"
        :padding-bottom "20px"
        :margin-top "-200px"
        :position "absolute"
        :left "50%"
        :top "50%"
        :border "1px solid rgba(0,0,0,.5)"
        :border-radius "6px"
        :border-top-right-radius "0"
        :background "rgba(0,0,0,.5)"
        :-webkit-box-shadow "1px 1px 0px 0px rgba(0,0,0,.5)"
        :box-shadow "1px 1px 0px 0px rgba(0,0,0,.5)")
      ("#login .navbar-brand"
        :padding "5px 8px 5px"
        :width "100%"
        :float "left"
        :margin-top "-140px" ;; was -90px
        :text-align "center"
        :font-size "24px"
        :font-weight "200"
        :margin-bottom "15px"
        :text-decoration "none"
        :color "#666")
      ("#login .login-wrapper"
        :width "320px"
        :margin-left "auto"
        :margin-right "auto"
        :position "relative")
      ("#login .login-wrapper #avatar"
        :width "90px"
        :height "90px"
        :border "1px solid #c9c9c9"
        :background "white"
        :position "absolute"
        :left "-125px"
        :top "-50px"
        :float "left"
        :padding "5px")
      ("#login .login-wrapper #avatar img"
        :float "left")
      ("#login .login-wrapper form .form-group"
        :border-bottom "none"
        :margin-bottom "10px"
        :position "relative")
      ("#login .login-wrapper form input.form-control"
        :padding-left "46px !important")
      ("#login .login-wrapper form label"
        :margin-bottom "0")
      ("#login .login-wrapper .icon"
        :position "absolute"
        :top "1px"
        :left "1px"
        :height "34px"
        :width "36px"
        :border-right "1px solid #c9c9c9")
      ("#login .login-wrapper .icon i"
        :margin-left "8px"
        :margin-top "6px")
      ("#login .login-wrapper .or strong"
        :background "rgba(0,0,0,.75)"
        :padding-left "10px"
        :padding-right "10px")
      ("#login .login-wrapper .seperator"
        :margin-top "-10px")
      ("#login .login-wrapper #log, #login .login-wrapper #reg, #login .login-wrapper #forgot"
        :display "none")
      ("#login #bar"
        :position "absolute"
        :right "-43px"
        :top "-1px")
      ("#login #bar a"
        :border-radius "0"
        :border "1px solid rgba(0,0,0,.75)"
        :background "rgba(18,18,18,.5)"
        :width "43px")
      ("#login #bar[data-active=\"log\"] a#log,
        #login #bar[data-active=\"reg\"] a#reg,
        #login #bar[data-active=\"forgot\"] a#forgot"
        :background "rgba(0,0,0,.5)"
        :border-left "1px solid transparent"
        :box-shadow "none"
        :margin-left "0"
        :border-bottom-left-radius "0")
      ("#login .login-wrapper[data-active=\"log\"] #log,
        #login .login-wrapper[data-active=\"reg\"] #reg,
        #login .login-wrapper[data-active=\"forgot\"] #forgot"
        :display "block")
    ;; Error & Offline pages
    ("html.errorPage"
      :background "url(/static/images/patterns/debut_light.png) repeat")
    (".errorContainer"
      :width "480px"
      :margin-left "-240px"
      :padding-bottom "20px"
      :margin-top "-200px"
      :position "absolute"
      :left "50%"
      :top "50%"
      :border "1px solid #c9c9c9"
      :background "white"
      :-webkit-box-shadow "1px 1px 0px 0px rgba(255, 255, 255, 1)"
      :box-shadow "1px 1px 0px 0px rgba(255, 255, 255, 1)")
    (".errorContainer form"
      :margin-bottom "10px")
    (".errorContainer .page-header h1"
      :font-size "128px"
      :line-height "150px"
      :text-shadow "4px 3px 0px #fff, 9px 8px 0px rgba(0,0,0,0.15)")
    (".errorContainer .page-header h1.offline"
      :font-size "100px"
      :line-height "124px")
    (".errorContainer .or strong"
      :background "white"
      :padding-left "10px"
      :padding-right "10px")
    (".errorContainer .seperator"
      :margin-top "-10px")
    ;; Media Queries
    )))

;; EOF
