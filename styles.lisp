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
  (css-lite:css
    ;; Shared styles
    (("html")
      (:background "url(/static/images/patterns/carbon_fibre_v2.png) repeat"
       :height "100%"))
    (("body")
      (:min-height "100%"
       :display "inline-block"
       :overflow-x "hidden"))
    (("*")
      (:-webkit-box-sizing "border-box"
       :-moz-box-sizing "border-box"
       :box-sizing "border-box"
       :outline "0 !important"))
    ;; Typography
    ;; Bootstrap custom styles
    ;; Page structure
    ;; Off-canvas styles
    ;; Custom panels
    ;; Plugin custom styles
    ;; CSS Animations
    ;; Login Page
    (("html.loginPage")
      (:background "url(/static/images/patterns/carbon_fibre_v2.png) repeat"
       :height "auto"))
    ;; Error & Offline pages
    ;; Media Queries
    ))

;; EOF