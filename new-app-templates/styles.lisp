;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: styles.lisp

(in-package :{APPNAME})

;; Implement your custom stylesheets here
;; They will be available across all your vhosts defined in config.lisp
;; and served over HTTPS when needed
(define-easy-handler ({APPNAME}-css :uri "/{APPNAME}.css") ()
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

;; EOF
