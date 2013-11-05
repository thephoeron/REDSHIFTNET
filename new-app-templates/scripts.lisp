;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {APPNAME}; Base: 10 -*- file: scripts.lisp

(in-package :{APPNAME})

;; Implement your custom javascript here
;; They will be available across all your vhosts defined in config.lisp
;; and served over HTTPS when needed
(define-easy-handler ({APPNAME}-js :uri "/{APPNAME}.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    ((@ ($ document) ready)
        (lambda ()
            ((@ ($ "#ajax-link") click)
                (lambda ()
                    ((@ ($ "#ajax-box") load) "/ajax-page")
                    (return false)))
            (return false)))))

;; EOF
