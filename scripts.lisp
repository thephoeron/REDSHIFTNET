;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*-
;;;; file: scripts.lisp

;;;; Copyright (c) 2012 -- 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; jQuery convenience macro for Parenscript
(defmacro $$ ((selector event-binding) &body body)
  `((@ ($ ,selector) ,event-binding) (lambda () ,@body)))

(ps:import-macros-from-lisp '$$)

;; REDSHIFTNET Default Splash Page jQuery functions
;; Buggy as all shit... need to double-check parenscript docs
; (define-easy-handler (redshiftnet-splash-js :uri "/js/splash-page.js") ()
;   (setf (content-type*) "text/javascript")
;   (ps
;     ((@ ($ document) ready)
;         (lambda ()
;           ((@ ($ "#scene") parallax))
;           (flet ((rotation ((@ ($ "#logo") rotate)
;                                (create
;                                  angle 0
;                                  animate-to 360
;                                  callback #'rotation
;                                  duration 49000
;                                  easing '(lambda (x time b c d)
;                                           (return (+ b (* c (floor time d)))))))))
;             (rotation)
;             (return t))
;           (return t)))))

;; REDSHIFTNET Master jQuery functions
(define-easy-handler (redshiftnet-js :uri "/js/redshiftnet.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    ((@ ($ document) ready)
        (lambda ()
          ((@ ($ ".select2") select2))
          ((@ ($ "#ajax-link") click)
              (lambda ()
                ((@ ($ "#ajax-box") load) "/ajax-page")
                (return false)))
          (return false)))))

;; EOF
