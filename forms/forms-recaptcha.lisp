;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: forms-recaptcha.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(defvar *public-key* nil)
(defvar *private-key* nil)

(defclass recaptcha (rsn-form-field) ())

(defun recaptcha-passed? (challenge response ip &optional (private-key *private-key*))
  (string= "true" 
	   (car (split #\Newline
		       (http-request "http://api-verify.recaptcha.net/verify" 
				     :method :post 
				     :parameters `(("privatekey" . ,private-key)
						   ("remoteip" . ,ip)
						   ("challenge" . ,challenge)
						   ("response" . ,response)))))))

(defmethod validate ((field recaptcha) values)
  "A reCaptcha, being a foreign API call, is validated in a completely different way"
  (declare (ignore values))
  (let* ((result (recaptcha-passed? (post-parameter "recaptcha_challenge_field")
				    (post-parameter "recaptcha_response_field")
				    *private-key* ))
	 (errors (unless result (list "You seem to have mistyped the reCaptcha"))))
    (values result errors)))

(defmethod show ((field recaptcha) &optional v error)
  (declare (ignore v))
  (html-to-str 
    (:li :class (string-downcase (name field))
	 (:span :class "label")
	 (when error 
	   (htm (:span :class "rsn-form-error"
		       (dolist (s error) 
			 (htm (:p (str s)))))))
	 (:script :type "text/javascript" :src (format nil "https://www.google.com/recaptcha/api/challenge?k=~a" *public-key*))
	 (:noscript (:iframe :src (format nil "https://www.google.com/recaptcha/api/noscript?k=~a" *public-key*)
			     :height "300" :width "500" :frameborder "0")
		    (:br)
		    (:textarea :name "recaptcha_challenge_field" :rows "3" :cols "40")
		    (:input :type "hidden" :name "recaptcha_response_field" :value "manual_challenge")))))

;; EOF
