;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: auth.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

(hunchentoot:define-easy-handler (rsn-auth-css :uri "/rsn-auth.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
    (("body")
      (:padding-top "40px"
       :padding-bottom "40px"
       :background-color "#f5f5f5"))
    ((".form-signin")
      (:max-width "300px"
       :padding "19px 29px 29px"
       :margin "0 auto 20px"
       :background-color "#fff"
       :border "1px solid #e5e5e5"
       :-webkit-border-radius "5px"
       :-moz-border-radius "5px"
       :border-radius "5px"
       :-webkit-box-shadow "0 1px 2px rgba(0,0,0,.05)"
       :-moz-box-shadow "0 1px 2px rgba(0,0,0,.05)"
       :box-shadow "0 1px 2px rgba(0,0,0,.05)")
      ((".form-signin-heading")
        (:margin-bottom "0"))
      (("input[type=\"text\"], input[type=\"password\"]")
        (:font-size "16px"
         :height "auto"
         :margin-bottom "15px"
         :padding "7px 9px")))))

(defun login-form ()
  (cl-who:with-html-output (hunchentoot::*standard-output*)
    (:div :class "container"
      (:div :style "text-align: center;"
        (:img :src "/static/logo.png"))
      (:form :class "form-signin" :method :post :action ""
        (:h2 :class "form-signin-heading" "Please sign in")
        (:input :class "input-block-level" :type :text :name "username" :placeholder "Username")
        (:input :class "input-block-level" :type :password :name "password" :placeholder "Password")
        (:input :class "btn btn-large btn-primary" :type :submit :value "Sign in")))))

;; the 'authorized page' easy-handler wrapper macro
;; it should always and ONLY ever be passed an SSL acceptor.
(defmacro auth-page ((name uri title) &body body) ;; acceptor) cl:&body body)
    `(hunchentoot:define-easy-handler (,name :uri ,uri) () ;; :acceptor-names ,acceptor) ()
        (if (hunchentoot:ssl-p)
            ;; then
            (if (and (eql :post (hunchentoot:request-method*))
                     (null (hunchentoot:session-value 'token)))
                ;; then
                (let ((username (rsn::trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "username"))))
                      (password (rsn::trim-or-nil (cl-who:escape-string (hunchentoot:post-parameter "password")))))
                    (if (validate-credentials username password)
                        (progn
                            ;(hunchentoot:start-session)
                            ;(cl-who:with-html-output-to-string (hunchentoot::*standard-output* nil :prologue t :indent t)
                                (create-new-session username)
                                (progn
                                  ,@body)) ;)
                        (progn
                            ;(rsn::push-error-msg "Your credentials could not be validated.")
                            (basic-app-page (:title "Login Failed")
                                (rsn::show-all-messages)
                                (cl-who:with-html-output (hunchentoot::*standard-output*)
                                    (login-form))))))
                ;; else
                (if (and (eql :get (hunchentoot:request-method*))
                         (null (hunchentoot:session-value 'token)))
                    ;; then
                    (progn
                        (hunchentoot:start-session)
                        (basic-app-page (:title "Login")
                            (cl-who:with-html-output (hunchentoot::*standard-output*)
                                (login-form))))
                    ;; else
                    ;(when (hunchentoot:session-value 'token))
                    (let* ((token (hunchentoot:session-value 'token))
                           (the-sesh (postmodern:get-dao 'public-session token))
                           (the-user (public-session-user-id token)))
                        (if (validate-session token)
                            (progn
                                (update-public-session-exp-date (local-time:format-timestring nil (local-time:now)) token)
                                (create-new-session the-user)
                                (cl-who:with-html-output (hunchentoot::*standard-output*)
                                    ,@body))
                            (progn
                                ;(rsn::push-error-msg "Your session could not be validated.  Please sign in again.")
                                (basic-app-page (:title "Validation Failure")
                                    (rsn::show-all-messages)
                                    (cl-who:with-html-output (hunchentoot::*standard-output*)
                                        (login-form))))))))
            ;; else
            (basic-app-page (:title "Error: Insecure Connection")
                (:p "You are not on a secure connection.  Please use an SSL enabled browser and try again.")))))
    ;;;; Beginning of waste code
    ;   (cl:unless (hunchentoot:ssl-p)
    ;       (rsn:basic-app-page (:title "Error: Insecure Connection")
    ;           (:p "You are not on a secure connection.  Please use an SSL enabled browser and try again.")))
    ;   (cl:when (hunchentoot:ssl-p)
    ;       ;;(progn
          ;       (cl:unless (hunchentoot:session-value 'token)
       ;          ; then
       ;          ;; (cl:progn ;; lambda ()
       ;            (cl:when (cl:not (cl:or (cl:eql :post (hunchentoot:request-method*))
       ;                                    (cl:eql :get (hunchentoot:request-method*))))
       ;                (rsn:basic-app-page (:title "Malformed Request")
       ;                    (cl-who:with-html-output (hunchentoot::*standard-output*)
       ;                        (:p "Your request was either malformed or something weird happened... Ah well."))))
       ;              (cl:when (cl:eql :post (hunchentoot:request-method*))
       ;                  (cl:let ((username (rsn::trim-or-nil (hunchentoot:post-parameter "username")))
       ;                           (password (rsn::trim-or-nil (hunchentoot:post-parameter "password"))))
       ;                      (cl:if (validate-credentials username password)
       ;                        ;; (cl:and (rsn::require-fields username password))
       ;                          ;then -- create new session
       ;                          (cl:progn 
       ;                            (hunchentoot:start-session)
       ;                            (create-new-session username)
       ;                            (rsn:app-page (:title ,title)
       ;                                (rsn::show-all-messages)
       ;                                (cl-who:with-html-output (hunchentoot::*standard-output*)
       ;                                    ,@body))
       ;                            ;(hunchentoot:redirect hunchentoot:request-uri*)
       ;                            )
       ;                          ;else -- fail auth
       ;                          (cl:progn
       ;                            (rsn::push-error-msg "The username and password you provided are not valid.")
       ;                            (hunchentoot:start-session)
       ;                            (rsn:basic-app-page (:title "Login") (login-form))))))
       ;              (cl:when (cl:eql :get (hunchentoot:request-method*))
       ;                (hunchentoot:start-session)
       ;                  (rsn:basic-app-page (:title "Login") (login-form))))
                ; (cl:when (hunchentoot:session-value 'token)
       ;          ; else
          ;           (cl:let* ((token (hunchentoot:session-value 'token))
          ;                     (the-sesh (postmodern:get-dao 'public-session token))
          ;                     (the-user (postmodern:get-dao 'public-user (public-session-user the-sesh))))
          ;               (cl:when (validate-session token)
          ;                   (cl:setf (public-session-exp-date the-sesh) (local-time:format-timestring cl:nil (local-time:now)))
          ;                   (create-new-session (public-user-username the-user))
          ;                   (rsn:app-page (:title ,title) 
          ;                     (rsn::show-all-messages)
                ;               (cl-who:with-html-output (hunchentoot::*standard-output*)
          ;                         ,@body)))
          ;               (cl:when (cl:not (validate-session token))
          ;                 (rsn:basic-app-page (:title "Major Fail")
          ;                     (rsn::show-all-messages)
          ;                     (cl-who:with-html-output (hunchentoot::*standard-output*)
             ;                      (:p (fmt "Something went seriously wrong.... Request Token: ~A" token))))))))
    ;;;;;;;;;; End of waste code
    ; ))
    ; (rsn:basic-app-page (:title "Bottom of Macro Error")
    ;   (cl-who:with-html-output (hunchentoot::*standard-output*)
    ;       (:p "Somehow every condition failed and you ended up here.  WTF?")))
    ; )

(auth-page (test-two "/test-two/" "A Test Auth Page") ;; 'rsn:*ssl-acceptor*)
    (let* ((token (hunchentoot:session-value 'token))
           (the-sesh (postmodern:get-dao 'public-session token))
           (the-user (public-session-user-id token))
           (appellation (public-user-first-name the-user)))
      (app-page (:title "A Test Auth Page")
        (cl-who:with-html-output (hunchentoot::*standard-output*)
            (:div :class "container"
                (:h1 (cl-who:fmt "Wow. You're successfully logged in, ~A." appellation))
                (rsn::show-all-messages))))))

;; EOF
