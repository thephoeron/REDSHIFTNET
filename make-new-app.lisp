;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: REDSHIFTNET; Base: 10 -*- file: make-new-app.lisp

;;;; Copyright (c) 2012 -- 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :redshiftnet)

;; Stolen mercilessly from Weblocks

(export '(make-application))

(defparameter *app-name-placeholder* (ppcre:quote-meta-chars "{APPNAME}")
  "A placeholder located in template files.")

(defun attributize-name (name)
  "Convert a string or a symbol to a format suitable for
serialization (in particular for markup languages like HTML).

Ex:
\(attributize-name 'hello-world) => \"hello-world\"
\(attributize-name \"Hello world-ref\") => \"hello-world-ref\""
  (when (null name)
    (return-from attributize-name ""))
  (let ((namestr (etypecase name
		     (symbol (symbol-name name))
		     (string name)
		     (integer (format nil "~A" name)))))
    (string-downcase (substitute #\- #\Space namestr))))

(defun make-dir (pathname)
  "Create directory 'pathname'.
Borrowed from cl-darcs with permission of copyright owner."
  (with-simple-restart (ignore-error "Ignore ~A directory creation error." pathname)
    (multiple-value-bind (path created) (ensure-directories-exist pathname)
      (declare (ignore path))
      (unless created
        (error "Directory ~A already exists." pathname)))))

(defun copy-directory (source target &key excluding)
  "Copy all files and directories in 'source' to 'target'.
'source' and 'target' are pathnames designating directories, both of
which must exist.  'Excluding' is a list of files and directories
to exclude.

Symlinks will confuse the function.

Borrowed from cl-darcs with permission of copyright owner."
  (let* ((wild (make-pathname :directory '(:relative :wild-inferiors)
                              :name :wild
                              :type :wild
                              :version :wild))
         (source-wild (merge-pathnames wild source))
         (target-wild (merge-pathnames wild target))
         (excluding-wild (mapcar
                          (lambda (excluded) (merge-pathnames wild excluded))
                          excluding))
         (files (fad:list-directory (truename source))))
    (dolist (source-file files)
      (let ((target-file (translate-pathname source-file source-wild target-wild)))
        (cond
          ((some (lambda (excluded) (pathname-match-p source-file excluded)) excluding-wild)
           ;; File excluded - do nothing.
           )
          ((fad:directory-pathname-p source-file)
           (make-dir target-file)
           (copy-directory source-file target-file :excluding excluding))
          (t
            (ensure-directories-exist target-file :verbose t)
            (copy-file source-file target-file :overwrite t)))))))

(defun asdf-system-directory (system-name)
  "Returns a directory of the asdf system file of system
'system-name'."
  (make-pathname :directory
		 (pathname-directory (truename (asdf:system-definition-pathname
						(asdf:find-system system-name))))))

(defun copy-file-replace (source target &optional match replacement)
  "Copies 'source' to 'target' replacing all instances of 'match' with
'replacement' in the target."
  (let (text)
    (with-open-file (stream source)
      (setf text (make-string (file-length stream)))
      (read-sequence text stream))
    (setf text (ppcre:regex-replace-all match text replacement))
    (with-open-file (stream target :direction :output :if-does-not-exist :create :if-exists nil)
      (write-sequence text stream))))

(defun make-application (name &optional target)
  "Creates a directory 'name' under directory 'target' and fills it
with files that allow to easily get started with a new REDSHIFTNET
application.

If 'target' isn't specified, 'make-applicaiton' uses
*default-pathname-defaults*.

'name' cannot be NIL, as it's being used in numerous places in the
generated files to specify the name of the newly created
application. Note, 'name' is expected to be a symbol naming the
application."
  ; ensure name is a symbol
  (assert (symbolp name))
  ; if target is missing, set default
  (unless target
    (setf target *default-pathname-defaults*))
  ; create a directory 'name' under 'target'
  (let* ((new-project-dir
           (merge-pathnames
             (make-pathname :directory `(:relative ,(attributize-name name)))
             (truename (pathname-as-directory target)))))
    ; create necessary directories
    (ensure-directories-exist new-project-dir :verbose t)
    ; copy redshiftnet static files and support folders
    (copy-directory
      (pathname-as-file
        (truename
          (merge-pathnames
            (make-pathname :directory '(:relative "new-app-templates" "static"))
            (asdf-system-directory :redshiftnet))))
		  new-project-dir)
    (copy-directory
      (pathname-as-file
        (truename
          (merge-pathnames
            (make-pathname :directory '(:relative "new-app-templates" "logs"))
            (asdf-system-directory :redshiftnet))))
      new-project-dir)
    (copy-directory
      (pathname-as-file
        (truename
          (merge-pathnames
            (make-pathname :directory '(:relative "new-app-templates" "cert"))
            (asdf-system-directory :redshiftnet))))
      new-project-dir)
    ; copy packages.lisp
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "packages" :type "lisp")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name "packages" :type "lisp")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy config.lisp
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "config" :type "lisp")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name "config" :type "lisp")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy db.lisp
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "db" :type "lisp")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name "db" :type "lisp")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy forms.lisp
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "forms" :type "lisp")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name "forms" :type "lisp")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy make.lisp
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "make" :type "lisp")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name "make" :type "lisp")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy requests.lisp
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "requests" :type "lisp")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name "requests" :type "lisp")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy scripts.lisp
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "scripts" :type "lisp")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name "scripts" :type "lisp")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy styles.lisp
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "styles" :type "lisp")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name "styles" :type "lisp")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy templates.lisp
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "templates" :type "lisp")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name "templates" :type "lisp")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy {APPNAME}.asd
    (copy-file-replace
      (merge-pathnames
        (make-pathname :directory '(:relative "new-app-templates")
                       :name "{APPNAME}" :type "asd")
        (asdf-system-directory :redshiftnet))
      (merge-pathnames
        (make-pathname :name (attributize-name name) :type "asd")
        new-project-dir)
      *app-name-placeholder*
      (attributize-name name))
    ; copy {APPNAME}.lisp
    (copy-file-replace
      (merge-pathnames
			  (make-pathname :directory '(:relative "new-app-templates")
				       :name "{APPNAME}" :type "lisp")
			  (asdf-system-directory :redshiftnet))
		  (merge-pathnames
			  (make-pathname :name (attributize-name name) :type "lisp")
			  new-project-dir)
			*app-name-placeholder*
		  (attributize-name name))
    ; Should check to see if quicklisp is available and app has been created under ~/quicklisp/local-projects/
    ; if not, tell user to add new app to asdf central registry
    (format t "~%~A has been created.~%" name)
    (format t "Please add '~A' to asdf:*central-registry* before you proceed.~%" new-project-dir)
    (format t "Happy hacking!")))

;; EOF
