REDSHIFTNET
===========

Common Lisp Web Application Framework for enterprise data-driven apps that require hardened security and high performance.

Features
--------

* Bootstrap 3.0.0 and D3 built-in, for rapid cross-platform data-driven interface development
* VHOST support for HTTP and HTTPS, to deploy web-apps across multiple top-level and sub- domains
* Improved session handling over Hunchentoot, with database-tracked session info
* Secured page macro with session validation and user authentication
* Full PostgreSQL integration
* Ironclad powered PBKDF2-SHA256 password hashing
* CL-ISAAC powered random session token generation
* Convenience macros for defining validated forms
* Three-tiered notification system for alerting users through inline messages, ajax popups, and email
* Easy integration of shell commands to leverage the full power of linux in your web-apps
* JSON and XML serialization of data
* Template-based document generation in multiple export formats

Upcoming Features
-----------------

* Separate Admin and End-user front ends
* Live-updating graphs and widgets
* Calendars and scheduling tools
* Front-end source code editing and database-management

Installation and Use
--------------------

Clone this project into ``~/quicklisp/local-projects/``

From the SBCL REPL:

    * (ql:quickload "redshiftnet")

    * (rsn:make-app 'my-new-app "~/quicklisp/local-projects/")

Your new REDSHIFTNET project will then be available under ``~/quicklisp/local-projects/my-new-app/`` ready to be loaded with ASDF or Quicklisp.

For best performance, use the ``make.lisp`` script in the project directory to compile your completed web-app on the deployment server:

    $ CC=gcc sbcl --script make.lisp

You can then run your app simply by calling (for example):

    $ ./my-new-app --www-port=8080 --ssl-port=8090

Your web-app runs as a background process, all output being logged in the files specified in your app's config.lisp file.  It can be added to your crontab so that if it crashes or the server is restarted, it will be restarted automatically.

Dependencies
------------

REDSHIFTNET was designed to replace the AMP of the LAMP stack, and with shared-hosts such as WebFaction in mind.  It should theoretically run anywhere SBCL runs, but has only been tested on Linux and OS X.

* Linux x86_64 or OS X Server
* SBCL 1.1.7+
* PostgreSQL 9.1+
* Quicklisp
* Pandoc

Lisp libraries available through Quicklisp:

* Hunchentoot
* CL-WHO
* CSS-LITE
* Parenscript
* Postmodern
* Submarine
* Local-time
* CL-PPCRE
* Split-Sequence
* YASON
* BABEL
* Ironclad
* CL-SMTP
* SB-DAEMON
* CLON: The Command-Line Options Nuker
* CL-FAD

Other Dependencies (clone into ``~/quicklisp/local-projects``):

* CL-ISAAC <https://github.com/thephoeron/cl-isaac>
* LET-OVER-LAMBDA <https://github.com/thephoeron/let-over-lambda>
* UBER-SHELL <https://github.com/thephoeron/uber-shell>

