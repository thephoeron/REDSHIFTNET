REDSHIFTNET
===========

Common Lisp Web Application Framework for enterprise data-driven apps that require hardened security, high performance, and web-facing admin access.

REDSHIFTNET was designed to replace large-scale web frameworks such as Django, Joomla, Drupal, Ruby-on-Rails, ASP.NET, and even blogging platforms such as WordPress, keeping shared-hosts such as WebFaction in mind.

REDSHIFTNET is built on top of the Hunchentoot web server, so your REDSHIFTNET app will compile and run as a standalone executable without needing to configure Apache, nginx, or other HTTP/S web server.

Documentation is available at: http://thephoeron.viewdocs.io/REDSHIFTNET

Features
--------

* One-line app creation, automatically added to quicklisp
* Compile web-apps as binaries and run as daemons, with built-in swank server
* jQuery, Bootstrap 3.0.0 and D3 included for rapid cross-platform data-driven interface development
* VHOST support for HTTP and HTTPS, to deploy web-apps across multiple top-level and sub- domains
* Improved session handling over Hunchentoot, with database-tracked session info
* Secured pages with user authentication, session validation, and forced re-login on session expiry
* Separate Admin and End-user front ends
* Full PostgreSQL integration, with database introspection and automatic table views
* Ironclad powered PBKDF2-SHA256 password hashing
* CL-ISAAC powered random session token generation
* Form-generation with integrated validation (based on [Formlets](http://github.com/Inaimathi/formlets))
* Three-tiered notification system for alerting users through inline messages, ajax popups, and email
* Easy integration of shell commands to leverage the full power of linux in your web-apps
* JSON and XML serialization of data
* Ready-to-use live-updating graphs and widgets


Upcoming Features
-----------------

* Template-based document generation in multiple export formats
* Calendars and scheduling tools
* AJAX jGrowl notifications for system messages
* Automatically generated edit forms for database table views
* Front-end source code editing and server management
* Userspaces separated into realms, in addition to groups


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

    $ ./my-new-app --swank-port=33798 --www-port=8080 --ssl-port=8090

Alternatively, you can install the binary to a directory on your path:

    $ install -t ~/bin ./my-new-app

Your web-app runs as a background process, all output being logged in the files specified in your app's config.lisp file.  Add it to your crontab so that if the process or server crashes or is restarted, it will be restarted automatically.

Dependencies
------------

Note (11/25/2013): *CSS-Lite in process of being replaced by CL-CSS, for better CSS3 support*.

REDSHIFTNET should theoretically run anywhere SBCL runs, but has only been tested on Linux and OS X.

* Linux x86_64 or OS X Server
* SBCL 1.1.7+
* PostgreSQL 9.1+
* Quicklisp
* Pandoc

Lisp libraries available through Quicklisp:

* Hunchentoot
* CL-WHO
* CL-CSS
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
* SWANK
* CLON: The Command-Line Options Nuker
* CL-FAD
* CL-ISAAC
* LET-OVER-LAMBDA

Other Dependencies (clone into ``~/quicklisp/local-projects``):

* [UBER-SHELL](https://github.com/thephoeron/uber-shell)

Additional Configuration
------------------------

Some of the included submodule repos require additional configuration, dependencies, installation, or compilation.  See the original repos for instructions.

These include:

* jQuery FullCalendar
* jQuery DataTables
* jGrowl

You can find them under `new-app-templates/static/js/plugins/`.

Contribution
------------

If you would like to contribute to REDSHIFTNET, you can either create a pull request or join the development on [Cloud9 IDE](https://c9.io/thephoeron/redshiftnet).

License
-------

REDSHIFTNET is released under the MIT License (attribution required).  Please see the file `LICENSE` in the toplevel directory for more information.
