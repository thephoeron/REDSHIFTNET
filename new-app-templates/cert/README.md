Create/place your ssl key(s) and certificate(s) in this folder, in PEM format.

Name them obviously and ensure they are referenced correctly in ../config.lisp

To create a new cert (from your project home directory):

cd cert/
openssl genrsa -out key.pem 1024
openssl req -config openssl.config -new -key key.pem -out request.pem
openssl req -x509 -days 365 -key key.pem -in request.pem -out certificate.pem
