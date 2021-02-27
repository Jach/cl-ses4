#!/bin/sh
sbcl --no-sysinit --no-userinit --eval '(load "~/quicklisp/setup.lisp")' --load cl-ses4.asd --eval '(ql:quickload :cl-ses4)' --eval '(asdf:make :cl-ses4)' --eval '(quit)'
