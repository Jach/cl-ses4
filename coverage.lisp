#|
Run tests + generate coverage report, use sbcl --script coverage.lisp
|#
(in-package #:cl-user)
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(let ((sys-dir "~/projects/cl-ses4/"))
  (push sys-dir asdf:*central-registry*))

(ql:quickload :cl-ses4 :silent t)
(ql:quickload :cl-ses4/test :silent t)

#+sbcl
(progn
  (require :sb-cover)
  (declaim (optimize sb-cover:store-coverage-data)))

(let ((*compile-verbose* nil)
      (*load-verbose* nil))
  (asdf:load-system :cl-ses4 :force t)

  (asdf:test-system :cl-ses4))

#+sbcl
(sb-cover:report "coverage/")
