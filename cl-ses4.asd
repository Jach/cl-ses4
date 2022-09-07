(asdf:defsystem #:cl-ses4
  :description "AWS SES email sender using Signature Version 4 of Amazon's API"
  :version "1.3"
  :author "Kevin Secretan <github@thejach.com>"
  :license "Public Domain"

  :depends-on (#:dexador
               #:ironclad
               #:quri
               #:cl-ppcre
               #:local-time
               #:arrow-macros
               #:cl-base64
               #:quickapp)

  :serial t
  :pathname "src"
  :components ((:file "sig")
               (:file "canonicalize")
               (:file "ses"))

  :in-order-to ((asdf:test-op (asdf:test-op #:cl-ses4/test)))
  :build-operation "program-op"
  :build-pathname "../ses-send-email"
  :entry-point "ses4:main")

(asdf:defsystem #:cl-ses4/test
  :depends-on (#:cl-ses4
               #:fiveam
               #:uiop)
  :serial t
  :pathname "test"
  :components ((:file "ses-test"))
  :perform (asdf:test-op (o c) (uiop:symbol-call ':5am '#:run-all-tests ':summary ':suite)))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

#|
(let ((sys-dir "~/projects/cl-ses4/"))
  (push sys-dir asdf:*central-registry*))
(ql:quickload :cl-ses4)
(ql:quickload :cl-ses4/test)
|#
