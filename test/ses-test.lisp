(defpackage #:ses4/test
  (:use #:common-lisp #:fiveam
        #:ses4 #:ses4/sig #:ses4/canonicalize))
(in-package #:ses4/test)
; (setf *run-test-when-defined* 'T)

(def-suite ses-test)
(in-suite ses-test)

(def-test canonical-uri ()
  (is (equal "/"
             (canonicalize-uri "/")))
  (is (equal "/"
             (canonicalize-uri "")))
  (is (equal "/docs"
             (canonicalize-uri "/docs")))
  (is (equal "/docs/"
             (canonicalize-uri "/docs/")))
  (is (equal "/docs/foo/bar"
             (canonicalize-uri "/docs/foo/bar")))
  (is (equal "/docs/foo"
             (canonicalize-uri "/docs/./foo")))
  (is (equal "/foo"
             (canonicalize-uri "/docs/../foo")))
  (is (equal "/documents%2520and%2520settings/"
             (canonicalize-uri "/documents and settings/")))
  )

(def-test canonical-query-string ()
  (is (equal ""
             (canonicalize-query-string "")))
  (is (equal "A=b"
             (canonicalize-query-string "A=b")))
  (is (equal "A=b"
             (canonicalize-query-string '(("A" . "b")))))
  (is (equal "Foo=bar&bat=Man"
             (canonicalize-query-string "bat=Man&Foo=bar")))
  (is (equal "Foo=bar&bat=Man"
             (canonicalize-query-string '(("bat" . "Man") ("Foo" . "bar")))))
  (is (equal "Foo=bAa09-_.~r"
             (canonicalize-query-string "Foo=bAa09-_.~r")))
  (is (equal "Email=foo%40example.com&Subject=Test%20Mail"
             (canonicalize-query-string "Email=foo@example.com&Subject=Test Mail")))
  (is (equal "Subject=A%253DA"
             (canonicalize-query-string "Subject=A%3DA")))
  (is (equal "Subject=A%253DA&Test=This%26That"
             (canonicalize-query-string '(("Subject" . "A=A") ("Test" . "This&That")))))
  (is (equal "Foo=Foo&Foo=bar&bat=Man"
             (canonicalize-query-string "bat=Man&Foo=bar&Foo=Foo")))
  )

(def-test canonical-headers ()
  (is (equal "content-type:application/x-www-form-urlencoded; charset=utf-8
host:iam.amazonaws.com
my-header1:a b c
my-header2:\"a b c\"
x-amz-date:20150830T123600Z
"
             (canonicalize-headers '(("Host" . "iam.amazonaws.com")
                                     ("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8")
                                     ("My-header1" . "    a   b   c  ")
                                     ("X-Amz-Date" . "20150830T123600Z")
                                     ("My-Header2" . "    \"a   b   c\"  ")))))
          )

(def-test canonical-signed-headers ()
  (is (equal "content-type;host;x-amz-date"
             (canonicalize-signed-headers '(("Host" . "whatever") ("Content-Type" . "whatever") ("X-Amz-Date" . "whatever")))))
          )

(def-test empty-payload-hashed ()
  (is (equal "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
             (hash-payload ""))))

(def-test canonical-request ()
  (let ((headers '(("Host" . "iam.amazonaws.com") ("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8") ("X-Amz-Date" . "20150830T123600Z"))))
    (is (equal "GET
/
Action=ListUsers&Version=2010-05-08
content-type:application/x-www-form-urlencoded; charset=utf-8
host:iam.amazonaws.com
x-amz-date:20150830T123600Z

content-type;host;x-amz-date
e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
               (create-canonical-request "GET"
                                         "/"
                                         "Action=ListUsers&Version=2010-05-08"
                                         headers
                                         ""))))
  )

(def-test string-to-sign ()
  (let* ((headers '(("Host" . "iam.amazonaws.com") ("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8") ("X-Amz-Date" . "20150830T123600Z")))
         (canonical-request (create-canonical-request "GET"
                                                      "/"
                                                      "Action=ListUsers&Version=2010-05-08"
                                                      headers
                                                      "")))
    (is (equal "AWS4-HMAC-SHA256
20150830T123600Z
20150830/us-east-1/iam/aws4_request
f536975d06c0309214f805bb90ccff089219ecd68b2577efef23edd43b7e1a59"
               (create-string-to-sign (local-time:encode-timestamp 0 0 36 12 30 8 2015) "us-east-1" "iam" canonical-request))))
  )

(def-test derived-signing-key ()
  (is (equal "c4afb1cc5771d871763a393e44b703571b55cc28424d1a5e86da6ed3c154a4b9"
             (string-downcase (hex-digest (derive-signing-key "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                                                              (local-time:encode-timestamp 0 0 0 0 30 8 2015)
                                                              "us-east-1"
                                                              "iam"))))))

(def-test signature ()
  (let* ((headers '(("Host" . "iam.amazonaws.com") ("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8") ("X-Amz-Date" . "20150830T123600Z")))
         (canonical-request (create-canonical-request "GET"
                                                      "/"
                                                      "Action=ListUsers&Version=2010-05-08"
                                                      headers
                                                      ""))
         (date (local-time:encode-timestamp 0 0 36 12 30 8 2015))
         (string-to-sign (create-string-to-sign date "us-east-1" "iam" canonical-request))
         (derived-key (derive-signing-key "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                                 date
                                 "us-east-1"
                                 "iam")))
    (is (equal "5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7"
               (sign derived-key string-to-sign)))))

(def-test auth-header ()
  (let* ((headers '(("Host" . "iam.amazonaws.com") ("Content-Type" . "application/x-www-form-urlencoded; charset=utf-8") ("X-Amz-Date" . "20150830T123600Z")))
         (canonical-request (create-canonical-request "GET"
                                                      "/"
                                                      "Action=ListUsers&Version=2010-05-08"
                                                      headers
                                                      ""))
         (date (local-time:encode-timestamp 0 0 36 12 30 8 2015))
         (string-to-sign (create-string-to-sign date "us-east-1" "iam" canonical-request))
         (derived-key (derive-signing-key "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                                 date
                                 "us-east-1"
                                 "iam"))
         (signature (sign derived-key string-to-sign)))
    (is (equal "AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20150830/us-east-1/iam/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature=5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7"
               (create-authorization-header "AKIDEXAMPLE" date "us-east-1" "iam" headers signature)))))

(def-test initial-headers ()
  (is (equal '(("Date" . "20150830T123600Z")
               ("Host" . "email.us-west-2.amazonaws.com")
               ("Content-Length" . "12")
               ("Content-Type" . "application/x-www-form-urlencoded"))
             (create-initial-ses-headers (local-time:encode-timestamp 0 0 36 12 30 8 2015) "email.us-west-2.amazonaws.com" 12))))

(def-test hmac-sha-sanity ()
  (is (equal "CA52E7B754AA86C8911CC88BC90CF87FD6FFDC363232E11977997DFA946D8CCA"
             (hex-digest (hmac-sha256 (hmac-sha256 "a" "b") (hmac-sha256 "c" "d"))))))

(def-test invalid-credentials ()
  (setf ses4:*access-key* "bad-key"
        ses4:*secret-key* "bad-secret")
  (signals dexador.error:http-request-forbidden
    (ses4:send-simple-email :from "test@example.com" :to "test2@example.com" :subject "Subject" :body "Body")))
