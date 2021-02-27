(defpackage #:ses4
  (:documentation "Primary package for cl-ses4, exports two functions for sending mail
                   and a main function to run as a standalone program.")
  (:use #:common-lisp #:ses4/sig #:ses4/canonicalize)
  (:export
    #:send-simple-email
    #:send-raw-email

    #:main

    #:*ses-host*
    #:*region*
    #:*access-key*
    #:*secret-key*
    ))
(in-package #:ses4)

(defparameter *ses-host* "email.us-east-1.amazonaws.com")
(defparameter *region* "us-east-1")
(defparameter *service* "ses")

(defun handle-endpoint (endpoint)
  (when endpoint
    (setf *ses-host* (quri:uri-host (quri:uri endpoint)))
    (setf *region* (second (uiop:split-string *ses-host* :separator ".")))))


(defparameter *access-key* "")
(defparameter *secret-key* "")

(defun assoc-v (item alist)
  (cdr (assoc item alist :test #'string=)))

(define-condition ses-error (error)
  ((msg :initarg :msg :accessor ses-error-msg)))

(defun handle-keys (keyfile)
  (if keyfile
      (with-open-file (s keyfile)
        (let ((creds (read s)))
          (setf *access-key* (assoc-v "aws-access-key" creds))
          (setf *secret-key* (assoc-v "aws-secret-key" creds))))
      (error 'ses-error :msg "A credentials file is required")))


(defun send-simple-email (&key from to subject body)
  "Sends a simple text email to one recipient."
  (when (or (null from) (null to) (null subject) (null body))
    (error 'ses-error :msg "You must supply each of the email's from, to, subject, and body fields"))
  (send-email `(("Action" . "SendEmail")
                ("Source" . ,from)
                ("Destination.ToAddresses.member.1" . ,to)
                ("Message.Subject.Data" . ,subject)
                ("Message.Body.Text.Data" . ,body))))

(defun send-raw-email (raw)
  "Sends a raw email, suitable as input from something expecting a sendmail-like interface, formatted like
MIME-Version: 1.0
Content-type: text/html; charset=utf-8
From: user@example.com
To: to@example.com
Subject: Thing

<html>
...
</html>
"
  (send-email `(("Action" . "SendRawEmail")
                ("RawMessage.Data" . ,(cl-base64:string-to-base64-string raw)))))

(defun send-email (email-params)
  (let* ((date-time (simple-date-time:now))
         (email-enc (quri:url-encode-params email-params))
         (headers (create-initial-ses-headers date-time *ses-host* (length email-enc)))
         (canonical-request (create-canonical-request "POST"
                                                      "/"
                                                      ""
                                                      headers
                                                      email-enc))
         (string-to-sign (create-string-to-sign date-time *region* *service* canonical-request))
         (derived-key (derive-signing-key *secret-key*
                                 date-time
                                 *region*
                                 *service*))
         (signature (sign derived-key string-to-sign))
         (auth (create-authorization-header *access-key* date-time *region* *service* headers signature)))
    (push (cons "Authorization" auth) headers)

    (dex:post (uiop:strcat "https://" *ses-host*)
              :headers headers
              :content email-enc)))

(defun main ()
  (handler-bind
    ((ses-error (lambda (c)
                  (format *error-output* "~a~%" (ses-error-msg c))
                  (uiop:quit 1)))
     (serious-condition (lambda (c)
                          (if (eql #+sbcl 'SB-SYS:INTERACTIVE-INTERRUPT #-sbcl nil (type-of c))
                              (format *error-output* "~&Received interrupt.~%Are you trying to send a raw email? Pipe or redirect the data instead of interactively filling stdin.~%")
                              (uiop:print-condition-backtrace c :count 15))
                          (uiop:quit 1))))

    (-main (uiop:command-line-arguments))))

(defun -main (args)
  (let* ((arg-defs `(("h" "help" "Display this help menu.")
                     ("k" "keys" "FILE" ,(format nil "The AWS credentials file to use. Should be in the form~%((\"aws-access-key\" . \"\")~% (\"aws-secret-key\" . \"\"))"))
                     ("e" "endpoint" "URL" "The Amazon SES endpoint to use. If an endpoint is not specified, the default of \"https://email.us-east-1.amazonaws.com/\" is used.")
                     ("f" "from" "FROM_EMAIL" "The email of the sender.")
                     ("t" "to" "TO_EMAIL" "The email of the receiver.")
                     ("s" "subject" "SUBJECT" "The subject of the email.")
                     ("m" "message" "MESSAGE" "The email body.")
                     ("r" "raw" "Send a raw email. Data will be read from STDIN.")))
         (parsed-args (quickapp:parse-args arg-defs args))
         (arglist (second parsed-args)))
    (if (or (zerop (length arglist))
            (assoc "help" arglist :test #'string=))
        (progn
          (format t "Usage: ses-send-email -k FILE [OPTIONS]~%~a~%" (quickapp:generate-flag-string arg-defs))
          (format t "Examples:~%ses-send-mail -k aws-credentials -r < /my-email.txt~%cat my-email.txt | ses-send-mail -k aws-credentials -r~%ses-send-mail -k aws-credentials -f noreply@example.com -t user@example.com -s Subject -m 'Hello User'~%"))
        (let* ((keys (assoc-v "keys" arglist))
               (endpoint (assoc-v "endpoint" arglist))
               (from (assoc-v "from" arglist))
               (to (assoc-v "to" arglist))
               (subject (assoc-v "subject" arglist))
               (message (assoc-v "message" arglist))
               (raw (assoc "raw" arglist :test #'string=)))
          (handle-keys keys)
          (handle-endpoint endpoint)
          (if raw
              (send-raw-email
                (format nil "~{~a~%~}" (loop for line = (read-line *standard-input* nil :eof)
                                             until (eql line :eof)
                                             collecting line)))
              (send-simple-email :from from :to to :subject subject :body message))))))
