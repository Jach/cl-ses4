(defpackage #:ses4/sig
  (:documentation "Functions related to hashing and cryptographically
                   SIGning data used for the AWS request")
  (:use #:common-lisp #:arrow-macros)
  (:export
    #:amazon-iso-8601-basic-time
    #:hash-payload
    #:sha256
    #:hex-digest
    #:create-string-to-sign
    #:credential-scope
    #:hmac-sha256
    #:derive-signing-key
    #:sign
    ))
(in-package #:ses4/sig)

(defun amazon-iso-8601-basic-time (date-time)
  "Returns date-time in the format  YYYYMMDDTHHMMSSZ, which is required by Amazon.
   simple-date-time:|yyyymmddThhmmssZ| does not handle time zones, and without any
   info after the 'Z' the date is assumed to be at UTC+0. This naively corrects the hour
   based on whatever simple-date-time:*default-timezone* is, however this is likely to break
   for unusual timezones that aren't offset by whole hours."
  (format nil "~04,'0d~02,'0d~02,'0dT~02,'0d~02,'0d~02,'0dZ"
          (simple-date-time:year-of date-time) (simple-date-time:month-of date-time) (simple-date-time:day-of date-time)
          (+ (simple-date-time:hour-of date-time) (- simple-date-time:*default-timezone*)) (simple-date-time:minute-of date-time) (simple-date-time:second-of date-time)))

(defun hash-payload (payload)
  "Creates HashedPayload = Lowercase(HexEncode(Hash(payload)))
   using SHA256"
  (string-downcase (hex-digest (sha256 payload))))

(defun sha256 (str)
  (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array str)))

(defun hex-digest (byte-vec)
  (format nil "~{~2,'0x~}" (coerce byte-vec 'list)))

(defun create-string-to-sign (date-time region service canonical-request)
  "Given a simple-date-time:date-time, a region, a service, and a cononical request (created by #'create-canonical-request)
   returns a string to sign according to https://docs.aws.amazon.com/general/latest/gr/sigv4-create-string-to-sign.html
   with AWS4-HMAC-SHA256 as the default algorithm."
  (let ((\n '(#\Newline)))
    (concatenate 'string
                 "AWS4-HMAC-SHA256" \n
                 (amazon-iso-8601-basic-time date-time) \n
                 (credential-scope date-time region service) \n
                 (hash-payload canonical-request))))

(defun credential-scope (date-time region service)
  (uiop:strcat (simple-date-time:yyyymmdd date-time) "/" region "/" service "/aws4_request"))

(defun hmac-sha256 (key data &aux mac)
  (when (typep key 'string)
    (setf key (ironclad:ascii-string-to-byte-array key)))
  (when (typep data 'string)
    (setf data (ironclad:ascii-string-to-byte-array data)))

  (setf mac (ironclad:make-mac :hmac key :sha256))
  (ironclad:update-mac mac data)
  (ironclad:produce-mac mac))

(defun derive-signing-key (secret-key date region service)
  "Given an AWS Secret Access Key, a simple-date-time:date, region, and service,
   computes the signing key as per https://docs.aws.amazon.com/general/latest/gr/sigv4-calculate-signature.html"
  (-> (uiop:strcat "AWS4" secret-key)
    (hmac-sha256 (simple-date-time:yyyymmdd date))
    (hmac-sha256 region)
    (hmac-sha256 service)
    (hmac-sha256 "aws4_request")))

(defun sign (derived-key string-to-sign)
  "Computes signature for the derived key and string-to-sign calculated by above functions"
  (string-downcase (hex-digest (hmac-sha256 derived-key string-to-sign))))
