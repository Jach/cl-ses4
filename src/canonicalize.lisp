(defpackage #:ses4/canonicalize
  (:documentation "Functions related to canonicalizing data used
                   for the AWS request into the expected formats")
  (:use #:common-lisp #:ses4/sig)
  (:export
    #:create-canonical-request
    #:canonicalize-uri
    #:canonicalize-query-string
    #:canonicalize-headers
    #:canonicalize-signed-headers

    #:create-initial-ses-headers
    #:create-authorization-header
    ))
(in-package #:ses4/canonicalize)

(defun create-canonical-request (http-request-method uri query-string headers request-payload)
  "Following https://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
   constructs a canonical request given each not-yet-canonicalized parameter.
   Note headers should be an alist.
   Query string can be either an alist (recommended) or a string"
  (let ((\n '(#\Newline)))
    (concatenate 'string
                 http-request-method \n
                 (canonicalize-uri uri) \n
                 (canonicalize-query-string query-string) \n
                 (canonicalize-headers headers) \n
                 (canonicalize-signed-headers headers) \n
                 (hash-payload request-payload))))

(defun canonicalize-uri (uri)
  "Canonicalizes URI as per https://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
   Specifically, each path segment is uri-encoded twice, and redundant or relative path components are removed."
  (let* ((full-uri (quri:uri uri))
         (path (quri:uri-path full-uri))
         (segments (uiop:split-string path :separator "/"))
         (segments-len (length segments)))
    (with-output-to-string (s)
      (write-string "/" s)

      (dotimes (i segments-len)
        (let ((segment (elt segments i))
              (next-segment (if (< (1+ i) segments-len)
                                (elt segments (1+ i)))))
          (unless (or (equal "." segment)
                      (equal "" segment)
                      (equal ".." segment)
                      (equal ".." next-segment))
            (write-string (quri:url-encode (quri:url-encode segment)) s)
            (unless (= i (1- segments-len))
              (write-string "/" s))))))))

(defun canonicalize-query-string (qs)
  "Canonicalizes query string to AWS rules as per https://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
   Expects the query string either as a string in the form 'Param1=Val1&Param2=Val2', with any equal signs in either names or values
   already encoded to %3D, or an alist of query params in the form ((Param1 . Val1) (Param2 . Val2)) with not-yet-encoded equal signs.
   Returns the query string as a string in the form 'Param1=Val1&Param2=Val2'"
  (let ((sorted (sort-query-params (if (stringp qs)
                                       (quri:uri-query-params (quri:uri (uiop:strcat "?" qs)))
                                       qs))))
    (dolist (param sorted)
      (setf (car param) (cl-ppcre:regex-replace-all "=" (car param) (quri:url-encode "=")))
      (setf (cdr param) (cl-ppcre:regex-replace-all "=" (cdr param) (quri:url-encode "="))))
    (let ((uri (quri:uri "")))
      (setf (quri:uri-query-params uri) sorted) ; implicitly encodes
      (quri:uri-query uri))))

(defun sort-query-params (params)
  "Given an alist of query params, sorts them by param name in ascending order.
   Params with duplicate names are then sorted by value."
  (sort params
        (lambda (p1 p2)
          (destructuring-bind ((k1 . v1) (k2 . v2)) (list p1 p2)
            (if (string= k1 k2)
                (string< v1 v2)
                (string< k1 k2))))))

(defun canonicalize-headers (headers)
  "Given an alist of headers, canonicalize them as per https://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
   The result will be a string with all header names lowercased and sorted, with each followed by a colon, a trimmed value, then a newline.
   The trimming process removes excess whitespace before/after the value and replaces sequential spaces with a single space."
  (setf headers (copy-alist headers))
  (dolist (header headers)
    (setf (car header) (string-downcase (car header)))
    (setf (cdr header) (trim (spaces-merge (cdr header)))))
  (setf headers (sort headers (lambda (h1 h2)
                                (string< (car h1) (car h2)))))
  (with-output-to-string (s)
    (dolist (header headers)
      (format s "~a:~a~%" (car header) (cdr header)))))

(defun trim (string)
  "Trims leading/ending whitespace"
  (cl-ppcre:regex-replace-all "^[\\s]+(.+?)[\\s]+$" string "\\1"))

(defun spaces-merge (string)
  "Merges sequential spaces into a single space"
  (cl-ppcre:regex-replace-all " {2,}" string " "))

(defun canonicalize-signed-headers (headers)
  "Return list of headers in alist of HEADERS used as the signed headers value in https://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html
   Each header name is lowercased, sorted, and joined with a semicolon separator. Values are not included."
  (format nil "~{~a~^;~}" (sort (mapcar (lambda (h) (string-downcase (car h))) headers)
                                #'string<)))


(defun create-initial-ses-headers (date-time host content-length)
  `(("Date" . ,(amazon-iso-8601-basic-time date-time))
    ("Host" . ,host)
    ("Content-Length" . ,(format nil "~a" content-length))
    ("Content-Type" . "application/x-www-form-urlencoded")))

(defun create-authorization-header (access-key date region service headers signature)
  "Creates the header value part of the Authorization header defined by https://docs.aws.amazon.com/general/latest/gr/sigv4-add-signature-to-request.html"
  (uiop:strcat "AWS4-HMAC-SHA256 Credential=" access-key "/" (credential-scope date region service)
               ", SignedHeaders=" (canonicalize-signed-headers headers)
               ", Signature=" signature))

