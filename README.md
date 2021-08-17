# CL-SES4
[![Build Status](https://jenkins.thejach.com/buildStatus/icon?job=cl-ses4&style=plastic)](https://jenkins.thejach.com/job/cl-ses4/)

This is a library for using Amazon Web Services (AWS) [Simple Email
Service](https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-ses-api-authentication.html)
(SES) to send emails using the newer [Signature Version
4](https://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html)
signing process.

# Usage

## Stand-alone program

The included `build.sh` script will produce a binary with SBCL.

```
$ ./build.sh
$ ./ses-send-email -h
Usage: ses-send-email -k FILE [OPTIONS]
  -h  --help             Display this help menu
  -k  --keys=FILE        The AWS credentials file to use. Should be in the form
(("aws-access-key" . "")
 ("aws-secret-key" . ""))

  -e  --endpoint=URL     The Amazon SES endpoint to use. If an endpoint is not specified, the default of "https://email.us-east-1.amazonaws.com/" is used.
  -f  --from=FROM_EMAIL  The email of the sender.
  -t  --to=TO_EMAIL      The email of the receiver.
  -s  --subject=SUBJECT  The subject of the email.
  -m  --message=MESSAGE  The email body.
  -r  --raw              Send a raw email. Data will be read from STDIN.

Examples:
ses-send-mail -k aws-credentials -r < /my-email.txt
cat my-email.txt | ses-send-mail -k aws-credentials -r
ses-send-mail -k aws-credentials -f noreply@example.com -t user@example.com -s Subject -m 'Hello User'
```

The binary also works as the target for PHP's `mail()`, i.e. configure your
`php.ini` file to include the line:

`sendmail_path = /path/to/ses-send-email -k /path/to/aws-credentials -r`

## Library

After loading the system `cl-ses4`, the main package is `ses4`.  Bind
`ses4:*ses-host*` and `ses4:*ses-region*` if you need to be different from the
default (SES verifies your emails region-by-region), also set
`ses4:*access-key*` and `ses4:*secret-key*` to your respective credentials.

You can then call `(ses4:send-simple-email ...)` or `(ses4:send-raw-email ...)`
depending on your needs.

# Limitations

This was made primarily to solve my own use case, as such I have made no special
effort to support things like multiple recipients, CC/BCC, more expressive email
addresses, special support for HTML emails beyond setting the header yourself in
a raw email, using any of the other not-email-sending API endpoints that SES
exposes, more specific errors and error handling than what's present, or
validating anything works in other Common Lisp implementations. (The tests did
pass in CCL.)

If you have a specific feature request not yet supported, or run into a bug,
feel free to file an Issue or submit a Pull Request.

# Copyright License

This library is free software released into the public domain. See UNLICENSE for
more details.

# Motivation

Amazon deprecated the old version 3 signing process, which will at some point
(likely March 2021) break the 10+ year old Perl script I've been using. There is
another Common Lisp project related to sending emails with SES,
[cl-ses](https://github.com/CodyReichert/cl-ses/), however it too uses the old
version 3 signing process and I felt like it would be easier to start from
scratch. Perhaps in the future we can merge into a single project, or a new
project can supersede both.
