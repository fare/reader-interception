;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :reader-interception-test
  :depends-on (:reader-interception :fare-utils :hu.dwim.stefil)
  :components
  ((:file "reader-interception-test")))
