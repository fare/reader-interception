;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem "reader-interception"
  :version "1.0.0"
  :description "Intercept the reader to replace CL syntax with your own"
  :author "Francois-Rene Rideau"
  :license "MIT"
  :components ((:file "reader-interception"))
  :in-order-to ((test-op (test-op "reader-interception-test"))))
