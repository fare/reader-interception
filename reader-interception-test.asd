;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(defsystem "reader-interception-test"
  :version "1.0.0"
  :description "tests for reader-interception"
  :author "Francois-Rene Rideau"
  :license "MIT"
  :depends-on ("reader-interception" "fare-utils" "hu.dwim.stefil")
  :components ((:file "reader-interception-test"))
  :perform (test-op (o c) (symbol-call :reader-interception-test :test-suite)))
