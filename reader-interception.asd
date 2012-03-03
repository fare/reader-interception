;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
(defsystem :reader-interception
  :description "Intercept the reader to replace CL syntax with your own"
  :components
  ((:file "reader-interception")))

(defmethod perform ((op test-op) (system (eql (find-system :reader-interception))))
  (asdf:load-system :reader-interception-test)
  (funcall (asdf::find-symbol* :test-suite :reader-interception-test)))
