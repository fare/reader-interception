(in-package :cl-user)

(defpackage :reader-interception
  (:use :cl)
  (:export
   #:with-reader-interception
   #:prepare-reader-interception))

(in-package :reader-interception)

(defmacro with-reader-interception ((hint reader) &body body)
  "You may override the Common Lisp reader, e.g. around LOAD or COMPILE-FILE,
by using WITH-READER-INTERCEPTION (HINT READER) BODY...
thus replacing the CL syntax with any syntax of your choice.
By combining this with ASDF 2.018's :around-compile hook,
you could compile any language with CL via a proper translation frontend.
You *must* specify a HINT to pass to PREPARE-READER-INTERCEPTION,
to help the interception find the first (next) character that will be read,
so it can intercept reading from there.
You must specify your READER, a function taking a STREAM as argument,
and either returning an object or raising an error
such as READER-ERROR or END-OF-FILE.
Inside the BODY, the *READTABLE* is set so that you may READ
from a stream starting with the hinted character, and
it will use your specified parser instead of the CL reader.
Inside the READER itself, READ will behave like the unintercepted CL reader."
  `(call-with-reader-interception (lambda () ,@body) ,hint ,reader))

(defun prepare-reader-interception (hint &key (external-format :default))
  "Before you may intercept the CL reader, you MUST prime
the reader-interception with the first character to be read.
You may give the character itself,
NIL for no character (e.g. EOF),
T for any ASCII characters (assuming the first one won't be further unicode),
a stream to read the character from,
a string that will be used as input stream,
a pathname to be open as a stream to read from with specified external-format"
  (etypecase hint
    (null
     nil)
    ((eql t)
     (prepare-all-ascii))
    (character
     (prepare-character hint))
    (string
     (when (plusp (length hint))
       (prepare-character (aref hint 0))))
    (pathname
     (with-open-file (s hint :direction :input
                        :if-does-not-exist :error
                        :element-type 'character
                        :external-format external-format)
       (prepare-reader-interception s)))                      
    (stream
     (let ((next (read-char hint nil nil)))
       (when next
         (unread-char next hint)
         (prepare-character next)))))
  (values))

(defvar *saved-readtable* *readtable*)
(defvar *reader* 'standard-read)
(defvar *interception-readtable* (copy-readtable nil))
(defvar *intercepted-characters* (make-hash-table :test 'equal))

(defun initialize-interception ()
  (setf *interception-readtable* (copy-readtable nil))
  (setf *intercepted-characters* (make-hash-table :test 'equal)))

(defvar *standard-readtable* (copy-readtable nil))
(defun standard-read (&key
                      (stream *standard-input*)
                      (eof-error-p t) (eof-value nil)
                      (recursive-p nil))
  (let ((*readtable* *standard-readtable*))
    (with-standard-io-syntax
      (read-preserving-whitespace
       stream eof-error-p eof-value recursive-p))))

(defun prepare-character (x)
  (unless (gethash x *intercepted-characters*)
    (setf (gethash x *intercepted-characters*) t)
    (set-macro-character x 'intercept-char-reader nil
                         *interception-readtable*)))

(defun intercept-char-reader (stream char)
  (unread-char char stream)
  (prog1 (let ((*readtable* *saved-readtable*))
           (funcall *reader* stream))
    (prepare-reader-interception stream)))

(defun prepare-all-ascii ()
  ;; assumes code-char uses ASCII for codepoints below 128
  (loop :for c :below 128 :do
    (prepare-character (code-char c))))

(defun call-with-reader-interception (thunk &optional hint (reader *reader*))
  (prepare-reader-interception hint)
  (let ((*reader* reader)
        (*saved-readtable* *readtable*)
        (*readtable* *interception-readtable*))
    (funcall thunk)))
