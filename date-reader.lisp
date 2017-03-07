
(cl:defpackage :gantt-reader
  (:use :cl)
  (:import-from :named-readtables
                #:defreadtable)
  (:export #:date-reader))

(cl:in-package :gantt-reader)

(defun %read-interval (stream char arg)
  (declare (ignore char arg))
  (time-interval:parse-time-interval-string
   (with-output-to-string (str)
     (loop for c = (read-char stream nil)
        while (and c (or (digit-char-p c) (member c '(#\y #\m #\w #\d #\h #\s #\n #\o #\i #\Y #\M #\W #\D #\H #\S #\N #\O #\I))))
        do (princ c str)
        finally (when c (unread-char c stream))))))

(named-readtables:defreadtable date-reader
  (:merge :standard)
  (:macro-char #\@ #'local-time::%read-timestring)
  (:dispatch-macro-char #\# #\I #'%read-interval))

