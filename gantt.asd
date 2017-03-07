
(asdf:defsystem :gantt
  :depends-on (local-time time-interval named-readtables)
  :components
  ((:file "gantt")
   (:file "date-reader")))
