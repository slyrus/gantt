
;; (ql:quickload '(local-time time-interval named-readtables cl-who))

(asdf:defsystem :gantt
  :depends-on (local-time time-interval named-readtables cl-who)
  :serial t
  :components
  ((:file "gantt")
   (:file "date-reader")
   (:file "html")))
