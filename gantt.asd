
;; (ql:quickload '(local-time time-interval named-readtables cl-who))

(asdf:defsystem :gantt
  :depends-on (local-time time-interval named-readtables cl-who)
  :components
  ((:file "gantt")
   (:file "date-reader")))
