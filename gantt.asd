
;; (ql:quickload '(local-time time-interval named-readtables cl-who copy-directory))

(asdf:defsystem :gantt
  :depends-on (local-time time-interval named-readtables cl-who copy-directory alexandria)
  :serial t
  :components
  ((:file "gantt")
   (:file "date-reader")
   (:file "html")
   (:file "mermaid")))
