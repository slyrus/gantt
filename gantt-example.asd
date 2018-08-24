

(asdf:defsystem :gantt-example
  :depends-on (gantt named-readtables)
  :serial t
  :components
  ((:file "gantt-example")))
