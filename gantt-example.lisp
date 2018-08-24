(cl:defpackage #:gantt-example
  (:use #:cl #:gantt #:named-readtables)
  (:import-from #:gantt-reader
                #:date-reader))

(cl:in-package #:gantt-example)

(in-readtable date-reader)

(defparameter *example-project*
  (read-task
   `((:kitchen
      :name "Kitchen"
      :start @2018-09-01
      :resources ("Joe's Plumbing" "Warped Tile" "Bogus Countertops" "Shady Electrical Work" "Hardly Wood Floors"))
     ((:demo
       :name "Demo Old Kitchen"
       :start @2018-09-01
       :duration "2W"))
     ((:floor
       :name "New Floor"
       :start @2018-09-20
       :duration "2W"
       :resources ("Hardly Wood Floors")
       :depends-on (:demo)))
     ((:drywall
       :name "Drywall"
       :start @2018-10-01
       :duration "2W"
       :depends-on (:demo)))
     ((:cabinets :name "New Cabinets")
      ((:install-cabinets
        :name "Install Cabinets"
        :start @2018-10-15
        :duration "4W"
        :depends-on (:demo :drywall)))
      ((:paint-cabinets
        :name "Paint Cabinets"
        :start @2018-11-15
        :duration "2W"
        :depends-on (:install-cabinets))))
     ((:plumbing :name "Plumbing")
      ((:install-sink
        :name "Install Sink"
        :start @2018-11-01
        :duration "1D"
        :depends-on (:floor)))))))
