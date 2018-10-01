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
     ((:electrical :name "Electrical")
      ((:run-wires
        :name "Run Wires"
        :start @2018-09-01
        :duration "2W"
        :critical t
        :depends-on (:demo)))
      ((:install-outlets
        :name "Install Outlets"
        :duration "1W"
        :depends-on (:run-wires)))
      ((:install-lights
        :name "Install Lights"
        :duration "1W"
        :critical t
        :depends-on (:run-wires))))
     ((:drywall
       :name "Drywall"
       :start @2018-10-01
       :duration "2W"
       :critical t
       :depends-on (:demo :run-wires)))
     ((:ceiling :name "Ceiling")
      ((:install-ceiling
        :name "Install Ceiling"
        :start @2018-10-01
        :duration "2W"
        :critical t
        :depends-on (:demo)))
      ((:install-ceiling
        :name "Paint Ceiling"
        :duration "2W"
        :critical t
        :depends-on (:install-ceiling))))
     ((:cabinets :name "New Cabinets")
      ((:install-cabinets
        :name "Install Cabinets"
        :start @2018-10-15
        :duration "4W"
        :critical t
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
