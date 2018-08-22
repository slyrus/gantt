
(cl:in-package :gantt)

(defun mermaid-print-time-interval (interval &optional stream)
  (with-accessors ((years time-interval::interval-years)
                   (months time-interval::interval-months)
                   (weeks time-interval::interval-weeks)
                   (days time-interval::interval-days)
                   (hours time-interval::interval-hours)
                   (minutes time-interval::interval-minutes)
                   (seconds time-interval::interval-seconds)
                   (nanoseconds time-interval::interval-nanoseconds))
      interval
    (let ((mermaid-weeks (+ (* 52 years) (* 4.5 months) weeks)))
      (format stream "~@[ ~Dw~]~@[ ~Dd~]~@[~Dh~]"
              (unless (zerop mermaid-weeks) (round mermaid-weeks))
              (unless (zerop days) (round days))
              (unless (zerop hours) (round hours))))))


(defparameter *start-cutoff-date*
  (local-time:encode-timestamp 0 0 0 0 1 1 2018))

(defun last-ending-task (tasks)
  (sort tasks #'local-time:timestamp> :key 'task-end))

(defun write-mermaid-file (file task &key (show-finished-tasks t)
                                          (show-start t)
                                          (show-end t)
                                          (show-resources t)
                                          (show-dependencies nil)
                                          (row-colors *row-colors*)
                                          (start-cutoff-date *start-cutoff-date*))
  (declare (ignore show-finished-tasks show-start show-end show-resources show-dependencies row-colors))
  (with-open-file (s file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format s "gantt")
    (format s "~&dateFormat  YYYY-MM-DD")
    (format s "~&title ~A~%~%" (name task))

    (labels ((%print-task-tree-mermaid (task &key leaf start (level 0))
               (print (list (name task) (length (children task))))
               (if (plusp (length (children task)))
                   (loop :for child :across (children task)
                      :do (%print-task-tree-mermaid child
                                                    :leaf t
                                                    :start (or (start task) start)
                                                    :level (1+ level)))
                   (progn
                     (if leaf
                         (let ((start (or (start task) start))
                               (duration (duration task)))
                           (when (and start
                                      duration
                                      (local-time:timestamp> start start-cutoff-date))
                             (format s "~A :~A,~A,~A,~A~&"
                                     (name task)
                                     "active"
                                     (id task)
                                     (local-time:format-timestring nil start :format local-time:+iso-8601-date-format+)
                                     (mermaid-print-time-interval duration))))
                         (format s "~&section ~A~&" (name task)))))))
      (%print-task-tree-mermaid task :start (start task)))))
