
(cl:in-package :gantt)

(defun write-mermaid-file (file task &key (show-finished-tasks t)
                                          (show-start t)
                                          (show-end t)
                                          (show-resources t)
                                          (show-dependencies nil)
                                          (row-colors *row-colors*))
  (declare (ignore show-finished-tasks show-start show-end show-resources show-dependencies row-colors))
  (with-open-file (s file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format s "gantt")
    (format s "~&dateFormat  YYYY-MM-DD")
    (format s "~&title ~A~%~%" (name task))

    (labels ((%print-task-tree-mermaid (task &key leaf start)
               (print (list (name task) (length (children task))))
               (if (plusp (length (children task)))
                   (loop :for child :across (children task)
                      :do (%print-task-tree-mermaid child :leaf t :start (or (start task) start)))
                   (progn
                     (if leaf
                         (let ((start (or (start task) start)))
                           (when start
                             (format s "~A : ~A,~A~&" (name task)
                                     (local-time:format-timestring nil start :format local-time:+iso-8601-date-format+)
                                     (local-time:format-timestring nil start :format local-time:+iso-8601-date-format+))))
                         (format s "~&section ~A~&" (name task)))))))
      (%print-task-tree-mermaid task :start (start task)))))
