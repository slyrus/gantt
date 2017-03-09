
(cl:in-package :gantt)

(defmacro with-html-output* (args &body (body))
  `(cl-who:with-html-output ,args ,@(eval body)))

(defparameter *row-colors* '("#fff" "#eee" "#ddd" "#ccc" "#bbb" "#aaa"))

(defun write-task-tree-html (path task &key (indent 0))
  (with-open-file (s path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (with-html-output (s nil :indent t)
      (labels ((%print-task-tree-html (task indent)
                 (htm
                  (:tr :class "collapse"
                       :data-depth indent
                       :style
                       (let* ((index (mod indent (length *row-colors*)))
                              (color (elt *row-colors* index)))
                         (format nil "background-color: ~A;" color))
                       (:td
                        (when (plusp (length (children task)))
                          (htm (:span :class "toggle"))))
                       (:td :style
                            (format nil "text-indent: ~Dem; padding-left: ~Dem;" 0 indent)
                            (str (name task)))
                       (:td (str
                             (let ((start (task-start task)))
                               (when start (timestamp-ymd start)))))
                       (:td (str
                             (let ((end (task-end task)))
                               (when end (timestamp-ymd end)))))
                       (:td :style
                            (format nil "text-align: right;")
                            (str (format nil "~@[$~,'*:D~]" (task-cost task))))
                       (:td (str (if (task-finished-p task)
                                     "Finished"
                                     (task-progress task)))))
                  (when (children task)
                    (htm
                     (map nil
                          (lambda (x)
                            (htm
                             (str (%print-task-tree-html x (+ indent 1)))))
                          (children task)))))))
        (htm
         (:html :lang "en"
                (:head
                 (:meta :charset "utf-8")
                 (:meta :http-equiv "X-UA-Compatible" :content "ID=edge")
                 (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                 (:link :rel "stylesheet" :type "text/css" :href "gantt.css")
                 (:script :src "jquery-3.1.1.js" :type "text/javascript")
                 (:script :src "gantt.js" :type "text/javascript")
                 (:style :type "text/css" "
* { font-family: Arial, Helvetica, sans-serif }
")
                 (:title "Title!"))
                (:body
                 (progn
                   (htm
                    (:table :id "mytable" :border 0 :cellpadding 4
                            (:tr
                             (:th)
                             (:th "Task Name")
                             (:th "Start")
                             (:th "End")
                             (:th "Cost")
                             (:th "Progress"))
                            (%print-task-tree-html task indent)))))))))
    path))

