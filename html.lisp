
(cl:in-package :gantt)

(defmacro with-html-output* (args &body (body))
  `(cl-who:with-html-output ,args ,@(eval body)))

(defparameter *row-colors* '("#ffffff" "#f7f7f7"
                             "#efefef" "#e7e7e7"
                             "#dfdfdf" "#d7d7d7"
                             "#cfcfcf" "#c7c7c7"))

(defun write-task-tree-html (path task &key (indent 0)
                                            (show-finished-tasks t)
                                            (show-start t)
                                            (show-end t)
                                            (show-resources t)
                                            (show-dependencies nil)
                                            (row-colors *row-colors*))
  ;; copy resources to path
  (let ((dest-directory (pathname-directory (merge-pathnames *default-pathname-defaults* path))))
    (let ((src-js (merge-pathnames "js/"
                                   (asdf:component-pathname
                                    (asdf:find-system :gantt))))
          (dest-js (merge-pathnames "js/" (make-pathname :directory dest-directory))))
      (copy-directory:copy src-js dest-js))
    (let ((src-css (merge-pathnames "css/"
                                    (asdf:component-pathname
                                     (asdf:find-system :gantt))))
          (dest-css (merge-pathnames "css/" (make-pathname :directory dest-directory))))
      (copy-directory:copy src-css dest-css))
    (with-open-file (s path :direction :output :if-does-not-exist :create :if-exists :supersede)
      (with-html-output (s nil :indent t)
        (labels ((%print-task-tree-html (task indent)
                   (unless (and (not show-finished-tasks)
                                (and (task-progress task)
                                     (>= (task-progress task) 1)))
                     (htm
                      (:tr :class "collapse"
                           :data-depth indent
                           :style
                           (let* ((index (mod indent (length row-colors)))
                                  (color (elt row-colors index)))
                             (format nil "background-color: ~A;" color))
                           (:td
                            (when (plusp (length (children task)))
                              (htm (:span :class "toggle"))))
                           (:td :style
                                (if (plusp (length (children task)))
                                    (format nil "font-weight: bold; text-indent: ~Dem; padding-left: ~Dem;"
                                            0 indent)
                                    (format nil "text-indent: ~Dem; padding-left: ~Dem;"
                                            0 indent))
                                (str (name task)))
                           (when show-start
                             (htm
                              (:td (str
                                    (let ((start (task-start task)))
                                      (when start (timestamp-ymd start)))))))
                           (when show-end
                             (htm
                              (:td (str
                                    (let ((end (task-end task)))
                                      (when end (timestamp-ymd end)))))))
                           (let ((task-cost (task-cost task)))
                             (if (and (not (task-finished-p task)) task-cost)
                                 (htm
                                  (:td :style
                                       (format nil "text-align: right; text-indent: ~Dem; padding-left: ~Dem;"
                                               0 indent)
                                       (str (format nil "~@[$~,'*:D~]" task-cost))))
                                 (htm
                                  (:td :style
                                       (if (plusp (length (children task)))
                                           (format nil "font-weight: bold; text-indent: ~Dem; padding-left: ~Dem;"
                                                   0 indent)
                                           (format nil "text-indent: ~Dem; padding-left: ~Dem;" 0 indent))
                                       (str (format nil "~@[$~,'*:D~]" (cost task)))))))
                           (:td (str (if (task-finished-p task)
                                         "Finished"
                                         (task-progress task))))
                           (when show-resources
                             (htm
                              (:td
                               (let ((resources (task-resources task)))
                                 (when resources
                                   (htm (:ul
                                         (loop for resource in resources
                                            do
                                              (htm
                                               (:li (str (name resource))))))))))))
                           (when show-dependencies
                             (htm
                              (:td
                               (let ((dependencies (task-dependencies task)))
                                 (when dependencies
                                   (htm (:ul
                                         (loop for dependency in dependencies
                                            do
                                              (let ((task-a (task-a dependency))
                                                    (task-b (task-b dependency)))
                                                (htm
                                                 (:li (str (format nil "~A -> ~A"
                                                                   (name task-a)
                                                                   (name task-b)))))))))))))))
                      (when (children task)
                        (htm
                         (map nil
                              (lambda (x)
                                (htm
                                 (str (%print-task-tree-html x (+ indent 1)))))
                              (children task))))))))
          (htm
           (:html :lang "en"
                  (:head
                   (:meta :charset "utf-8")
                 (:meta :http-equiv "X-UA-Compatible" :content "ID=edge")
                 (:meta :name "viewport" :content "width=device-width, initial-scale=1")
                 (:link :rel "stylesheet" :type "text/css" :href "css/gantt.css")
                 (:script :src "js/jquery-3.1.1.js" :type "text/javascript")
                 (:script :src "js/gantt.js" :type "text/javascript")
                 (:style :type "text/css" "
* { font-family: Arial, Helvetica, sans-serif }
")
                 (:title (str (name task))))
                (:body
                 (progn
                   (htm
                    (:table :id "mytable"
                            (:tr
                             (:th)
                             (:th "Task Name")
                             (when show-start
                               (htm
                                (:th "Start")))
                             (when show-end
                               (htm
                                (:th "End")))
                             (:th "Cost")
                             (:th "Progress")
                             (when show-resources
                               (htm
                                (:th "Resources")))
                             (when show-dependencies
                               (htm
                                (:th "Dependencies"))))
                            (%print-task-tree-html task indent)))))))))
    path)))

