
(cl:defpackage :gantt
  (:use :cl)
  (:import-from :local-time
                #:encode-timestamp
                #:timestamp+
                #:timestamp+
                #:timestamp-minimum
                #:timestamp-maximum)
  (:import-from :time-interval
                #:time-interval)
  (:import-from :cl-who
                #:with-html-output
                #:fmt
                #:htm
                #:str)
  (:export #:task
           #:name
           #:add-task
           #:find-task
           #:find-task-named
           #:deftask
           #:defproject
           #:defgroup
           #:task-start
           #:task-end
           #:task-cost
           #:task-progress
           #:task-resources
           #:task-dependencies
           #:task-child

           #:resource
           #:defresource
           #:add-resource

           #:dependency
           #:add-dependency

           #:start
           #:end
           #:duration

           #:print-task-tree
           #:read-task))

(cl:in-package :gantt)

;;;
;;; task class
(defclass task ()
  ((id :initarg :id :accessor id)
   (name :initarg :name :accessor name :initform nil)
   (children :initarg :children :accessor children :initform (make-array 0 :fill-pointer t))
   (parent :initarg :parent :accessor parent :initform nil)
   (start :initarg :start :accessor task-start :initform nil)
   (duration :initarg :duration :accessor duration :initform nil)
   (dependencies :initarg :dependencies :accessor task-dependencies :initform nil)
   (cost :initarg :cost :accessor task-cost :initform nil)
   (progress :initarg :progress :accessor task-progress :initform nil)
   (resources :initarg :resources :accessor task-resources :initform nil)
   (notes :initarg :notes :accessor task-notes :initform nil)
   (critical :initarg :critical :accessor task-critical :initform nil)))

(defmethod print-object ((obj task) out)
  (print-unreadable-object (obj out :type t :identity t)
    (pprint-logical-block (out nil)
      (format out "~S ~S" (id obj) (name obj))
      (pprint-newline :fill out)
      (let ((children (children obj)))
        (when children
          (pprint-logical-block (out children :prefix "#(" :suffix ")")
            (loop (write (pprint-pop) :stream out) 
               (pprint-exit-if-list-exhausted) 
               (write-char #\space out) 
               (pprint-newline :fill out)))
          (pprint-newline :fill out))))))

;;
;; handy trick: now we can traverse child indices like so:
;;
;; (reduce #'gantt::task-child (list gantt-example::*example-project* 3 1))
;;
(defun task-child (task &optional (index 0))
  (elt (children task) index))

;;;
;;; dependency class
(defclass dependency ()
  ((task-a :initarg :task-a :accessor task-a :initform nil)
   (dependency-type-a :initarg :dependency-type-a :accessor dependency-type-a :initform nil)
   (task-b :initarg :task-b :accessor task-b :initform nil)
   (dependency-type-b :initarg :dependency-type-b :accessor dependency-type-b :initform nil)))

(defmethod print-object ((obj dependency) out)
  (print-unreadable-object (obj out :type t :identity t)
    (with-accessors ((a task-a)
                     (type-a dependency-type-a)
                     (b task-b)
                     (type-b dependency-type-b))
        obj
      (pprint-logical-block (out nil :prefix "#(" :suffix ")")
        (write a :stream out)
        (write-char #\space out)
        (write type-a :stream out)
        (pprint-newline :fill out)
        (write b :stream out)
        (write-char #\space out)
        (write type-b :stream out)))))

(defun %add-dependency (dependency)
  (with-accessors ((a task-a)
                   (b task-b))
      dependency
    (pushnew dependency (task-dependencies a) :key #'task-b)
    (pushnew dependency (task-dependencies b) :key #'task-a)))

(defun add-dependency (dep-a dep-b)
  (destructuring-bind (dep-a-obj dep-a-type)
      (if (listp dep-a)
          dep-a
          (list dep-a :finish))
    (destructuring-bind (dep-b-obj dep-b-type)
      (if (listp dep-b)
          dep-b
          (list dep-b :start))
      (%add-dependency (make-instance 'dependency
                                      :task-a dep-a-obj
                                      :dependency-type-a dep-a-type
                                      :task-b dep-b-obj
                                      :dependency-type-b dep-b-type)))))

(defun other-task (dependency task)
  (let ((taska (task-a dependency)))
    (if (eql taska task)
        (values (task-b dependency) (dependency-type-b dependency))
        (values (task-a dependency) (dependency-type-a dependency)))))

(defun self-task (dependency task)
  (let ((taska (task-a dependency)))
    (if (eql taska task)
        (values (task-a dependency) (dependency-type-a dependency))
        (values (task-b dependency) (dependency-type-b dependency)))))

(defun prerequisite-tasks (task)
  (declare (optimize (debug 3)))
  (let ((deps (task-dependencies task)))
    (loop for dep in deps
       append (multiple-value-bind (taska self-type)
                  (self-task dep task)
                (declare (ignore taska))
                (when (eql :start self-type)
                  (list (other-task dep task)))))))
;;;
;;; resource class
(defclass resource ()
  ((name :initarg :name :accessor name)))

(defmethod print-object ((obj resource) out)
  (print-unreadable-object (obj out :type t :identity t)
    (with-accessors ((name name ))
        obj
      (write name :stream out))))

(defun defresource (name)
  (make-instance 'resource :name name))

;;;
;;; note class
(defclass note ()
  ((name :initarg :name :accessor name)))

(defmethod print-object ((obj note) out)
  (print-unreadable-object (obj out :type t :identity t)
    (with-accessors ((name name ))
        obj
      (write name :stream out))))

(defun defnote (name)
  (make-instance 'note :name name))


(defun print-task-tree (task &key stream (indent 0))
  (let ((out (or stream
                 (make-string-output-stream))))
    (labels ((%print-task-tree (task indent)
               (format out "~&")
               (dotimes (i indent)
                 (write-char #\space out))
               (format out "~A" (or (name task) (id task)))
               (map nil
                    (lambda (x)
                      (%print-task-tree x (+ indent 2)))
                    (children task))))
      (%print-task-tree task indent))
    (unless stream
      (get-output-stream-string out))))

(defun flatten-task-tree (tree)
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (when (children subtree)
                   (map nil #'traverse (children subtree)))
                 (push subtree list))))
      (traverse tree))
    (nreverse list)))

(defun timestamp-ymd (timestamp)
  (local-time:format-timestring nil timestamp
                                :format '((:year 4) #\- (:month 2) #\- (:day 2))))

(defun defproject (name)
  (make-instance 'task :name name))

(defun defgroup (name)
  (make-instance 'task :name name))

(defun deftask (id &key name start duration progress cost parent notes critical)
  (apply #'make-instance 'task :id id
         (append (when name
                   `(:name ,name))
                 (when start
                   `(:start ,start))
                 (when duration
                   `(:duration ,(if (stringp duration)
                                    (time-interval:parse-time-interval-string duration)
                                    duration)))
                 (when cost
                   `(:cost ,cost))
                 (when progress
                   `(:progress ,progress))
                 (when parent
                   `(:parent ,parent))
                 (when notes
                   `(:notes ,notes))
                 (when critical
                   `(:critical ,critical)))))

;; find-task looks DOWN in task tree
(defun find-task (item task &key (test #'equal) (key #'id))
  (labels ((%find-task (task)
             (when task
               (if 
                (and (atom task) (funcall test (funcall key task) item))
                (return-from find-task task)
                (let ((children (children task)))
                  (find item children :key #'%find-task :test test))))))
    (%find-task task)))

(defun find-task-named (name task)
  (find-task name task :key #'name))

(defun add-resource (resource task)
  (pushnew resource (task-resources task)))

;;; Resource looks UP in task tree
(defun find-resource (name resource &key (test #'equal))
  (labels ((%find-resource (resource)
             (when resource
               (if
                (and (atom resource) (funcall test (name resource) name))
                (return-from find-resource resource)
                (let ((children (children resource)))
                  (find name children :key #'%find-resource :test test))))))
    (%find-resource resource)))

(defun add-note (note task)
  (pushnew note (task-notes task)))

(defun top-task (task)
  (let ((parent (parent task)))
    (if parent
        (top-task parent)
        task)))

(defgeneric task-end (task))

(defmethod task-end ((obj task))
  (cond ((and (task-start obj)
              (duration obj))
         (time-interval:t+
          (task-start obj) (duration obj)))))

(defun task-finished-p (task)
  (let ((progress (task-progress task)))
    (and progress (>= progress 1.0))))

;; functions
(defun add-child (parent child)
  (vector-push-extend child (children parent)))

(defun add-task (parent child &key start duration depends-on)
  "Adds a task to the parent and returns the added (child) task."
  (add-child parent child)
  (when start
    (setf (task-start child)
          start))
  (when duration
    (setf (duration child)
          duration))
  (when depends-on
    (flet ((add-dep (prerequisite-task)
             (add-dependency prerequisite-task child)))
      (cond ((atom depends-on)
             (add-dep depends-on))
            ((listp depends-on)
             (map nil #'add-dep depends-on)))))
  child)

(defun start (task)
  (or (task-start task)
      (let ((first-child-start (first-child-task-start task))
            (last-prereq-end (last-prereq-task-end task)))
        (cond ((and first-child-start last-prereq-end)
               (apply #'local-time:timestamp-maximum
                      (append (alexandria:ensure-list (first-child-task-start task))
                              (alexandria:ensure-list (last-prereq-task-end task)))))
              (first-child-start first-child-start)
              (last-prereq-end last-prereq-end)))))

(defun last-ending-task (tasks)
  (car (sort (remove-if #'null tasks :key 'end) #'local-time:timestamp> :key 'end)))

(defun first-child-task-start (task)
  (reduce (lambda (&optional a b)
            (cond ((null a) b)
                  ((null b) a)
                  (t (timestamp-minimum a b))))
          (let ((children (children task)))
            (map 'vector #'start children))))

(defun last-child-task-end (task)
  (reduce (lambda (&optional a b)
            (cond ((null a) b)
                  ((null b) a)
                  (t (timestamp-maximum a b))))
          (let ((children (children task)))
            (map 'vector #'end children))))

(defun last-prereq-task-end (task)
  (let ((last-prereq (last-ending-task (prerequisite-tasks task))))
     (when last-prereq
       (if (duration task)
           (time-interval:t+
            (end last-prereq) (duration task))
           (end last-prereq)))))

(defun end (task)
  ;; use task end if provided
  (or (task-end task)
      ;; if not use the latter of task-end of children or the end of the prereqs + the duration
      (let ((prereq-ends (last-prereq-task-end task))
            (duration (duration task)))
        (let ((candidate-end-timestamps
               (append (alexandria:ensure-list (last-child-task-end task))
                       (alexandria:ensure-list
                        (when prereq-ends
                          (if duration
                              (time-interval:t+ prereq-ends duration)
                              prereq-ends))))))
          (when candidate-end-timestamps
            (apply #'local-time:timestamp-maximum candidate-end-timestamps))))))

(defun cost (task)
  (unless (task-finished-p task)
    (or (task-cost task)
        (reduce (lambda (&optional a b)
                  (cond ((null a) b)
                        ((null b) a)
                        (t (+ a b))))
                (let ((children (children task)))
                  (map 'vector #'cost children))))))

(defun remove-keyword-arg (args remove-keys)
  (loop for (key value) on args by #'cddr
     unless (member key remove-keys)
       append (list key value)))

(defun read-task (task-spec &optional task-tree)
  (let ((atom-or-list (car task-spec)))
    (let ((task (if (atom atom-or-list)
                    (deftask atom-or-list)
                    (destructuring-bind (id &rest args
                                            &key depends-on resources
                                            &allow-other-keys)
                        atom-or-list
                      (let ((task (apply #'deftask id
                                         :parent (car task-tree)
                                         (remove-keyword-arg args '(:resources :depends-on)))))
                        (when depends-on
                          (loop for dependency in depends-on
                             do
                               (let ((dep-task
                                      (loop for parent-task in task-tree
                                         thereis (find-task dependency parent-task))))
                                 (if dep-task
                                     (add-dependency dep-task task)
                                     (warn "Missing dependency ~A" dependency)))))
                        (when resources
                          (loop for resource in resources
                             do
                               (let ((rsrc
                                      (loop for parent-task in task-tree
                                         thereis (find-resource resource parent-task))))
                                 (if rsrc
                                     (add-resource rsrc task)
                                     (let ((rsrc (defresource resource)))
                                       (add-resource rsrc task))))))
                        task)))))
      (map nil (lambda (x)
                 (add-task task (read-task x (cons task task-tree))))
           (cdr task-spec))
      task)))

