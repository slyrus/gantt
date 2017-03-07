
(cl:defpackage :gantt
  (:use :cl)
  (:import-from :local-time
                #:encode-timestamp
                #:timestamp+
                #:timestamp+)
  (:import-from :time-interval
                #:time-interval)
  (:export #:task
           #:add-task
           #:find-task
           #:deftask
           #:defproject
           #:defgroup

           #:dependency
           #:add-dependency

           #:start
           #:end
           #:day-timestamp
           #:duration))

(cl:in-package :gantt)

;;;
;;; task class
(defclass task ()
  ((name :initarg :name :accessor name)
   (children :initarg :children :accessor children :initform (make-array 0 :fill-pointer t))
   (start-timestamp :initarg :start-timestamp :accessor start-timestamp :initform nil)
   (duration :initarg :duration :accessor duration :initform nil)
   (dependencies :initarg :dependencies :accessor dependencies :initform nil)
   (cost :initarg :cost :accessor task-cost :initform nil)
   (progress :initarg :progress :accessor task-progress :initform nil)))

(defmethod print-object ((obj task) out)
  (print-unreadable-object (obj out :type t :identity t)
    (pprint-logical-block (out nil)
      (format out "~s " (name obj))
      (pprint-newline :fill out)
      (let ((children (children obj)))
        (when children
          (pprint-logical-block (out children :prefix "#(" :suffix ")")
            (loop (write (pprint-pop) :stream out) 
               (pprint-exit-if-list-exhausted) 
               (write-char #\space out) 
               (pprint-newline :fill out)))
          (pprint-newline :fill out))))))

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
    (pushnew dependency (dependencies a) :key #'task-b)
    (pushnew dependency (dependencies b) :key #'task-a)))

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

(defun print-task-tree (task &key stream (indent 0))
  (let ((out (or stream
                 (make-string-output-stream))))
    (labels ((%print-task-tree (task indent)
               (format out "~&")
               (dotimes (i indent)
                 (write-char #\space out))
               (format out "~A" (name task))
               (map nil
                    (lambda (x)
                      (%print-task-tree x (+ indent 2)))
                    (children task))))
      (%print-task-tree task indent))
    (unless stream
      (get-output-stream-string out))))


(defun defproject (name)
  (make-instance 'task :name name))

(defun defgroup (name)
  (make-instance 'task :name name))

(defun deftask (name &key start-timestamp duration progress cost)
  (apply #'make-instance 'task :name name
         (append (when start-timestamp
                   `(:start-timestamp ,start-timestamp))
                 (when duration
                   `(:duration ,(if (stringp duration)
                                    (time-interval:parse-time-interval-string duration)
                                    duration)))
                 (when cost
                   `(:cost ,cost))
                 (when progress
                   `(:progress ,progress)))))

(defun find-task (name task &key (test #'equal))
  (labels ((%find-task (task)
             (when task
               (if 
                (and (atom task) (funcall test (name task) name))
                (return-from find-task task)
                (let ((children (children task)))
                  (find name children :key #'%find-task :test test))))))
    (%find-task task)))

(defgeneric end-timestamp (task))

(defmethod end-timestamp ((obj task))
  (cond ((and (start-timestamp obj)
              (duration obj))
         (time-interval::timestamp-add-interval
          (start-timestamp obj) (duration obj)))
        ((start-timestamp obj)
         (start-timestamp obj))))

;; functions
(defun add-child (parent child)
  (vector-push-extend child (children parent)))

(defun add-task (parent child &key start duration depends-on)
  "Adds a task to the parent and returns the added (child) task."
  (add-child parent child)
  (when start
    (setf (start-timestamp child)
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
  (or (start-timestamp task)
      (reduce (lambda (&optional a b)
                (cond ((null a) b)
                      ((null b) a)
                      (t (min a b))))
              (let ((children (children task)))
                (map (type-of children) #'start children)))))

(defun end (task)
  (or (end-timestamp task)
      (reduce (lambda (&optional a b)
                (cond ((null a) b)
                      ((null b) a)
                      (t (max a b))))
              (let ((children (children task)))
                (map (type-of children) #'end children)))))

(defun cost (task)
  (or (task-cost task)
      (reduce (lambda (&optional a b)
                (cond ((null a) b)
                      ((null b) a)
                      (t (+ a b))))
              (mapcar #'cost
                      (children task)))))

(defun day-timestamp (day month year)
  (encode-timestamp 0 0 0 0 day month year))


