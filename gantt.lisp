
(cl:defpackage :gantt
  (:use :cl)
  (:import-from :local-time
                #:encode-timestamp
                #:timestamp+
                #:timestamp+)
  (:import-from :time-interval
                #:time-interval)
  (:import-from :cl-who
                #:with-html-output
                #:fmt
                #:htm
                #:str)
  (:export #:task
           #:add-task
           #:find-task
           #:deftask
           #:defproject
           #:defgroup
           #:task-start
           #:task-end
           #:task-cost
           #:task-progress
           #:resource
           #:defresource

           #:dependency
           #:add-dependency

           #:dependencies
           #:start
           #:end
           #:duration

           #:print-task-tree
           #:read-task))

(cl:in-package :gantt)

;;;
;;; task class
(defclass task ()
  ((name :initarg :name :accessor name)
   (children :initarg :children :accessor children :initform (make-array 0 :fill-pointer t))
   (start :initarg :start :accessor task-start :initform nil)
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

(defun deftask (name &key task-start duration progress cost)
  (apply #'make-instance 'task :name name
         (append (when task-start
                   `(:task-start ,task-start))
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

(defgeneric task-end (task))

(defmethod task-end ((obj task))
  (cond ((and (task-start obj)
              (duration obj))
         (time-interval:t+
          (task-start obj) (duration obj)))
        ((task-start obj)
         (task-start obj))))

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
      (reduce (lambda (&optional a b)
                (cond ((null a) b)
                      ((null b) a)
                      (t (min a b))))
              (let ((children (children task)))
                (map (type-of children) #'start children)))))

(defun end (task)
  (or (task-end task)
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
              (let ((children (children task)))
                (map (type-of children) #'cost children)))))

(defun remove-keyword-arg (args remove-key)
  (loop for (key value) on args by #'cddr
     unless (eq remove-key key)
       append (list key value)))

(defun read-task (task-spec &optional task-tree)
  (let ((atom-or-list (car task-spec)))
    (let ((task (if (atom atom-or-list)
                    (deftask atom-or-list)
                    (destructuring-bind (name &rest args &key depends-on &allow-other-keys)
                        atom-or-list
                      (let ((task (apply #'deftask name (remove-keyword-arg args :depends-on))))
                        (when depends-on
                          (let ((dep-task
                                 (loop for parent-task in task-tree
                                    thereis (find-task depends-on parent-task))))
                            (when dep-task
                              (add-dependency dep-task task))))
                        task)))))
      (map nil (lambda (x)
                 (add-task task (read-task x (cons task task-tree))))
           (cdr task-spec))
      task)))

