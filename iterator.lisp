#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(defclass iterator ()
  ((object :initarg :object :accessor object)))

(defgeneric has-more (iterator))
(defgeneric next (iterator))
(defgeneric make-iterator (object &key &allow-other-keys))

(defclass list-iterator (iterator)
  ())

(defmethod has-more ((iterator list-iterator))
  (object iterator))

(defmethod next ((iterator list-iterator))
  (pop (object iterator)))

(defmethod make-iterator ((list list) &key)
  (make-instance 'list-iterator :object list))

(defclass vector-iterator (iterator)
  ((start :initarg :start :accessor start))
  (:default-initargs :start 0))

(defmethod has-more ((iterator vector-iterator))
  (< (start iterator) (length (object iterator))))

(defmethod next ((iterator vector-iterator))
  (prog1 (aref (object iterator) (start iterator))
    (incf (start iterator))))

(defmethod make-iterator ((vector vector) &key (start 0))
  (make-instance 'vector-iterator :object vector :start start))

(defclass array-iterator (vector-iterator)
  ((total-length :accessor total-length)))

(defmethod initialize-instance :after ((iterator array-iterator) &key object)
  (setf (total-length iterator) (array-total-size object)))

(defmethod has-more ((iterator array-iterator))
  (< (start iterator) (total-length iterator)))

(defmethod next ((iterator array-iterator))
  (prog1 (row-major-aref (object iterator) (start iterator))
    (incf (start iterator))))

(defmethod make-iterator ((array array) &key (start 0))
  (make-instance 'array-iterator :object array :start start))

(defclass stream-iterator (iterator)
  ((buffer :accessor buffer)
   (index :initform 1 :accessor index)
   (limit :initform 1 :accessor limit)))

(defmethod initialize-instance :after ((iterator stream-iterator) &key (buffer-size 4096) object)
  (setf (buffer iterator) (make-array buffer-size :element-type (stream-element-type object))))

(defmethod has-more ((iterator stream-iterator))
  (when (= (index iterator) (limit iterator))
    (setf (limit iterator) (read-sequence (buffer iterator) (object iterator)))
    (setf (index iterator) 0))
  (not (= 0 (limit iterator))))

(defmethod next ((iterator stream-iterator))
  (prog1 (aref (buffer iterator) (index iterator))
    (incf (index iterator))))

(defmethod make-iterator ((stream stream) &key (buffer-size 4096))
  (make-instance 'stream-iterator :object stream :buffer-size buffer-size))

(defclass directory-iterator (list-iterator)
  ())

(defmethod initialize-instance :after ((iterator directory-iterator) &key pathname)
  (setf (object iterator) (directory pathname)))

(defmethod make-iterator ((pathname pathname) &key)
  (make-instance 'directory-iterator :object pathname))

(defclass random-iterator (iterator)
  ((limit :initarg :limit :reader limit))
  (:default-initargs
   :object (make-random-state)
   :limit 1.0f0))

(defmethod has-more ((iterator random-iterator))
  T)

(defmethod next ((iterator random-iterator))
  (random (limit iterator) (object iterator)))

(defmethod make-iterator ((random-state random-state) &key (limit 1.0))
  (make-instance 'random-iterator :object random-state :limit limit))

(defmethod make-iterator ((symbol (eql :random)) &key (limit 1.0))
  (make-instance 'random-iterator :limit limit))

(defclass package-iterator (iterator)
  ((prefetch :initform NIL :accessor prefetch))
  (:default-initargs
   :status '(:internal :external :inherited)))

(defmethod initialize-instance :after ((iterator package-iterator) &key object status)
  (setf (object iterator) (package-iterator object status)))

(defmethod has-more ((iterator package-iterator))
  (cond ((prefetch iterator)
         (car (prefetch iterator)))
        (T
         (multiple-value-bind (more symb) (funcall (object iterator))
           (setf (prefetch iterator)
                 (cons more symb))
           more))))

(defmethod next ((iterator package-iterator))
  (cond ((prefetch iterator)
         (prog1 (cdr (prefetch iterator))
           (setf (prefetch iterator) NIL)))
        (T
         (nth-value 1 (funcall (object iterator))))))

(defmethod make-iterator ((package package) &key)
  (make-instance 'package-iterator :object package))

(defclass hash-table-iterator (iterator)
  ((prefetch :initform NIL :accessor prefetch)))

(defmethod initialize-instance :after ((iterator hash-table-iterator) &key object)
  (setf (object iterator) (hash-table-iterator object)))

(defmethod has-more ((iterator hash-table-iterator))
  (cond ((prefetch iterator)
         (car (prefetch iterator)))
        (T
         (multiple-value-bind (more key val) (funcall (object iterator))
           (setf (prefetch iterator) (list more key val))
           more))))

(defmethod next ((iterator hash-table-iterator))
  (cond ((prefetch iterator)
         (prog1 (rest (prefetch iterator))
           (setf (prefetch iterator) NIL)))
        (T
         (multiple-value-bind (more key val) (funcall (object iterator))
           (declare (ignore more))
           (list key val)))))

(defmethod make-iterator ((hash-table hash-table) &key)
  (make-instance 'hash-table-iterator :object hash-table))
