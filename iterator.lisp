#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for.iterator)

(defgeneric has-more (iterator))
(defgeneric next (iterator))
(defgeneric current (iterator))
(defgeneric (setf current) (value iterator))
(defgeneric end (iterator))
(defgeneric make-iterator (object &key &allow-other-keys))
(defgeneric step-functions (iterator))

(defclass iterator ()
  ((object :initarg :object :accessor object)
   (current :accessor current)))

(defmethod next :around ((iterator iterator))
  (setf (slot-value iterator 'current) (call-next-method)))

(defmethod end ((iterator iterator)))

(defmethod step-functions ((iterator iterator))
  (values (lambda () (next iterator))
          (lambda () (has-more iterator))
          (lambda (value) (setf (current iterator) value))
          (lambda () (end iterator))))

(defclass list-iterator (iterator)
  ())

(defmethod initialize-instance :after ((iterator list-iterator) &key object)
  (setf (object iterator) (cons NIL object)))

(defmethod has-more ((iterator list-iterator))
  (cdr (object iterator)))

(defmethod next ((iterator list-iterator))
  (setf (object iterator) (cdr (object iterator)))
  (car (object iterator)))

(defmethod (setf current) (value (iterator list-iterator))
  (setf (car (object iterator)) value))

(defmethod step-functions ((iterator list-iterator))
  (let ((list (object iterator)))
    (declare (type list list))
    (values
     (lambda ()
       (setf list (cdr list))
       (car list))
     (lambda ()
       (cdr list))
     (lambda (value)
       (setf (car list) value))
     (lambda ()))))

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

(defmethod (setf current) (value (iterator vector-iterator))
  (setf (aref (object iterator) (1- (start iterator))) value))

(defmethod step-functions ((iterator vector-iterator))
  (let ((vec (object iterator))
        (i (start iterator)))
    (declare (type vector vec))
    (declare (type (unsigned-byte 32) i))
    (values
     (lambda ()
       (prog1 (aref vec i)
         (incf i)))
     (lambda ()
       (< i (length vec)))
     (lambda (value)
       (setf (aref vec i) value))
     (lambda ()))))

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

(defmethod (setf current) (value (iterator array-iterator))
  (setf (row-major-aref (object iterator) (1- (start iterator))) value))

(defmethod step-functions ((iterator array-iterator))
  (let ((arr (object iterator))
        (total (total-length iterator))
        (i (start iterator)))
    (declare (type vector arr))
    (declare (type (unsigned-byte 32) total i))
    (values
     (lambda ()
       (prog1 (row-major-aref arr i)
         (incf i)))
     (lambda ()
       (< i total))
     (lambda (value)
       (setf (row-major-aref arr i) value))
     (lambda ()))))

(defmethod make-iterator ((array array) &key (start 0))
  (make-instance 'array-iterator :object array :start start))

(defclass stream-iterator (iterator)
  ((buffer :accessor buffer)
   (index :initform 1 :accessor index)
   (limit :initform 1 :accessor limit)
   (close-stream :initarg :close-stream :accessor close-stream)))

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

(defmethod (setf current) ((value integer) (iterator stream-iterator))
  (write-byte value (object iterator)))

(defmethod (setf current) ((value character) (iterator stream-iterator))
  (write-char value (object iterator)))

(defmethod (setf current) ((value sequence) (iterator stream-iterator))
  (write-sequence value (object iterator)))

(defmethod end ((iterator stream-iterator))
  (when (close-stream iterator)
    (close (object iterator))))

(defmethod step-functions ((iterator stream-iterator))
  (let ((object (object iterator))
        (buffer (buffer iterator))
        (index (index iterator))
        (limit (limit iterator)))
    (declare (type stream object))
    (values
     (lambda ()
       (prog1 (aref buffer index)
         (incf index)))
     (lambda ()
       (when (= index limit)
         (setf limit (read-sequence buffer object))
         (setf index 0))
       (not (= 0 limit)))
     (lambda (value)
       (etypecase value
         (integer (write-byte value object))
         (character (write-char value object))
         (sequence (write-sequence value object))))
     (if (close-stream iterator)
         (lambda ()
           (close object))
         (lambda ())))))

(defclass stream-line-iterator (iterator)
  ((buffer :initform NIL :accessor buffer)
   (close-stream :initarg :close-stream :accessor close-stream)))

(defmethod has-more ((iterator stream-line-iterator))
  (or (buffer iterator)
      (setf (buffer iterator) (read-line (object iterator) NIL NIL))))

(defmethod next ((iterator stream-line-iterator))
  (let ((buffer (buffer iterator)))
    (cond (buffer
           (setf (buffer iterator) NIL)
           buffer)
          (T
           (read-line (object iterator))))))

(defmethod end ((iterator stream-line-iterator))
  (when (close-stream iterator)
    (close (object iterator))))

(defmethod step-functions ((iterator stream-line-iterator))
  (let ((object (object iterator))
        (buffer (buffer iterator)))
    (values
     (lambda ()
       (cond (buffer
              (setf buffer NIL)
              buffer)
             (T
              (read-line object))))
     (lambda ()
       (or buffer
           (setf buffer (read-line object NIL NIL))))
     (lambda (value)
       (declare (ignore value))
       (error "Cannot write to a STREAM-LINE-ITERATOR."))
     (if (close-stream iterator)
         (lambda ()
           (close object))
         (lambda ())))))

(defmethod make-iterator ((stream stream) &key (buffer-size 4096) close-stream)
  (case buffer-size
    ((:line :lines)
     (make-instance 'stream-line-iterator :object stream :close-stream close-stream))
    (T (make-instance 'stream-iterator :object stream :buffer-size buffer-size :close-stream close-stream))))

(defclass directory-iterator (list-iterator)
  ())

(defmethod initialize-instance :after ((iterator directory-iterator) &key object)
  (setf (object iterator) (cons NIL (directory object))))

(defmethod make-iterator ((pathname pathname) &key buffer-size (element-type 'character))
  (if (wild-pathname-p pathname)
      (make-instance 'directory-iterator :object pathname)
      (make-iterator (open pathname :element-type element-type) :buffer-size buffer-size :close-stream T)))

(defclass random-iterator (iterator)
  ((limit :initarg :limit :reader limit))
  (:default-initargs
   :limit 1.0f0))

(defmethod has-more ((iterator random-iterator))
  T)

(defmethod next ((iterator random-iterator))
  (random (limit iterator) (object iterator)))

(defmethod step-functions ((iterator random-iterator))
  (let ((object (object iterator))
        (limit (limit iterator)))
    (values
     (lambda ()
       (random limit object))
     (lambda ()
       T)
     (lambda (value)
       (declare (ignore value))
       (error "Cannot write to a RANDOM-ITERATOR."))
     (lambda ()))))

(defmethod make-iterator ((random-state random-state) &key (limit 1.0))
  (make-instance 'random-iterator :object random-state :limit limit))

(defclass package-iterator (iterator)
  ((prefetch :initform NIL :accessor prefetch))
  (:default-initargs
   :status '(:internal :external :inherited)))

(defmethod initialize-instance :after ((iterator package-iterator) &key object status)
  (setf (object iterator) (org.shirakumo.for::package-iterator object status)))

(defmethod has-more ((iterator package-iterator))
  (if (prefetch iterator)
      (car (prefetch iterator))
      (multiple-value-bind (more symb) (funcall (object iterator))
        (setf (prefetch iterator) (cons more symb))
        more)))

(defmethod next ((iterator package-iterator))
  (if (prefetch iterator)
      (prog1 (cdr (prefetch iterator))
        (setf (prefetch iterator) NIL))
      (nth-value 1 (funcall (object iterator)))))

(defmethod step-functions ((iterator package-iterator))
  (let ((object (object iterator))
        (prefetch (prefetch iterator)))
    (values
     (lambda ()
       (if prefetch
           (prog1 (cdr prefetch)
             (setf prefetch NIL))
           (nth-value 1 (funcall object))))
     (lambda ()
       (if prefetch
           (car prefetch)
           (multiple-value-bind (more symb) (funcall object)
             (setf prefetch (cons more symb))
             more)))
     (lambda (value)
       (declare (ignore value))
       (error "Cannot write to a PACKAGE-ITERATOR."))
     (lambda ()))))

(defmethod make-iterator ((package package) &key)
  (make-instance 'package-iterator :object package))

(defclass hash-table-iterator (iterator)
  ((iterator :initform NIL :accessor iterator)
   (prefetch :initform NIL :accessor prefetch)))

(defmethod initialize-instance :after ((iterator hash-table-iterator) &key object)
  (setf (iterator iterator) (org.shirakumo.for::hash-table-iterator object)))

(defmethod has-more ((iterator hash-table-iterator))
  (if (prefetch iterator)
      (car (prefetch iterator))
      (multiple-value-bind (more key val) (funcall (iterator iterator))
        (setf (prefetch iterator) (list more key val))
        more)))

(defmethod next ((iterator hash-table-iterator))
  (if (prefetch iterator)
      (prog1 (rest (prefetch iterator))
        (setf (prefetch iterator) NIL))
      (multiple-value-bind (more key val) (funcall (iterator iterator))
        (declare (ignore more))
        (list key val))))

(defmethod (setf current) (value (iterator hash-table-iterator))
  (setf (gethash (first (current iterator)) (object iterator)) value))

(defmethod step-functions ((iterator hash-table-iterator))
  (let ((object (object iterator))
        (prefetch (prefetch iterator))
        (iterator (iterator iterator))
        current)
    (declare (type hash-table object))
    (declare (type function iterator))
    (values
     (lambda ()
       (if prefetch
           (prog1 (setf current (rest prefetch))
             (setf prefetch NIL))
           (multiple-value-bind (more key val) (funcall iterator)
             (declare (ignore more))
             (setf current (list key val)))))
     (lambda ()
       (if prefetch
           (car prefetch)
           (multiple-value-bind (more key val) (funcall iterator)
             (setf prefetch (list more key val))
             more)))
     (lambda (value)
       (setf (gethash (first current) object) value))
     (lambda ()))))

(defmethod make-iterator ((hash-table hash-table) &key)
  (make-instance 'hash-table-iterator :object hash-table))
