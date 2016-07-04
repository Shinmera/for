#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(defvar *clauses* (make-hash-table :test 'eql))

(defun clause (name)
  (or (gethash name *clauses*)
      (error "A FOR clause with the name ~s is not known." name)))

(defun (setf clause) (func name)
  (setf (gethash name *clauses*) func))

(defun remove-clause (name)
  (clause name)
  (remhash name *clauses*))

(defmacro define-clause (name args &body body)
  `(progn (setf (clause ',name)
                '(,name ,args ,@body))
          ',name))

(defun clause-forms ()
  (loop for form being the hash-values of *clauses*
        collect form))

(defmacro with-clauses (() &body body)
  `(macrolet ,(clause-forms)
     ,@body))
