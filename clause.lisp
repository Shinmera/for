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

(defmacro define-direct-clause (name args &body body)
  `(progn (setf (clause ',name)
                (lambda ,args
                  ,@body))
          ',name))

(defmacro define-simple-clause (name args &body body)
  `(define-direct-clause ,name ,args
     (values* NIL (progn ,@body))))

(defun convert-clauses (forms)
  (collect-for-values
   forms
   (lambda (form)
     (let ((clause (when (consp form) (ignore-errors (clause (first form))))))
       (if clause
           (multiple-value-list (apply clause (rest form)))
           (list NIL form))))))
