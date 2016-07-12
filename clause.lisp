#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(defvar *clauses* (make-hash-table :test 'eql))

(defun clause (name)
  (let ((result (or (gethash name *clauses*)
                    (gethash (find-symbol (string name) #.*package*) *clauses*))))
    (if (and result (symbolp result)) (clause result) result)))

(defun (setf clause) (func name)
  (setf (gethash name *clauses*) func))

(defun remove-clause (name)
  (clause name)
  (remhash name *clauses*))

(defmethod documentation ((name symbol) (type (eql 'clause)))
  (when name
    (documentation (clause name) type)))

(defmethod documentation ((func function) (type (eql 'clause)))
  (documentation func T))

(defmethod (setf documentation) (docstring (name symbol) (type (eql 'clause)))
  (when name
    (setf (documentation (clause name) type) docstring)))

(defmethod (setf documentation) (docstring (func function) (type (eql 'clause)))
  (setf (documentation func T) docstring))

(defmacro define-direct-clause (name args &body body)
  (let ((env (or (lambda-fiddle:environment-lambda-var args) (gensym "ENVIRONMENT"))))
    `(progn (setf (clause ',name)
                  (lambda ,(lambda-fiddle:remove-environment-part args)
                    (let ((,env *environment*))
                      (declare (ignorable ,env))
                      ,@body)))
            ',name)))

(defmacro define-simple-clause (name args &body body)
  (multiple-value-bind (args outer-let inner-let result-let) (compute-binding-parts NIL NIL args)
    `(define-direct-clause ,name ,(rest args)
       (let ,outer-let
         (values* `(let* ,(list
                           ,@result-let)
                     (declare (ignorable ,,@(mapcar #'delist outer-let))))
                  (let ,inner-let
                    (declare (ignorable ,@(mapcar #'delist inner-let)))
                    ,@body))))))

(defun convert-clauses (forms)
  (collect-for-values
   forms
   (lambda (form)
     (let ((clause (when (consp form) (clause (first form)))))
       (if clause
           (multiple-value-list (apply clause (rest form)))
           (list NIL form))))))
