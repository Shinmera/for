#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(defvar *bindings* (make-hash-table :test 'eql))

(defun binding (name)
  (let ((result (or (gethash name *bindings*)
                    (gethash (find-symbol (string name) #.*package*) *bindings*))))
    (if (and result (symbolp result)) (binding result) result)))

(defun (setf binding) (func name)
  (setf (gethash name *bindings*) func))

(defun remove-binding (name)
  (binding name)
  (remhash name *bindings*))

(defmethod documentation ((name symbol) (type (eql 'binding)))
  (when name
    (documentation (binding name) type)))

(defmethod documentation ((func function) (type (eql 'binding)))
  (documentation func T))

(defmethod (setf documentation) (docstring (name symbol) (type (eql 'binding)))
  (when name
    (setf (documentation (binding name) type) docstring)))

(defmethod (setf documentation) (docstring (func function) (type (eql 'binding)))
  (setf (documentation func T) docstring))

(defmacro define-alias-binding (name referenced-binding-name)
  `(progn (setf (binding ',name) ',referenced-binding-name)
          ',name))

(defmacro define-direct-binding (name args &body body)
  (let ((env (or (lambda-fiddle:environment-lambda-var args) (gensym "ENVIRONMENT"))))
    `(progn (setf (binding ',name)
                  (lambda ,(lambda-fiddle:remove-environment-part args)
                    (let ((,env *environment*))
                      (declare (ignorable ,env))
                      ,@body)))
            ',name)))

(defun translate-form-vars (form from to)
  ;; FIXME: This needs a code walker...
  (let ((from (mapcar #'delist from))
        (to (mapcar #'delist to)))
    (typecase form
      (null form)
      (symbol (if (find form from) (elt to (position form from)) (list 'quote form)))
      (list (case (first form)
              (quote form)
              (T (list*
                  'list
                  (list 'quote (first form))
                  (loop for item in (rest form)
                        collect (translate-form-vars item from to))))))
      (T form))))

(defun bindings-for-gens (aux gens)
  (loop for var in aux for gen in gens
        collect (list (delist var) gen)))

(defun compute-binding-parts (var vars args)
  (let* ((var (enlist var))
         (vargen (gensym "VAR"))
         (vars-gens (loop for var in vars collect `(gensym ,(string (delist var)))))
         (vars-gen-gens (loop for var in vars collect (gensym (string (delist var)))))
         (aux (lambda-fiddle:collect-for-keyword '&aux args))
         (aux-gens (loop for var in aux collect `(gensym ,(string (delist var)))))
         (aux-gen-gens (loop for var in aux collect (gensym (string (delist var)))))
         (all (append vars aux))
         (all-gen-gens (append vars-gen-gens aux-gen-gens)))
    (values (list* (first var) (lambda-fiddle:remove-aux-part args))
            (append (mapcar #'list aux-gen-gens aux-gens)
                    (mapcar #'list vars-gen-gens vars-gens))
            (append (bindings-for-gens aux aux-gen-gens)
                    (bindings-for-gens vars vars-gen-gens))
            (append (loop for var in vars for gen in vars-gen-gens
                          collect `(list ,gen ,(if (eql var (lambda-fiddle:rest-lambda-var args))
                                                   `(list* 'list ,var)
                                                   var)))
                    (loop for var in aux for gen in aux-gen-gens
                          collect `(list ,gen ,(translate-form-vars (delist var #'second) all all-gen-gens))))
            `(loop for ,vargen in (lambda-fiddle:extract-lambda-vars (enlist ,(first var)))
                   collect (list ,vargen ,(translate-form-vars (second var) all all-gen-gens))))))

(defmacro define-form-binding (name (var &rest args) &body body)
  (multiple-value-bind (args outer-let inner-let result-let varform) (compute-binding-parts var NIL args)
    `(define-direct-binding ,name ,args
       (let ,outer-let
         (values* `(let* ,(list*
                           ,@result-let
                           ,varform)
                     (declare (ignorable ,,@(mapcar #'delist outer-let))))
                  (let ,inner-let
                    (declare (ignorable ,@(mapcar #'delist inner-let)))
                    ,@body))))))

(defmacro define-accumulation-binding (name (var &rest args) &body body)
  `(define-form-binding ,name (,var ,@args)
     (values
      (progn ,@body)
      (delist var))))

(defmacro define-form-symbol-macro-binding (name (var &rest args) &body body)
  (multiple-value-bind (args outer-let inner-let result-let varform) (compute-binding-parts var NIL args)
    `(define-direct-binding ,name ,args
       (let ,outer-let
         (values* `(with-interleaving
                     (let* ,(list
                             ,@result-let)
                       (declare (ignorable ,,@(mapcar #'delist outer-let))))
                     (symbol-macrolet ,,varform))
                  (let ,inner-let
                    (declare (ignorable ,@(mapcar #'delist inner-let)))
                    ,@body))))))

(defmacro define-value-binding (name (var &rest args) &body body)
  (let ((vars (lambda-fiddle:extract-lambda-vars (lambda-fiddle:remove-aux-part args))))
    (multiple-value-bind (args outer-let inner-let result-let varform) (compute-binding-parts var vars args)
      `(define-direct-binding ,name ,args
         (let ,outer-let
           (values* `(let* ,(list*
                             ,@result-let
                             ,varform)
                       (declare (ignorable ,,@(mapcar #'delist outer-let))))
                    (let ,inner-let
                      (declare (ignorable ,@(mapcar #'delist inner-let)))
                      ,@body)))))))

(defmacro define-value-symbol-macro-binding (name (var &rest args) &body body)
  (let ((vars (lambda-fiddle:extract-lambda-vars (lambda-fiddle:remove-aux-part args))))
    (multiple-value-bind (args outer-let inner-let result-let varform) (compute-binding-parts var vars args)
      `(define-direct-binding ,name ,args
         (let ,outer-let
           (values* `(with-interleaving
                       (let* ,(list
                               ,@result-let)
                         (declare (ignorable ,,@(mapcar #'delist outer-let))))
                       (symbol-macrolet ,,varform))
                    (let ,inner-let
                      (declare (ignorable ,@(mapcar #'delist inner-let)))
                      ,@body)))))))

(defun convert-bindings (bindings)
  (collect-for-values
   bindings
   (lambda (binding)
     (destructuring-bind (var type &rest args) binding
       (let ((binding (or (binding type)
                          (error "A FOR binding with the name ~s is not known." type))))
         (multiple-value-list (apply binding var args)))))))
