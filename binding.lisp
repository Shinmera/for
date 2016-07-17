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

(defun normalize-declaration-inner (inner)
  (case (first inner)
    ;; These are the ones we know and can split
    ((dynamic-extent ignorable ignore inline notinline optimize special)
     (loop for arg in (cdr inner)
           collect (list (first inner) arg)))
    ;; These have an arg
    ((ftype type)
     (loop for arg in (cddr inner)
           collect (list (first inner) (second inner) arg)))
    ;; Everything else is just verbatim
    (T (list inner))))

(defun normalize-declarations (body)
  (let* ((exprs (form-fiddle:lambda-declarations `(lambda () ,@body)))
         (decls (loop for expr in exprs append (cdr expr)))
         (norms (loop for decl in decls
                      append (normalize-declaration-inner decl))))
    (loop for norm in norms
          collect `(declare ,norm))))

(defun compute-declaration-parts (body inner-let)
  (flet ((outer-decl-p (decl)
           (find (first (second decl)) '(dynamic-extent ftype inline notinline optimize special type)))
         (var-for-var (var)
           (or (second (assoc var inner-let)) var)))
    (let* ((decls (normalize-declarations body))
           (outer (remove-if-not #'outer-decl-p decls))
           (inner (remove-if #'outer-decl-p decls))
           (translated-outer
             (loop for decl in outer
                   for (type arg1 arg2) = (second decl)
                   collect (case type
                             (type
                              ``(declare (type ,',arg1 ,,(var-for-var arg2))))
                             (dynamic-extent
                              ``(declare (dynamic-extent ,,(var-for-var arg1))))
                             (special
                              ``(declare (special ,,(var-for-var arg1))))
                             (T
                              ``decl)))))
      (values (form-fiddle:lambda-forms `(lambda () ,@body))
              translated-outer
              inner))))

(defun emit-binding-definition (name var args body &key extra-vars symbol-macro-p)
  (multiple-value-bind (args outer-let inner-let result-let varform) (compute-binding-parts var extra-vars args)
    (multiple-value-bind (body outer-decls inner-decls) (compute-declaration-parts body inner-let)
      `(define-direct-binding ,name ,args
         (let ,outer-let
           (values* `(with-interleaving
                       (let* ,(list*
                               ,@result-let
                               ,(unless symbol-macro-p varform))
                         (declare (ignorable ,,@(mapcar #'delist outer-let)))
                         ,,@outer-decls)
                       ,,(when symbol-macro-p
                           ``(symbol-macrolet ,,varform)))
                    (let ,inner-let
                      (declare (ignorable ,@(mapcar #'delist inner-let)))
                      ,@inner-decls
                      ,@body)))))))

(defmacro define-form-binding (name (var &rest args) &body body)
  (emit-binding-definition name var args body))

(defmacro define-accumulation-binding (name (var &rest args) &body body)
  `(define-form-binding ,name (,var ,@args)
     ,@(form-fiddle:lambda-declarations `(lambda () ,@body))
     (values
      (progn ,@(form-fiddle:lambda-forms `(lambda () ,@body)))
      (delist var))))

(defmacro define-form-symbol-macro-binding (name (var &rest args) &body body)
  (emit-binding-definition name var args body :symbol-macro-p T))

(defmacro define-value-binding (name (var &rest args) &body body)
  (let ((vars (lambda-fiddle:extract-lambda-vars (lambda-fiddle:remove-aux-part args))))
    (emit-binding-definition name var args body :extra-vars vars)))

(defmacro define-value-symbol-macro-binding (name (var &rest args) &body body)
  (let ((vars (lambda-fiddle:extract-lambda-vars (lambda-fiddle:remove-aux-part args))))
    (emit-binding-definition name var args body :extra-vars vars :symbol-macro-p T)))

(defun convert-bindings (bindings)
  (collect-for-values
   bindings
   (lambda (binding)
     (destructuring-bind (var type &rest args) binding
       (let ((binding (or (binding type)
                          (error "A FOR binding with the name ~s is not known." type))))
         (multiple-value-list (apply binding var args)))))))
