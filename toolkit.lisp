#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(defun enlist (a &rest els)
  (if (listp a)
      a
      (list* a els)))

(defun delist (a &optional (key #'first))
  (if (listp a)
      (funcall key a)
      a))

(defmacro with-interleaving (&body body)
  (let ((sentinel (cons NIL NIL)))
    (loop for next = sentinel then copy
          for form in body
          for copy = (copy-list form)
          do (setf (cdr (last next)) (cons copy NIL)))
    (cadr sentinel)))

(defun copy-list* (list)
  (let* ((head (cons NIL NIL))
         (tail head))
    (dolist (el list (values (cdr head) tail))
      (setf tail (setf (cdr tail) (cons el NIL))))))

(defun replace-lambda-vars (lambda-list vars new-vars)
  (flet ((replacement (var)
           (if (find var vars)
               (elt new-vars (position var vars))
               var)))
    (loop for item in lambda-list
          collect (cond ((listp item)
                         (mapcar #'replacement item))
                        (T (replacement item))))))

(defmacro update (place value-form)
  (etypecase place
    (symbol `(setf ,place ,value-form))
    (list
     (let* ((vars (lambda-fiddle:extract-all-lambda-vars place))
            (gens (loop for var in vars collect (gensym (string var))))
            (replaced (replace-lambda-vars place vars gens)))
       `(destructuring-bind ,replaced ,value-form
          ,@(loop for var in vars
                  for gen in gens
                  collect `(setf ,var ,gen)))))))

(defun hash-table-iterator (table)
  (with-hash-table-iterator (it table)
    (lambda () (it))))

(defun package-iterator (package statuses)
  (let ((statuses (enlist statuses)))
    (macrolet ((emit-it (&rest statuses)
                 `(when (and ,@(loop for status in statuses collect `(find ,status statuses)))
                    (with-package-iterator (it package ,@statuses)
                      (lambda () (it))))))
      (or (emit-it :internal :external :inherited)
          (emit-it :internal :external)
          (emit-it :internal :inherited)
          (emit-it :external :inherited)
          (emit-it :internal)
          (emit-it :external)
          (emit-it :inherited)
          (error "At least one of :INTERNAL :EXTERNAL :INHERITED required for package iteration.")))))
