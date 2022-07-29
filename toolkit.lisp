#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(defmacro unwind-protect* (cleanup &body body)
  `(unwind-protect (progn ,@body)
     ,cleanup))

(defmacro values* (&rest values)
  `(values-list
    (list*
     ,@(butlast values)
     (multiple-value-list
      ,(car (last values))))))

(defun enlist (a &rest els)
  (if (listp a)
      a
      (list* a els)))

(defun delist (a &optional (key #'first))
  (if (listp a)
      (funcall key a)
      a))

(defmacro with-interleaving (&body body)
  (let* ((root (copy-list (pop body)))
         (current root))
    (dolist (form body root)
      (let ((form (copy-list form)))
        (when form
          (setf (cdr (last current)) (cons form NIL))
          (setf current form))))))

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
     (let ((result (gensym "RESULT"))
           (var (gensym "VAR")))
       (lambda-fiddle:with-destructured-lambda-list (:required req :optional opt :rest rest :body body :key key) place
         (let ((rest (or rest body)))
           `(let ((,result ,value-form))
              ;; Convert required into test and POP
              ,@(loop for var in req
                      do (assert (and (not (null var)) (symbolp var)))
                      collect `(if ,result
                                   (setf ,var (pop ,result))
                                   (error "Not enough arguments to satisfy lambda-list ~a." ',place)))
              ;; Convert &OPTIONAL into POP and default.
              ,@(loop for o in opt
                      for (var default) = (enlist o)
                      do (assert (and (not (null var)) (symbolp var)))
                      collect `(setf ,var (or (pop ,result) ,default)))
              ;; If we don't have &REST or &KEY, error if there are some elements left.
              ,@(when (and (not rest) (not (find '&key place)))
                  `((when ,result (error "Too many elements supplied to satisfy lambda-list ~a." ',place))))
              ;; Convert &REST by just setting it to the remainder.
              ,@(when rest `((setf ,rest ,result)))
              ;; Convert &KEY into GETF and default.
              ,@(loop for k in key
                      for (var default) = (enlist k)
                      do (assert (and (not (null var)) (symbolp var)))
                      collect `(setf ,var (or (getf ,result ,(intern (string var) :keyword)) ,default)))
              ;; If we have a non-empty &KEY list and don't have &ALLOW-OTHER-KEYS, check the keys.
              ,@(when (and key (not (find '&allow-other-keys place)))
                  `((loop for ,var in ,result by #'cddr
                          do (unless (find ,var ',(loop for k in key collect (intern (string k) :keyword)))
                               (error "Keyword argument ~s not found in lambda-list ~a."
                                      ,var ',place))))))))))))

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

(defun collect-for-values (expressions function)
  (loop for expr in expressions
        for values = (funcall function expr)
        when (first values) collect (first values) into all-init
        when (second values) collect (second values) into all-forms
        when (third values) collect (third values) into all-exit
        when (fourth values) collect (fourth values) into all-post
        finally (return (values all-init all-forms all-exit all-post))))
