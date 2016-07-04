#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

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

(define-simple-binding in (var list &key by)
  `(cond (,list
          ,(if by
               `(progn (update ,var (car ,list))
                       (setf ,list (funcall ,by ,list)))
               `(setf ,var (pop ,list))))
         (T
          (end-for))))

(define-simple-binding across (var vector &aux (i -1) (length (length vector)))
  `(if (= ,length (incf ,i))
       (end-for)
       (update ,var (aref ,vector ,i))))

(define-simple-binding over (var iterable &rest iterator-args &aux (iterator (apply #'make-iterator iterable iterator-args)))
  `(if (has-more ,iterator)
       (update ,var (next ,iterator))
       (end-for)))

(defun hash-table-iterator (table)
  (with-hash-table-iterator (it table)
    (lambda () (it))))

(define-simple-binding table-keys (var table &aux (iterator (hash-table-iterator table)) next key val)
  `(multiple-value-bind (,next ,key ,val) (funcall ,iterator)
     (declare (ignore ,val))
     (if ,next
         (update ,var ,key)
         (end-for))))

(define-simple-binding table-values (var table &aux (iterator (hash-table-iterator table)) next key val)
  `(multiple-value-bind (,next ,key ,val) (funcall ,iterator)
     (declare (ignore ,key))
     (if ,next
         (update ,var ,val)
         (end-for))))

(define-simple-binding table-pairs (var table &aux (iterator (hash-table-iterator table)) next key val)
  `(multiple-value-bind (,next ,key ,val) (funcall ,iterator)
     (cond (,next
            (update ,(first var) ,key)
            (update ,(second var) ,val))
           (T
            (end-for)))))

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

(define-simple-binding symbols (var package &rest status &aux (iterator (package-iterator package (or status '(:internal :external :inherited)))) next symbol)
  `(multiple-value-bind (,next ,symbol) (funcall ,iterator)
     (if ,next
         (setf ,var ,symbol)
         (end-for))))

(define-simple-binding between ((var (if ascending (- from step) (+ from step))) from to &key (by 1) &aux (ascending (< from to)))
  (declare (ignore from))
  `(cond (,ascending
          (when (<= ,to ,var)
            (end-for))
          (incf ,var ,by))
         (T
          (when (<= ,var ,to)
            (end-for))
          (decf ,var ,by))))

(define-simple-binding from ((var (- from step)) from &key (by 1))
  `(incf ,var ,by))

(define-simple-binding repeating ((var 0) limit)
  `(when (< ,limit (incf ,var))
     (end-for)))

(define-binding collecting (var form &aux (head (cons NIL NIL)) (tail head))
  `(setf ,tail (setf (cdr ,tail) (cons ,form NIL))
         ,var (cdr ,head)))

(defun copy-list* (list)
  (let ((head (cons NIL NIL))
        (tail head))
    (dolist (el list (values (cdr head) tail))
      (setf tail (setf (cdr tail) (cons el NIL))))))

(define-binding appending (var form &aux (head (cons NIL NIL)) (tail head))
  (let ((result (gensym "RESULT"))
        (new-tail (gensym "NEW-TAIL")))
    `(let ((,result ,form))
       (when ,result
         (multiple-value-bind (,head ,new-tail) (copy-list* ,result)
           (setf (cdr ,tail) ,head
                 ,tail ,new-tail))
         (setf ,var (cdr ,head))))))

(define-binding nconcing (var form &aux (head (cons NIL NIL)) (tail head))
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (when ,result
         (setf (cdr ,tail) ,result
               ,tail (last ,result))
         (setf ,var (cdr ,head))))))

(define-binding counting ((var 0) form)
  `(when ,form (incf ,var)))

(define-binding summing ((var 0) form)
  `(incf ,var ,form))

(define-binding maximizing (var form)
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (when (or (not ,var) (< ,var ,result))
         (setf ,var ,result)))))

(define-binding minimizing (var form)
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (when (or (not ,var) (< ,result ,var))
         (setf ,var ,result)))))

(define-clause always (form)
  `(unless ,form (return-for NIL)))

(define-clause never (form)
  `(when ,form (return-for NIL)))

(define-clause thereis (form)
  `(when ,form (return-for T)))

(define-clause while (form)
  `(unless ,form (end-for)))

(define-clause until (form)
  `(when ,form (end-for)))
