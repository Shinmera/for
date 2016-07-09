#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(define-value-binding with (var value)
  (declare (ignorable value)))

(define-value-binding in (var list &key (by NIL by-p))
  `(cond (,list
          ,(if by-p
               `(progn (update ,var (car ,list))
                       (setf ,list (funcall ,by ,list)))
               `(update ,var (pop ,list))))
         (T
          (end-for))))

(define-value-binding on (var list &key (by NIL by-p))
  `(cond (,list
          ,(if by-p
               `(progn (update ,var ,list)
                       (setf ,list (funcall ,by ,list)))
               `(progn (update ,var ,list)
                       (setf ,list (cdr ,list)))))
         (T
          (end-for))))

(define-value-binding across (var vector &aux (i -1) (length (length vector)))
  `(if (= ,length (incf ,i))
       (end-for)
       (update ,var (aref ,vector ,i))))

(define-value-symbol-macro-binding over ((var (current iterator)) iterable &rest iterator-args &aux (iterator (apply #'make-iterator iterable iterator-args)))
  `(if (has-more ,iterator)
       (next ,iterator)
       (end-for)))

(define-value-binding table-keys (var table &aux (iterator (hash-table-iterator table)) next key val)
  `(multiple-value-bind (,next ,key ,val) (funcall ,iterator)
     (declare (ignore ,val))
     (if ,next
         (update ,var ,key)
         (end-for))))

(define-value-binding table-values (var table &aux (iterator (hash-table-iterator table)) next key val)
  `(multiple-value-bind (,next ,key ,val) (funcall ,iterator)
     (declare (ignore ,key))
     (if ,next
         (update ,var ,val)
         (end-for))))

(define-value-binding table-pairs (var table &aux (iterator (hash-table-iterator table)) next key val)
  `(multiple-value-bind (,next ,key ,val) (funcall ,iterator)
     (cond (,next
            (update ,(first var) ,key)
            (update ,(second var) ,val))
           (T
            (end-for)))))

(define-value-binding symbols (var package &rest status &aux (iterator (package-iterator package (or status '(:internal :external :inherited)))) next symbol)
  `(multiple-value-bind (,next ,symbol) (funcall ,iterator)
     (if ,next
         (setf ,var ,symbol)
         (end-for))))

(define-value-binding between ((var (if ascending (- from by) (+ from by))) from to &key (by 1) &aux (ascending (< from to)))
  (declare (ignore from))
  `(cond (,ascending
          (when (<= ,to ,var)
            (end-for))
          (incf ,var ,by))
         (T
          (when (<= ,var ,to)
            (end-for))
          (decf ,var ,by))))

(define-value-binding from ((var (- from by)) from &key (by 1))
  `(incf ,var ,by))

(define-value-binding repeating ((var 0) limit)
  `(when (< ,limit (incf ,var))
     (end-for)))

(define-form-binding = (var form)
  `(update ,var ,form))

(define-accumulation-binding collecting (var form &aux (head (cons NIL NIL)) (tail head))
  `(setf ,tail (setf (cdr ,tail) (cons ,form NIL))
         ,var (cdr ,head)))

(define-accumulation-binding appending (var form &aux (head (cons NIL NIL)) (tail head))
  (let ((result (gensym "RESULT"))
        (new-tail (gensym "NEW-TAIL")))
    `(let ((,result ,form))
       (when ,result
         (multiple-value-bind (,head ,new-tail) (copy-list* ,result)
           (setf (cdr ,tail) ,head
                 ,tail ,new-tail))
         (setf ,var (cdr ,head))))))

(define-accumulation-binding nconcing (var form &aux (head (cons NIL NIL)) (tail head))
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (when ,result
         (setf (cdr ,tail) ,result
               ,tail (last ,result))
         (setf ,var (cdr ,head))))))

(defvar *unbound* (gensym "UNBOUND"))

(define-accumulation-binding reducing ((var *unbound*) form &key by)
  `(cond ((eq ,var *unbound*)
          (setf ,var ,form))
         (T
          (setf ,var (funcall ,by ,var ,form)))))

(define-accumulation-binding counting ((var 0) form)
  `(when ,form (incf ,var)))

(define-accumulation-binding summing ((var 0) form)
  `(incf ,var ,form))

(define-accumulation-binding maximizing (var form)
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (when (or (not ,var) (< ,var ,result))
         (setf ,var ,result)))))

(define-accumulation-binding minimizing (var form)
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (when (or (not ,var) (< ,result ,var))
         (setf ,var ,result)))))

(define-simple-clause always (form &aux (result T))
  (values `(unless ,form (setf ,result NIL) (end-for))
          result))

(define-simple-clause never (form &aux (result T))
  (values `(when ,form (setf ,result NIL) (end-for))
          result))

(define-simple-clause thereis (form &aux (result NIL))
  (values `(when ,form (setf ,result T) (end-for))
          result))

(define-simple-clause while (form)
  `(unless ,form (end-for)))

(define-simple-clause until (form)
  `(when ,form (end-for)))

(define-simple-clause returning (form)
  (values NIL
          form))

(define-alias-binding repeat repeating)
(define-alias-binding collect collecting)
(define-alias-binding append appending)
(define-alias-binding nconc nconcing)
(define-alias-binding reduce reducing)
(define-alias-binding count counting)
(define-alias-binding sum summing)
(define-alias-binding maximize maximizing)
(define-alias-binding minimize minimizing)
