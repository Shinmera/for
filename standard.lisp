#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(defun remove-bindings (bindings form)
  (cond ((and (listp form) (find (first form) '(let let*)))
         `(,(first form)
           ,(loop for binding in (second form)
                  unless (find (first binding) bindings :key #'car)
                  collect binding)
           ,@(cddr form)))
        (T form)))

(define-direct-binding being (var &rest sub-bindings)
  (let ((bindings (loop for bind in sub-bindings collect (list* var bind)))
        (being-counter (gensym (string 'being-counter)))
        (being-tag (gensym (string 'being-tag)))
        (vars (loop for var in (lambda-fiddle:extract-lambda-vars (enlist var))
                    collect `(,var NIL))))
    (multiple-value-bind (bind-init bind-forms) (convert-bindings bindings)
      (values `(with-interleaving
                 (let ((,being-counter 0)
                       ,@vars))
                 ,@(loop for bind in bind-init collect (remove-bindings vars bind)))
              `(tagbody
                  ,being-tag
                  (case ,being-counter
                    ,@(loop for form in (butlast bind-forms)
                            for i from 0
                            collect `(,i (macrolet ((end-for ()
                                                      `(progn (incf ,',being-counter)
                                                              (go ,',being-tag))))
                                           ,form)))
                    (T ,@(last bind-forms))))))))

(define-direct-binding when (var test sub-binding &rest args)
  (multiple-value-bind (init body value) (apply (binding sub-binding) var args)
    (values init
            `(when ,test ,body)
            value)))

(define-direct-binding unless (var test sub-binding &rest args)
  (multiple-value-bind (init body value) (apply (binding sub-binding) var args)
    (values init
            `(unless ,test ,body)
            value)))

(define-value-binding as ((var value) value)
  (declare (ignorable value))
  (values NIL
          var))

(define-value-binding in (var list &key (by NIL by-p))
  (declare (type list list))
  `(cond (,list
          ,(if by-p
               `(progn (update ,var (car ,list))
                       (setf ,list (funcall ,by ,list)))
               `(update ,var (pop ,list))))
         (T
          (end-for))))

(define-value-binding on (var list &key (by NIL by-p))
  (declare (type list list))
  `(cond (,list
          ,(if by-p
               `(progn (update ,var ,list)
                       (setf ,list (funcall ,by ,list)))
               `(progn (update ,var ,list)
                       (setf ,list (cdr ,list)))))
         (T
          (end-for))))

(define-value-binding across (var vector &aux (i 0) (length (length vector)))
  (declare (type vector vector)
           (type (integer 0) i length))
  `(cond ((= ,length ,i)
          (end-for))
         (T (update ,var (aref ,vector ,i))
            (incf ,i))))

(define-direct-binding over (var iterable &rest iterator-args)
  (let ((iterator (gensym "ITERATOR"))
        (next (gensym "NEXT"))
        (has-more (gensym "HAS-MORE"))
        (update (gensym "UPDATE"))
        (end (gensym "END")))
    (values
     `(with-interleaving
        (let (,@(lambda-fiddle:extract-lambda-vars (enlist var))
              (,iterator (make-iterator ,iterable ,@iterator-args))))
        (multiple-value-bind (,next ,has-more ,update ,end) (step-functions ,iterator)
          (declare (ignore ,update)))
        (unwind-protect* (funcall ,end)))
     `(if (funcall ,has-more)
          (update ,var (funcall ,next))
          (end-for)))))

(define-value-symbol-macro-binding updating ((var (current iterator)) iterable &rest iterator-args &aux (iterator (apply #'make-iterator iterable iterator-args)))
  `(if (has-more ,iterator)
       (next ,iterator)
       (end-for)))

(define-value-binding table-keys (var table &aux (iterator (hash-table-iterator table)) next key val)
  (declare (type hash-table table)
           (type function iterator))
  `(multiple-value-bind (,next ,key ,val) (funcall ,iterator)
     (declare (ignore ,val))
     (if ,next
         (update ,var ,key)
         (end-for))))

(define-value-binding table-values (var table &aux (iterator (hash-table-iterator table)) next key val)
  (declare (type hash-table table)
           (type function iterator))
  `(multiple-value-bind (,next ,key ,val) (funcall ,iterator)
     (declare (ignore ,key))
     (if ,next
         (update ,var ,val)
         (end-for))))

(define-value-binding table-pairs (var table &aux (iterator (hash-table-iterator table)) next key val)
  (declare (type hash-table table)
           (type function iterator))
  `(multiple-value-bind (,next ,key ,val) (funcall ,iterator)
     (cond (,next
            (update ,(first var) ,key)
            (update ,(second var) ,val))
           (T
            (end-for)))))

(define-value-binding symbols (var package &rest status &aux (iterator (package-iterator package (or status '(:internal :external :inherited)))) next symbol)
  (declare (type function iterator))
  `(multiple-value-bind (,next ,symbol) (funcall ,iterator)
     (if ,next
         (setf ,var ,symbol)
         (end-for))))

(define-value-binding ranging ((var (if ascending (- from by) (+ from by))) from to &key (by 1) &aux (ascending (< from to)))
  (declare (type real var from to by)
           (type boolean ascending))
  (declare (ignore from))
  `(cond (,ascending
          (incf ,var ,by)
          (when (< ,to ,var)
            (end-for)))
         (T
          (decf ,var ,by)
          (when (< ,var ,to)
            (end-for)))))

(define-value-binding from ((var (if ascending (- from by) (+ from by))) from &key (to NIL to-p) (by 1) &aux (ascending (or (not to) (< from to))))
  (declare (type real var from by)
           (type (or real null) to)
           (type boolean ascending))
  (declare (ignore from))
  (if to-p
      `(cond (,ascending
              (incf ,var ,by)
              (when (<= ,to ,var)
                (end-for)))
             (T
              (decf ,var ,by)
              (when (<= ,var ,to)
                (end-for))))
      `(incf ,var ,by)))

(define-value-binding repeating ((var 0) limit)
  (declare (type (integer 0) var limit))
  `(when (< ,limit (incf ,var))
     (end-for)))

(define-direct-binding lines-of (var pathname/stream)
  (let ((arg (gensym "ARGUMENT"))
        (stream (gensym "STREAM"))
        (line (gensym "LINE")))
    (values
     `(with-interleaving
        (let* ((,var NIL)
               (,arg ,pathname/stream)
               (,stream (etypecase ,arg
                          ((or string pathname) (open ,arg))
                          (stream ,arg)))))
        (unwind-protect* (close ,stream)))
     `(let ((,line (read-line ,stream NIL NIL)))
        (if ,line (setf ,var ,line) (end-for))))))

(define-direct-binding = (var form &key then)
  (if then
      (let ((undef '#.(gensym "UNDEF")))
        (values
         `(let (,var ,undef))
         `(if (eq ,var ,undef)
              (update ,var ,form)
              (update ,var ,then))))
      (values
       `(let (,var))
       `(update ,var ,form))))

(define-accumulation-binding collecting (var form &aux (head (cons NIL NIL)) (tail head))
  (declare (type list var)
           (type cons head tail))
  `(setf ,tail (setf (cdr ,tail) (cons ,form NIL))
         ,var (cdr ,head)))

(define-accumulation-binding appending (var form &aux (head (cons NIL NIL)) (tail head))
  (declare (type list var)
           (type cons head tail))
  (let ((result (gensym "RESULT"))
        (new-tail (gensym "NEW-TAIL")))
    `(let ((,result ,form))
       (when ,result
         (multiple-value-bind (,head ,new-tail) (copy-list* ,result)
           (setf (cdr ,tail) ,head
                 ,tail ,new-tail))
         (setf ,var (cdr ,head))))))

(define-accumulation-binding nconcing (var form &aux (head (cons NIL NIL)) (tail head))
  (declare (type list var)
           (type cons head tail))
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (when ,result
         (setf (cdr ,tail) ,result
               ,tail (last ,result))
         (setf ,var (cdr ,head))))))

(defvar *unbound* (gensym "UNBOUND"))

(define-accumulation-binding reducing ((var *unbound*) form &key (by NIL by-p))
  (unless by-p (error ":BY argument required for REDUCING binding."))
  `(cond ((eq ,var *unbound*)
          (setf ,var ,form))
         (T
          (setf ,var (funcall ,by ,var ,form)))))

(define-accumulation-binding counting ((var 0) form)
  (declare (type (integer 0) var))
  `(when ,form (incf ,var)))

(define-accumulation-binding summing ((var 0) form)
  (declare (type real var))
  `(incf ,var ,form))

(define-accumulation-binding maximizing (var form)
  (declare (type (or null real) var))
  (let ((result (gensym "RESULT")))
    `(let ((,result ,form))
       (when (or (not ,var) (< ,var ,result))
         (setf ,var ,result)))))

(define-accumulation-binding minimizing (var form)
  (declare (type (or null real) var))
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
  (let ((res (gensym "RES")))
    (values `(let ((,res ,form))
               (when ,res (setf ,result ,res) (end-for)))
            result)))

(define-simple-clause while (form)
  `(unless ,form (end-for)))

(define-simple-clause until (form)
  `(when ,form (end-for)))

(define-simple-clause returning (form)
  (values NIL
          form))

(define-direct-clause repeat (n)
  (let ((limit (gensym "LIMIT"))
        (counter (gensym "COUNTER")))
    (values
     `(let ((,limit ,n)
            (,counter 0)))
     `(when (<= ,limit (incf ,counter))
        (end-for)))))

(define-alias-binding update updating)
(define-alias-binding range ranging)
(define-alias-binding repeat repeating)
(define-alias-binding collect collecting)
(define-alias-binding append appending)
(define-alias-binding nconc nconcing)
(define-alias-binding reduce reducing)
(define-alias-binding count counting)
(define-alias-binding sum summing)
(define-alias-binding maximize maximizing)
(define-alias-binding minimize minimizing)
