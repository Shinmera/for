(ql:quickload '(staple for))
(in-package #:cl-user)

(defclass symb-binding (staple:symb-function)
  ())

(defmethod staple:symb-function ((symb symb-binding))
  (for:binding (staple:symb-symbol symb)))

(defmethod staple:symb-documentation ((symb symb-binding))
  (documentation (staple:symb-symbol symb) 'for:binding))

(defmethod staple:symb-type-order ((symb (eql 'symb-binding)))
  (1+ (staple:symb-type-order 'staple:symb-function)))

(defclass symb-clause (staple:symb-function)
  ())

(defmethod staple:symb-function ((symb symb-clause))
  (for:clause (staple:symb-symbol symb)))

(defmethod staple:symb-documentation ((symb symb-clause))
  (documentation (staple:symb-symbol symb) 'for:clause))

(defmethod staple:symb-type-order ((symb (eql 'symb-clause)))
  (1+ (staple:symb-type-order 'symb-binding)))

(staple:define-simple-converter symb-binding for:binding)
(staple:define-simple-converter symb-clause for:clause)

(defun staple ()
  (staple:generate :for
                   :packages '(:for)
                   :if-exists :supersede))
