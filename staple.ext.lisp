(defpackage #:for-staple
  (:nicknames #:org.shirakumo.for.staple)
  (:use #:cl))
(in-package #:org.shirakumo.for.staple)

(defclass binding (definitions:global-definition) ())

(definitions:define-simple-type-map binding for:binding)
(definitions:define-simple-object-lookup binding for:binding)
(definitions:define-simple-documentation-lookup binding for:binding)
(definitions:define-simple-definition-resolver binding for:binding)
(defmethod staple:definition-order ((_ binding)) 91)

(defclass clause (definitions:global-definition) ())

(definitions:define-simple-type-map clause for:clause)
(definitions:define-simple-object-lookup clause for:clause)
(definitions:define-simple-documentation-lookup clause for:clause)
(definitions:define-simple-definition-resolver clause for:clause)
(defmethod staple:definition-order ((_ clause)) 92)

(setf (staple:packages :for) '(:for))
