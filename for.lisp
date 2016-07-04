#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(defmacro with-loop-tagbody (() &body body)
  (let ((for-start (gensym "FOR-START"))
        (for-end (gensym "FOR-END")))
    `(tagbody
        ,for-start
        (macrolet ((end-for () `(go ,',for-end))
                   (skip-for () `(go ,',for-start)))
          ,@body
          (go ,for-start))
        ,for-end)))

(defmacro with-loop-block (() &body body)
  `(block NIL
     (macrolet ((return-for (&rest values) `(return ,@values)))
       ,@body)))

(defmacro for (bindings &body body)
  (multiple-value-bind (vars forms) (convert-bindings bindings)
    `(let* ,vars
       (with-loop-block ()
         (with-loop-tagbody ()
           ,@forms
           (with-clauses ()
             ,@body))))))
