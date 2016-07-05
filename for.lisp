#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

(defmacro with-for-tagbody (() &body body)
  (let ((for-start (gensym "FOR-START"))
        (for-end (gensym "FOR-END")))
    `(tagbody
        ,for-start
        (macrolet ((end-for () `(go ,',for-end))
                   (skip-for () `(go ,',for-start)))
          ,@body
          (go ,for-start))
        ,for-end)))

(defmacro with-for-block (() &body body)
  `(block NIL
     (macrolet ((return-for (&rest values) `(return ,@values)))
       ,@body)))

(defmacro with-interleaving (&body body)
  (let ((sentinel (cons NIL NIL)))
    (loop for next = sentinel then copy
          for form in body
          for copy = (copy-list form)
          do (setf (cdr (last next)) (cons copy NIL)))
    (cadr sentinel)))

(defmacro for (bindings &body body)
  (multiple-value-bind (surrounding forms) (convert-bindings bindings)
    `(with-interleaving
       ,@surrounding
       (with-for-block ()
         (with-for-tagbody ()
           ,@forms
           (with-clauses ()
             ,@body))))))
