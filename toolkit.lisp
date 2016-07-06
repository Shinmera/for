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
