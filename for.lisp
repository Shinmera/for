(in-package #:org.shirakumo.for)

(defvar *environment*)

(defmacro with-for-tagbody (body &body exit)
  (let ((for-start (gensym "FOR-START"))
        (for-end (gensym "FOR-END")))
    `(macrolet ((end-for () `(go ,',for-end))
                (skip-for () `(go ,',for-start)))
       (tagbody
          ,for-start
          ,body
          (go ,for-start)
          ,for-end
          (progn ,@exit)))))

(defmacro with-for-block (() &body body)
  `(block NIL
     (macrolet ((return-for (&rest values)
                  (cond ((not values) `(return NIL))
                        ((cdr values) `(return (values ,@values)))
                        (T            `(return ,(first values))))))
       ,@body)))

(defmacro for (&environment env bindings &body body)
  (let ((*environment* env))
    (multiple-value-bind (bind-init bind-forms bind-exit bind-post) (convert-bindings bindings)
      (multiple-value-bind (clause-init body-forms clause-exit) (convert-clauses body)
        `(with-interleaving
           ,@bind-init
           ,@clause-init
           (with-for-block ()
             (with-for-tagbody
                 (progn ,@bind-forms
                        ,@body-forms
                        ,@bind-post)
               (return-for
                ,@clause-exit
                ,@bind-exit))))))))
