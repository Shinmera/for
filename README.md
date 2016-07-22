## About For
For is a library for an extensible iteration macro. It allows you to write concise looping constructs similar to `loop` and `iterate`. Unlike loop however it is extensible and sensible, and unlike iterate it does not require code-walking and is easier to extend.

## How To
Load For using ASDF or Quicklisp.

    (ql:quickload :for)

Now we can use the `for` macro to do iteration. Most of the constructs you know from `loop` are available with the same name in For.

    (for:for ((li in (list 1 2 3 4))
              (vi across #(a b c d)))
      (format T "~&~a ~a" li vi))

Unlike `loop` and `iterate`, `for` makes a distinction between "bindings" and body forms. Body forms can also contain clauses:

    (for:for ((li in (list 1 2 3 4)))
      (thereis (evenp li)))

Naturally, there's also accumulation bindings:

    (for:for ((randoms collecting (random 10)))
      (until (= 10 (length randoms))))

You might realise that the above is a rather inefficient way of writing the loop. Instead we can also use the  `repeat` binding:

    (for:for ((i repeat 10)
              (randoms collecting (random 10))))

If we have multiple bindings or clauses that might have useful values to return, all of them are returned:

    (for:for ((a over *random-state* :limit 10)
              (b collect a))
      (thereis (evenp a)))
      
In order for short-circuiting clauses to have highest priority on values, clause-values are always returned first followed by binding values. Otherwise the order follows the declaration order of the respective clauses/bindings. Note that clauses must appear as a "top-level" form within the `for` body and cannot appear as the result of a macroexpansion.

For also features a generic iterator construct that allows you to iterate over a multitude of different data types without having to do a case-distinction by yourself. We already saw this with the `over *random-state*` binding from the previous example.

    (for:for ((a over '(1 2 3))
              (b over #(a b c))
              (c over (for:for ((table as (make-hash-table))) 
                        (setf (gethash (random 10) table) (random 10)) (repeat 3)))
              (d over *package*)
              (e over *random-state*)
              (f over (directory (merge-pathnames "*.*" (user-homedir-pathname))))
              (g over (make-string-input-stream "Hi!")))
      (print (list a b c d e f g)))

Some iterators also support updating the current element. If you require doing so, you can use the `updating` binding.

    (for:for ((list as (list 1 2 3 4 5))
              (item updating list))
      (setf item (expt item item)))

Some of the bindings also support destructuring the current item by a destructuring-lambda-list.

    (for:for (((type &key object limit) in '((counter :limit 5)
                                             (package :object *package*))))
      (format T "~&Type: ~a~@[ Object: ~a~]~@[ Limit: ~a~]" type object limit))
      
You can check a binding's or clause's documentation with `(documentation 'in 'for:binding)` which will tell you whether it supports destructuring through `update`.

Sometimes you may want to iterate over multiple things in sequence rather than in parallel. For this you can use the `being` binding, which allows you to pass a list of sub-bindings to sequentially use.

    (for:for (((k v) being
               (in '((駅 station) (出口 exit) (特急 express-train)))
               (across #((勉強 studying) (宿題 home-work) (授業 lesson) (試験 exam)))))
      (format T "~&~a: ~a" k v))

If a binding should only be updated based on a condition, there's the `when` and `unless` bindings that defer based on a test.

    (for:for ((random = (random 10))
              (list when (evenp random) collect random))
      (until (= 10 (length list))))

The following bindings are included in the `for-minimal` package:

* `=`
* `across`
* `appending` / `append`
* `as`
* `being`
* `collecting` / `collect`
* `counting` / `count`
* `from`
* `in`
* `maximizing` / `maximize`
* `minimizing` / `minimize`
* `nconcing` / `nconc`
* `on`
* `over`
* `ranging` / `range`
* `reducing` / `reduce`
* `repeating` / `repeat`
* `summing` / `sum`
* `symbols`
* `table-keys`
* `table-pairs`
* `table-values`
* `unless`
* `updating` / `update`
* `when`

The following clauses are included in the `for-minimal` package:

* `always`
* `never`
* `returning`
* `thereis`
* `until`
* `while`

Iterator classes for the following types is included in the `for-iterator` package:

* `list`
* `vector`
* `array`
* `stream`
* `pathname`
* `random-state`
* `package`
* `hash-table` each item is a list of key and value.

## Extending FOR
Both bindings and clauses are defined in terms of functions that return three values:

1. A surrounding form
   Surrounding forms will be wrapped around the rest of the expanded for by appending the rest to it. This happens through `with-interleaving`.
2. A loop body form
   The body form is put inside the loop where it will be evaluated once per iteration.
3. A return value form
   The return value form is evaluated on loop end. The position within the returned values is dependent on the clauses and bindings present during expansion. If not provided, no return value is generated. Note that this is distinct from having NIL as a third value.

Passed to the functions are the literal arguments used in the binding or clause expression. In that way, a clause/binding function must work just like a macro would.

### Bindings
The most primitive way to define bindings is through the `define-direct-binding` macro. This defines a verbatim binding function as described above. Note that the loop body forms of bindings will always be evaluated before the proper for body. 

In most cases you will want the arguments that are passed to the binding to be evaluated only once, before the loop starts properly. The `define-value-binding` macro will help you with that. Each argument you specify will be bound to a gensym within the definition body, and is automatically expanded to a variable with the value that is used in the binding. `&aux` arguments receive special treatment as they are expanded like regular variables and thus allow you to easily define helper variables necessary during iteration.

Let's look at an example binding definition:

    (define-value-binding across (var vector &aux (i -1) (length (length vector)))
      `(if (= ,length (incf ,i))
           (end-for)
           (update ,var (aref ,vector ,i))))

Expanding a simple call `(for ((a across vec)))` results in this expansion (after cleaning it up a little):

    (LET* ((#:VECTOR VEC)
           (#:I -1)
           (#:LENGTH (LENGTH #:VECTOR))
           (A NIL))
      (WITH-FOR-BODY
        (IF (= #:LENGTH (INCF #:I))
            (END-FOR)
            (UPDATE A (AREF #:VECTOR #:I))))

As you can see, our only argument, `vector` got expanded into a gensym-ed variable that is bound to the result of the `vector` argument. Our auxiliary variables received similar treatment. Note that references to other arguments automatically get translated to their proper gensyms.

In some cases however you'd like to re-evaluate an argument each iteration. To get this behaviour, you can use `define-form-binding`. Here's a simple example:

     (define-form-binding = (var form)
       `(update ,var ,form))

Expanding a simple call `(for ((a = (* 2 2))))` presents us with:

     (LET* ((A NIL))
       (WITH-FOR-BODY
         (UPDATE A (* 2 2)))

Usually you will want form bindings if you want to accumulate the results of it over time in some manner. In that case you usually also want to return the result of the accumulation once you're done. `define-accumulation-binding` does exactly that. One note about form bindings is that the auxiliary variables still act the same as in the value bindings-- they automatically get expanded to bindings in the resulting loop construct.

Let's look at an example that shows both:

    (define-accumulation-binding collecting (var form &aux (head (cons NIL NIL)) (tail head))
      `(setf ,tail (setf (cdr ,tail) (cons ,form NIL))
             ,var (cdr ,head)))

Expanding `(for ((a collecting 2)))` results in:

    (LET* ((#:HEAD (CONS NIL NIL)) (#:TAIL #:HEAD) (A NIL))
      (WITH-FOR-BODY
       (SETF #:TAIL (SETF (CDR #:TAIL) (CONS 2 NIL))
             A (CDR #:HEAD))
       (RETURN-FOR A))

As before, the auxiliary arguments got expanded to variable bindings with their respective default values.

Finally we have two variants of form and value bindings, `define-form-symbol-macro-binding` and `define-value-symbol-macro-binding`. The difference to the previous definition forms here is that the `var` is not bound as a variable, but instead as a symbol macro. Its default value is the symbol-macro expansion. This is useful if you want to provide an updateable place as the iteration var, as is the case with the `updating` binding.

### Clauses
Clauses work the exact same as bindings in terms of the base function, which you can define with `define-direct-clause`. Unlike bindings however, clauses simply get the body of their call as arguments, without an iteration var.

In order to ease things a bit there is also `define-simple-clause` which provides the same handling for arguments as `define-form-binding` does.

One thing to note is that the surrounding forms of clauses always appear deeper than those of bindings and that the result value forms of clauses always appear before those of bindings. The loop body form of a clause appears at the exact position in the body where the clause expression previously appeared.

### Iterators
In order to provide the generic `over` iteration construct, For includes a protocol to define iterators. In order for an iterator to work, it has to subclass `iterator` and provide three methods: `make-iterator`, `has-more`, and `next`. The first is merely there so that we can dispatch on the type of object we'd like to iterate over and construct an appropriate iterator for it. The second should return a generalised boolean that tells us whether we can safely call `next`. Finally, `next` itself advances the iterator and returns a new element. If sensible and possible, a method on `(setf current)` can also be provided to allow updating the current element to a new value.

Let's look at the list iterator as an example:

    (defclass list-iterator (iterator)
      ())
    
    (defmethod initialize-instance :after ((iterator list-iterator) &key object)
      (setf (object iterator) (cons NIL object)))
    
    (defmethod has-more ((iterator list-iterator))
      (cdr (object iterator)))
    
    (defmethod next ((iterator list-iterator))
      (setf (object iterator) (cdr (object iterator)))
      (car (object iterator)))
    
    (defmethod (setf current) (value (iterator list-iterator))
      (setf (car (object iterator)) value))
    
    (defmethod make-iterator ((list list) &key)
      (make-instance 'list-iterator :object list))

First we subclass `iterator`. Next we define an initialize method in order to prepend a cons to the list. We do this so that we know the next element will always be in the cadr of the `object` and we can still set the car of the current cons cell to update it. The `has-more` test is implemented accordingly. On `next` we then simply pop off the first cons and return our new current element. The `(setf current)` can then just update the car of the `object`. Finally we need a `make-iterator` method to dispatch on lists.
