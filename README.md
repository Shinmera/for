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

    (for:for ((a over :random :limit 10)
              (b collect a))
      (thereis (evenp a)))
      
In order for short-circuiting clauses to have highest priority on values, clause-values are always returned first followed by binding values. Otherwise the order follows the declaration order of the respective clauses/bindings.

For also features a generic iterator construct that allows you to iterate over a multitude of different data types without having to do a case-distinction by yourself. We already saw this with the `over :random` binding from the previous example.

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

The following bindings are included in the `for-minimal` package:

* `=`
* `across`
* `appending` / `append`
* `as`
* `between`
* `collecting` / `collect`
* `counting` / `count`
* `from`
* `in`
* `maximizing` / `maximize`
* `minimizing` / `minimize`
* `nconcing` / `nconc`
* `on`
* `over`
* `reducing` / `reduce`
* `repeating` / `repeat`
* `summing` / `sum`
* `symbols`
* `table-keys`
* `table-pairs`
* `table-values`
* `updating` / `update`

The following clauses are included in the `for-minimal` package:

* `always`
* `never`
* `returning`
* `thereis`
* `until`
* `while`

## Extending FOR
### Iterators


### Bindings


### Clauses

