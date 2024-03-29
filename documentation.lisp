(in-package #:org.shirakumo.for)

;; binding.lisp
(docs:define-docs
  (function binding
    "Accessor to the function that compiles the given binding.

If there is no binding named by the given symbol directly, another search is
performed using the symbol with the same symbol-name from the FOR package.

See REMOVE-BINDING")

  (function remove-binding
    "Removes the given binding function.

See BINDING")

  (function define-alias-binding
    "Defines an alias for a binding.

See BINDING")

  (function define-direct-binding
    "Defines a binding function.

Binding functions can return three values:

1. A single \"surrounding form\" that will go around the resulting loop. If you
   require multiple surrounding forms, you can use WITH-INTERLEAVING.
2. A form to run during each iteration. They will be run before any body forms.
3. A form whose value is returned by the FOR.
4. A form to run during each iteration. They will be run *after* any body forms.

The arguments that the function receives are directly translated from the
respective binding expression. One argument will always be passed in the
very least: the variable specified for the binding.

Note that a binding function receives its arguments as literals and thus must
process them like a macro would (destructive operations are bad juju).

Also note that unlike normal functions, the &environment lambda-list argument
is available and its value will be passed on from the calling FOR macro.

See BINDING
See REMOVE-BINDING")

  (function define-form-binding
    "Defines a binding that receives its arguments as literals.

&AUX variables in the ARGS lambda-list receive special treatment: they are
bound to gensyms within the definition body. Their value is only evaluated
and set within the expanded binding. This means that &AUX variables give you a
convenient way to introduce necessary helper variables to the expanded binding.
References to other AUX variables or the VAR are automatically rewritten to
the appropriate gensym.

VAR can also accept a default value, which receives the same semantic treatment
as &AUX variables do, with the exception that it is always the last binding to
be evaluated in the resulting expansion, meaning every other &AUX variable can
be referenced.

The primary value returned must be the form to be evaluated on each iteration.
A secondary value may be returned, which is a form to be evaluated when the
loop ends normally.

See DEFINE-DIRECT-BINDING
See DEFINE-VALUE-BINDING")

  (function define-accumulation-binding
    "Defines a binding for an accumulator.

This is identical to DEFINE-FORM-BINDING with the exception that the
secondary value is set to a (RETURN-FOR var) for you, meaning the
variable's contents are returned from the FOR upon normal termination.

See DEFINE-FORM-BINDING")

  (function define-form-symbol-macro-binding
    "Defines a binding that receives its arguments as literals and treats the VAR as a symbol-macro.

This is the exact same as DEFINE-FORM-BINDING with the exception that the
VAR is translated into a symbol-macro binding. Its value is still translated
accordingly to make sure references to AUX variables stay intact.

See DEFINE-FORM-BINDING")

  (function define-value-binding
    "Defines a binding that receives its arguments as values.

The treatment of all arguments in the ARGS lambda-list is as follows:
Within the definition body, they are bound to gensyms. Upon expansion of the
binding, each variable is expanded to a variable binding with the respective
value that was passed to the binding definition. Special exception is made for
the present-p optional variables that can be specified for optional or key
arguments, which are bound as usual in the definition body such that expansion
may be aware of which parameters were passed. In essence, you can interpret all
arguments as if treated by ONCE-ONLY.

&AUX variables in the args lambda-list receive special treatment: they are
bound to gensyms within the definition body. Their value is only evaluated
and set within the expanded binding. This means that AUX variables give you a
convenient way to introduce necessary helper variables to the expanded binding.
References to other arguments or the VAR are automatically rewritten to
the appropriate gensym.

VAR can also accept a default value, which receives the same semantic treatment
as &AUX variables do, with the exception that it is always the last binding to
be evaluated in the resulting expansion, meaning every other argument can
be referenced.

The primary value returned must be the form to be evaluated on each iteration.
A secondary value may be returned, which is a form to be evaluated when the
loop ends normally.

See DEFINE-FORM-BINDING
See DEFINE-DIRECT-BINDING")

  (function define-value-symbol-macro-binding
    "Defines a binding that receives its arguments as values and treats the VAR as a symbol-macro.

This is the exact same as DEFINE-VALUE-BINDING with the exception that the
VAR is translated into a symbol-macro binding. Its value is still translated
accordingly to make sure references to arguments stay intact.

See DEFINE-VALUE-BINDING")

  (function convert-bindings
    "Translates the given binding expressions into their respective parts.

Each binding must be a list of the following structure:

BINDING      ::= \(PLACE binding-type argument*)
PLACE        ::= variable | lambda-list
variable     --- A symbol naming a variable to bind the result of the binding to.
lambda-list  --- If the binding result is a list, destructure it by this lambda-
                 list, binding the respective symbols.
binding-type --- The type of binding determines how the PLACE is initialised and
                 how it changes each iteration.
argument     --- An argument to the binding. Depending on the binding it may be
                 evaluated once or in every iteration.

If an unknown binding is referenced, an error is signalled.

Returns three values:
1. A list of all surrounding forms
2. A list of all body forms
3. A list of all value forms

See BINDING"))

;; clause.lisp
(docs:define-docs
  (function clause
    "Accessor to the function that compiles the given clause.

If there is no clause named by the given symbol directly, another search is
performed using the symbol with the same symbol-name from the FOR package.

See REMOVE-CLAUSE")

  (function remove-clause
    "Removes the given clause function.

See CLAUSE")

  (function define-direct-clause
    "Defines a clause function.

Clause functions can return three values:

1. A single \"surrounding form\" that will go around the resulting loop. If you
   require multiple surrounding forms, you can use WITH-INTERLEAVING.
2. A form to run during each iteration.
3. A form whose value is returned by the FOR.

The arguments that the function receives are directly translated from the
respective clause expression.

Note that a clause function receives its arguments as literals and thus must
process them like a macro would (destructive operations are bad juju).

Also note that unlike normal functions, the &environment lambda-list argument
is available and its value will be passed on from the calling FOR macro.

See CLAUSE
See REMOVE-CLAUSE")

  (function define-simple-clause
    "Defines a simple clause.

&AUX variables in the args lambda-list receive special treatment: they are
bound to gensyms within the definition body. Their value is only evaluated
and set within the expanded clause. This means that AUX variables give you a
convenient way to introduce necessary helper variables to the expanded clause.
References to other &AUX are automatically rewritten to the appropriate gensym.

The primary value returned must be the form to be evaluated on each iteration.
A secondary value may be returned, which is a form to be evaluated when the
loop ends normally.

See DEFINE-DIRECT-CLAUSE")

  (function convert-clauses
    "Translates the given body forms into their respective parts.

If a body form is noticed that matches a clause --by virtue of being a list and
the first item thereof being a symbol that translates to a clause name-- then it
is replaced by the form computed by the clause function.

Returns three values:
1. A list of all surrounding forms
2. A list of all body forms
3. A list of all value forms

See CLAUSE"))

;; for.lisp
(docs:define-docs
  (variable *environment*
    "This variable will be bound to the environment object during the expansion of the FOR macro.")
  
  (function with-for-tagbody
    "Wraps the body in a looping tagbody, running the EXIT forms when the loop ends.

See END-FOR
See SKIP-FOR")

  (function end-for
    "Ends the for loop and jumps to the epilogue.

This is a local macro in WITH-FOR-TAGBODY.

See WITH-FOR-TAGBODY")

  (function skip-for
    "Skips the current iteration and jumps to the next one.

This is a local macro in WITH-FOR-TAGBODY.

See WITH-FOR-TAGBODY")

  (function with-for-block
    "Wraps the body in a looping block.

This establishes a NIL block.

See RETURN-FOR")

  (function return-for
    "Immediately returns the VALUES from the for block.

This is a local macro in WITH-FOR-BLOCK.

See WITH-FOR-BLOCK")

  (function for
    "Loops the body with the given bindings established.

Each binding should have the form (var binding-type args*)

Sometimes a var can be either a single symbol denoting a variable, or a
lambda-list to which the result is destructured and bound via UPDATE.
The support thereof depends on the binding construct.

Within the body, special iteration clauses may be present. A clause must appear
at the \"top-level\" of the body and cannot appear as a macro-expansion.

If the loop is terminated normally by END-FOR then multiple values may be returned
depending on how many bindings or clauses are present that want to return values.
The order of the values is as follows: the clause values are returned in the
order that the clauses appear in the body, followed by the binding values in the
order of the binding expressions.

The loop may also be terminated abnormally by a direct call to RETURN-FOR or RETURN.

See UPDATE
See BINDING
See CLAUSE
See END-FOR
See SKIP-FOR
See RETURN-FOR
See WITH-FOR-TAGBODY
See WITH-FOR-BLOCK
See CONVERT-BINDINGS
See CONVERT-CLAUSES"))

;; iterator.lisp
(docs:define-docs
  (function has-more
    "Returns a generalised boolean indicating whether the iterator has more items or not.")

  (function next
    "Advances the iterator by one item and returns the new item.

The behaviour is undefined if the iterator does not have more items.

See HAS-MORE")

  (function current
    "Accessor to the current item of the iterator.

The behaviour is undefined if CURRENT is used before NEXT has been called for a first time.
Some (but not all) iterators may support setting the current element to a new value.

See NEXT")

  (function end
    "Ends the iterator and performs potential cleanup.

You should always call this function with your iterator object once you are done to ensure
proper termination.")

  (function step-functions
    "Returns a set of functions to perform the iteration.

Returns four values:

   NEXT      --- Function of zero arguments that returns the next element.
   HAS-MORE  --- Function of zero arguments that returns whether there are
                 more elements available.
   UPDATE    --- Function of one argument that sets the current element to
                 the given value if possible.
   END       --- Function of zero arguments to finalise the iteration.

Iterators may specialise on this method to return tailored stepping
functions that avoid the CLOS dispatch cost. Note that calling these
functions may or may not change the internal iterator state.")

  (function make-iterator
    "Create an iterator object for the given type of object.")

  (type iterator
    "An iterator is responsible for iterating over a given data structure.

See HAS-MORE
See NEXT
See CURRENT
See MAKE-ITERATOR
See OBJECT")

  (function object
    "Accessor to the data structure the iterator is iterating over.

Note that this is not necessarily the same object as what was passed into the
constructor of the iterator. The iterator is free to modify this slot as it sees fit.

See ITERATOR")

  (type list-iterator
    "Iterator for proper lists.

Supports setting the current element.

See ITERATOR")

  (type vector-iterator
    "Iterator for vectors.

Supports setting the current element.

See START
See ITERATOR")

  (function start
    "Accessor to the index that points to the next element of the vector-iterator.")

  (type array-iterator
    "Iterator for general arrays.

Iteration is in row-major order.

Supports setting the current element.

See VECTOR-ITERATOR
See TOTAL-LENGTH")

  (function total-length
    "Slot holding the array-total-size.

See ARRAY-ITERATOR")

  (type stream-iterator
    "Iterator for input streams.

The stream is read through a buffer, the size of which can be set via the :BUFFER-SIZE
initarg. If :CLOSE-STREAM is set to non-NIL, CLOSE is performed on the stream upon END.

Supports setting the \"current\" element if the stream supports writing to it of course.

See BUFFER
See INDEX
See LIMIT
See ITERATOR")

  (function buffer
    "Accessor to the stream-iterator's buffer.

See STREAM-ITERATOR")

  (function index
    "Accessor to the current index within the buffer.

See STREAM-ITERATOR")

  (function limit
    "Accessor to the amount of data that is currently filled in the buffer.

See STREAM-ITERATOR")

  (function close-stream
    "Accessor to whether the stream should be closed on END call or not.

See STREAM-ITERATOR
See STREAM-LINE-ITERATOR")

  (type stream-line-iterator
    "Iterator for line based input streams.

If :CLOSE-STREAM is set to non-NIL, CLOSE is performed on the stream upon END.

See BUFFER
See CLOSE-STREAM
See ITERATOR")

  (type directory-iterator
    "Iterator for a DIRECTORY listing.

On construction, this performs a simple DIRECTORY call on the given object
and then iterates over the result list of pathnames. Thus, the pathname must
be wild.

See LIST-ITERATOR")

  (type random-iterator
    "Iterator for random numbers.

This iterator can be constructed through a RANDOM-STATE object. The argument for RANDOM
that determines its limit can be passed through the :LIMIT initarg.

See ITERATOR")

  (type package-iterator
    "Iterator for the symbols in a package.

The type of symbols that are iterated can be set through the :STATUS initarg, which must
be a list containing any of the following keywords: :INTERNAL :EXTERNAL :INHERITED

See PREFETCH
See ITERATOR")

  (function prefetch
    "Cache for the next value

Since the iterator constructs provided by CL do not allow merely testing whether a next
element is available without losing it if there is one, we must cache the value on a
HAS-MORE call and then use that on NEXT instead of calling the iterator function twice.

See PACKAGE-ITERATOR
See HASH-TABLE-ITERATOR")

  (type hash-table-iterator
    "Iterator for the key/value pairs in a package.

Each value returned by this iterator's CURRENT/NEXT is always a list of two values, the
respective key and its value.

Supports setting the current element.

See PREFETCH
See ITERATOR"))

;; standard.lisp
(docs:define-documentation-test binding (symb)
  (binding symb))

(docs:define-documentation-test clause (symb)
  (clause symb))

(docs:define-docs
  (binding being
    "Sequences multiple bindings onto the same var.

Expected as arguments is a list of binding expressions, except with each of them missing
the initial var as that is supplied by the var of the BEING binding.

During the iteration the var is then updated by the first inner binding until that calls
END-FOR, after which the var is then updated by the second binding and so forth until the
last binding calls END-FOR, in which case the behaviour is as usual.

Essentially this just does what you expect it would: it chains multiple bindings onto the
same var, sequentially using the next one after the one before finishes.

May support UPDATE depending on the inner bindings.

Note that only bindings that output a LET/LET* as their surrounding form are supported
without potential warnings about unused variables. The bindings also must not output the
var as a symbol macro as it is not possible to update that depending on which binding
is currently active.")

  (binding when
    "Conditionally apply a binding.

The sub-binding's step form is only evaluated if the given test succeeds. This means that
the var is only updated and the loop can only be ended when the test returns non-NIL.

May support UPDATE depending on the sub-binding.")

  (binding unless
    "Conditionally apply a binding.

The sub-binding's step form is only evaluated if the given test fails. This means that
the var is only updated and the loop can only be ended when the test returns NIL.

May support UPDATE depending on the sub-binding.")
  
  (binding as
    "Simply binds the value of the form to the var.

This returns the var on END-FOR.")
  
  (binding in
    "Iterates over a list, binding the current list element to the var every time.

Supports UPDATE.

Accepts a BY keyword argument, which determines how the list is stepped.")
  
  (binding on
    "Iterates over a list, binding the current list cons to the var every time.

Supports UPDATE.

Accepts a BY keyword argument, which determines how the list is stepped.")
  
  (binding across
    "Iterates over a vector, binding the current item to the var every time.

Supports UPDATE.")
  
  (binding over
    "Iterates over a generic sequence using an ITERATOR, binding the current item to the var every step.

Supports UPDATE.

Potentially accepts arbitrary arguments, depending on which iterator is selected
for the respective object.

See FOR-ITERATOR:MAKE-ITERATOR")

  (binding updating
    "Iterates over a generic sequence using an ITERATOR, with var being symbol macro to the current item.

You may SETF the var to update the item in the sequence, if the underlying iterator
supports doing so.

Potentially accepts arbitrary arguments, depending on which iterator is selected
for the respective object.

See FOR-ITERATOR:MAKE-ITERATOR")
  
  (binding table-keys
    "Iterates over a hash table, binding the current key to the var every time.

Supports UPDATE.")
  
  (binding table-values
    "Iterates over a hash table, binding the current value to the var every time.

Supports UPDATE.")
  
  (binding table-pairs
    "Iterates over a hash table, binding a list of the current key and value to the var every time.

Supports UPDATE.")
  
  (binding symbols
    "Iterates over the symbols of a package, binding the current symbol to the var every time.

Accepts an optional list of arguments that qualify the type of symbols to iterate over.
Each type must be one of :INTERNAL :EXTERNAL :INHERITED. If no arguments are given, it
defaults to (:INTERNAL :EXTERNAL :INHERITED).")
  
  (binding ranging
    "Iterates the var over the given range.

Ranges can be either decreasing or increasing. Both limits are inclusive. This means that
(a ranging 0 2) iterates over 0, 1, and 2.

An optional BY keyword argument is accepted, which determines the step.")
  
  (binding from
    "Increases the var by a step every time up to an optional limit.

An optional BY keyword argument is accepted, which determines the step.
An optional TO keyword argument is accepted, which determines the exclusive limit.
This means (a from 0 :to 2) iterates over 0 and 1. If TO is smaller than FROM then
the variable is decreased by BY every step.")
  
  (binding repeating
    "Increases the var the given amount of times starting with 1.

This is the same as (a between 1 n)")

  (binding lines-of
    "Iterates over the given file or stream, reading a line each time.

The stream is always closed when the For loop exits.")
  
  (binding =
    "Updates the var by the value of the given form every time.

If :then is passed, the first form is only used to provide the first iteration's
value of the var. Subsequent iterations will use the value of the :then form.

Supports UPDATE.")
  
  (binding collecting
    "Collects the results of FORM into a list where the VAR points to the head of the list.

This returns the var on END-FOR.")
  
  (binding appending
    "Appends the results of FORM into a list where the VAR points to the head of the list.

This returns the var on END-FOR.")
  
  (binding nconcing
    "Appends the results of FORM destructively into a list where the VAR points to the head of the list.

This returns the var on END-FOR.")
  
  (binding reducing
    "Combines the results of FORM by a combination function BY into var.

On the first iteration the var is simply set to the result of the FORM. On every
successive step, the var is set to the result of calling BY with the var as the
first argument and the result of the FORM as the second argument.

This returns the var on END-FOR.")
  
  (binding counting
    "Increases the var by one if the FORM returns non-NIL.

This returns the var on END-FOR.")
  
  (binding summing
    "Sums up the value of the FORM into the var.

This returns the var on END-FOR.")
  
  (binding maximizing
    "Sets the var to the maximum of the values returned by FORM so far each step.

If KEY is passed, the comparison is performed by the result of the KEY function on each value.

This returns the var on END-FOR.")
  
  (binding minimizing
    "Sets the var to the minimum of the values returned by FORM so far each step.

If KEY is passed, the comparison is performed by the result of the KEY function on each value.

This returns the var on END-FOR.")
  
  (clause always
    "If FORM always returns non-NIL, then T is returned.
As soon as FORM returns NIL the for is aborted with END-FOR and NIL is returned.")
  
  (clause never
    "If FORM never returns non-NIL, then T is returned.
As soon as FORM returns non-NIL the for is aborted with END-FOR and NIL is returned.")
  
  (clause thereis
    "If FORM never returns non-NIL, then NIL is returned.
As soon as FORM returns non-NIL the for is aborted with END-FOR and the non-NIL value is returned.

If KEY is passed, the result of the KEY function on the VALUE is used for the non-NIL test.")
  
  (clause while
    "When FORM returns NIL, END-FOR is called.")
  
  (clause until
    "When FORM returns non-NIL, END-FOR is called.")
  
  (clause returning
    "Does nothing each step, but makes sure to evaluate and return the value of FORM on END-FOR.")

  (clause repeat
    "Makes sure the loop body is repeated a maximum of N times."))

;; toolkit.lisp
(docs:define-docs
  (function values*
    "The same idea as LIST* except for values.")

  (function enlist
    "If A is not a list, turn it into one with the given ELS as further items.")

  (function delist
    "If A is a list, return the element that KEY retrieves.")

  (function with-interleaving
    "Interleave the body forms.

Essentially this means that the last form is appended to the form before it
and this is then appended to the form before that, and so on.")

  (function copy-list*
    "Same as COPY-LIST, but also returning the last cons of the new list as a second value.")

  (function replace-lambda-vars
    "Replaces all VARS in LAMBDA-LIST with the NEW-VAR at the same position.")

  (function update
    "Allows updating the PLACE with a new value.

Unlike just (setf place value), PLACE can also be a lambda-list where each variable
is then properly updated with the respective element from value list.")

  (function hash-table-iterator
    "Returns a function to iterate over a hash-table.

See CL:WITH-HASH-TABLE-ITERATOR")

  (function package-iterator
    "Returns a function to iterate over a package's symbols.

See CL:WITH-PACKAGE-ITERATOR"))
