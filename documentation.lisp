#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.for)

;; binding.lisp
(docs:define-docs
  (function binding
    "Accessor to the function that compiles the given binding.
If no such binding exists, an error is signalled.

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
3. A form to run during the loop end.

The arguments that the function receives are directly translated from the
respective binding expression. One argument will always be passed in the
very least: the variable specified for the binding.

Note that a binding function receives its arguments as literals and thus must
process them like a macro would (destructive operations are bad juju).

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
3. A list of all exit forms

See BINDING"))

;; clause.lisp
(docs:define-docs
  (function clause
    "Accessor to the function that compiles the given clause.
If no such clause exists, an error is signalled.

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
3. A form to run during the loop end.

The arguments that the function receives are directly translated from the
respective clause expression.

Note that a clause function receives its arguments as literals and thus must
process them like a macro would (destructive operations are bad juju).

See CLAUSE
See REMOVE-CLAUSE")

  (function define-simple-clause
    "Defines a simple clause.

This is the same as DEFINE-DIRECT-CLAUSE, except that the first return value is
always set to NIL. This means that you can use this if you simply want a clause
that expands to some body form and maybe an exit form.

See DEFINE-DIRECT-CLAUSE")

  (function convert-clauses
    "Translates the given body forms into their respective parts.

If a body form is noticed that matches a clause --by virtue of being a list and
the first item thereof being a symbol that translates to a clause name-- then it
is replaced by the form computed by the clause function.

Returns three values:
1. A list of all surrounding forms
2. A list of all body forms
3. A list of all exit forms

See CLAUSE"))

;; for.lisp
(docs:define-docs
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

Each binding should have the form (variable/destructuring-list binding-type args*)

Within the body, special iteration clauses may be present. A clause must appear
at the \"top-level\" of the body and cannot appear as a macro-expansion.

The return value is determined by one or more of the bindings or clauses. The
iteration may also be ended prematurely by either a direct RETURN-FOR, bypassing
end-of-loop forms, or by a simple END-FOR.

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

Supports setting the current element.

See VECTOR-ITERATOR
See TOTAL-LENGTH")

  (function total-length
    "Slot holding the array-total-size.

See ARRAY-ITERATOR")

  (type stream-iterator
    "Iterator for input streams.

The stream is read through a buffer, the size of which can be set via the :BUFFER-SIZE
initarg.

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

  (type directory-iterator
    "Iterator for a DIRECTORY listing.

On construction, this performs a simple DIRECTORY call on the given object
and then iterates over the result list of pathnames.

See LIST-ITERATOR")

  (type random-iterator
    "Iterator for random numbers.

This iterator can be constructed through either a RANDOM-STATE object or through the
:RANDOM keyword as an object. The argument for RANDOM that determines its limit can be
passed through the :LIMIT initarg.

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

  (function hash-table-iterator
    "Iterator for the key/value pairs in a package.

Each value returned by this iterator's CURRENT/NEXT is always a list of two values, the
respective key and its value.

Supports setting the current element.

See PREFETCH
See ITERATOR"))

;; standard.lisp
(docs:define-docs
  )

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

Unlike just (setf place value), PLACE can also be a destructuring-lambda-list
where each variable is then properly updated with the respective element from
value list.")

  (function hash-table-iterator
    "Returns a function to iterate over a hash-table.

See CL:WITH-HASH-TABLE-ITERATOR")

  (function package-iterator
    "Returns a function to iterate over a package's symbols.

See CL:WITH-PACKAGE-ITERATOR"))
