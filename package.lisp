#|
 This file is a part of for
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:for-iterator
  (:nicknames #:org.shirakumo.for.iterator)
  (:use #:cl)
  ;; iterator.lisp
  (:export
   #:has-more
   #:next
   #:current
   #:make-iterator
   #:iterator
   #:object
   #:list-iterator
   #:vector-iterator
   #:start
   #:array-iterator
   #:total-length
   #:stream-iterator
   #:buffer
   #:index
   #:limit
   #:directory-iterator
   #:random-iterator
   #:limit
   #:package-iterator
   #:prefetch
   #:hash-table-iterator
   #:prefetch))

(defpackage #:for-minimal
  (:nicknames #:org.shirakumo.for.minimal)
  (:use #:cl)
  ;; for.lisp
  (:export
   #:end-for
   #:skip-for
   #:return-for
   #:for)
  ;; standard.lisp
  (:export
   #:as
   #:in
   #:on
   #:across
   #:over
   #:updating
   #:table-keys
   #:table-values
   #:table-pairs
   #:symbols
   #:between
   #:from
   #:repeating
   #:=
   #:collecting
   #:appending
   #:nconcing
   #:reducing
   #:counting
   #:summing
   #:maximizing
   #:minimizing
   #:always
   #:never
   #:thereis
   #:while
   #:until
   #:returning
   #:update
   #:repeat
   #:collect
   #:append
   #:nconc
   #:reduce
   #:count
   #:sum
   #:maximize
   #:minimize))

(defpackage #:for
  (:nicknames #:org.shirakumo.for)
  (:use #:cl #:org.shirakumo.for.iterator #:org.shirakumo.for.minimal)
  ;; binding.lisp
  (:export
   #:binding
   #:remove-binding
   #:define-alias-binding
   #:define-direct-binding
   #:define-form-binding
   #:define-form-symbol-macro-binding
   #:define-value-binding
   #:define-value-symbol-macro-binding)
  ;; clause.lisp
  (:export
   #:clause
   #:remove-clause
   #:define-clause)
  ;; toolkit.lisp
  (:export
   #:with-interleaving
   #:update))

(do-external-symbols (symb '#:org.shirakumo.for.minimal)
  (export symb '#:org.shirakumo.for))

(do-external-symbols (symb '#:org.shirakumo.for.iterator)
  (export symb '#:org.shirakumo.for))