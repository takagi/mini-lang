# Mini-Lang

A simple embedded language on Common Lisp intended to be used to write physical simulation programs, aiming to be compiled to various backends.

## Syntax

    ;;; operation interfaces for scalar type

    (setf-scalar <variable> <expression>)

    (incf-scalar <variable> <expression>)

    (setf-scalar-array <variable> <index> <expression>)

    (incf-scalar-array <variable> <index> <expression>)

    (for-scalar-array <array-variable> <index-variable> <lisp-form>*)

    ;;; operation interfaces for vec3 type

    (setf-vec3 <variable> <expression>)

    (incf-vec3 <variable> <expression>)

    (setf-vec3-array <variable> <index> <expression>)

    (incf-vec3-array <variable> <index> <expression>)

    (for-vec3-array <array-variable> <index-variable> <lisp-form>*)

    ;;; function definition

    (define-function <name> (<args>) <exp>)
      where <args> ::= <arg>*
            <arg>  ::= (<type> <variable>)

    (clear-functions)

    ;;; the BNF of the expression

    <expression> ::= <bool-literal>
                   | <int-literal>
                   | <scalar-literal>
                   | <vec3-literal>
                   | <external-environment-reference>
                   | (let (<binding>*) <expression>)
                   | (if <expression> <expression> <expression>)
                   | <variable>
                   | (<user-defined-function> <expression>*)
                   | (<built-in-function> <expression>*)

    <bool-literal> ::= t | nil

    <int-literal> ::= a fixnum

    <scalar-literal> ::= a double float

    <vec3-literal> ::= (<scalar-literal> <scalar-literal> <scalar-literal>)

    <external-environment-reference> ::= (bool <lisp-form>)
                                       | (int <lisp-form>)
                                       | (scalar <lisp-form>)
                                       | (vec3 <lisp-form>)
                                       | (vec3 <lisp-form> <lisp-form> <lisp-form>)
                                       | (scalar-aref <lisp-form> <index>)
                                       | (vec3-aref <lisp-form> <index>)

    <lisp-form> ::= a lisp form

    <index> ::= a fixnum

    <binding> ::= (<variable> <expression>)

    <variable> ::= a symbol

    <user-defined-function> ::= a function defined by define-function

    <built-in-function> ::= + | - | * | / | norm | dot | exp | expt | = | <= | > | debug


## Usage

(not provided yet)

## Installation

(not provided yet)

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)

# License

Licensed under the LLGPL License.

