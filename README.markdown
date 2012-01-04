# Mini-Lang

A simple embedded language on Common Lisp intended to be used to write physical simulation programs, aiming to be compiled to various backends.

## Syntax

  ;;; the BNF of the operation interfaces

  (setf-scalar <place> <expression>)

  (incf-scalar <place> <expression>)

  (for-scalar-array <variable> <index> <body>)

  (setf-scalar-array <expression>)  ; in a body of for-scalar-array only

  (incf-scalar-array <expression>)  ; in a body of for-scalar-array only

  (setf-vec3 <place> <expression>)

  (incf-vec3 <place> <expression>)

  (for-vec3-array <variable> <index> <body>)

  (setf-vec3-array <expression>)  ; in a body of for-vec3-array only

  (incf-vec3-array <expression>)  ; in a body of for-vec3-array only

  ;;; the BNF of the expression

  <expression> ::= <bool-literal>
                 | <int-literal>
                 | <scalar-literal>
                 | <vec3-literal>
                 | <external-environment-reference>
                 | (let (<binding>*) <expression>)
                 | (if <expression> <expression> <expression>)
                 | <variable>
                 | (<op> <expression>*)

  <bool-literal> ::= t | nil

  <int-literal> ::= a fixnum

  <scalar-literal> ::= a double float

  <vec3-literal> ::= (<scalar-literal> <scalar-literal> <scalar-literal>)

  <external-environment-reference> ::= (bool <lisp-form>)
                                     | (int <lisp-form>)
                                     | (scalar <lisp-form>)
                                     | (vec3 <lisp-form>)
                                     | (scalar-aref <lisp-form> <index>)
                                     | (vec3-aref <lisp-form> <index>)

  <lisp-form> ::= a lisp form

  <index> ::= a fixnum

  <binding> ::= (<variable> <expression>)

  <variable> ::= a symbol

  <op> ::= + | - | * | / | norm | exp | =


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

