# Mini-lang

Efficient and expressive vector math library with an embedded language on Common Lisp.

## Embedded language

### Data and Types

Mini-lang provides the following data types.

**Scalar types**

    int float double

**Vector types**

    int2 int3 int4
    float2 float3 float4
    double2 double3 double4

**Array types**

    int-array int2-array int3-array int4-array
    float-array float2-array float3-array float4-array
    double-array double2-array double3-array double4-array

### Syntax

Mini-lang provides the following syntax.

**Literal**

To be described.

**Reference**

To be described.

**THE form**

    THE type form

**IF form**

    IF test-form then-form else-form

**LET form**

    LET

**Function application**

To be described.

### Built-in functions

Mini-lang provides the following built-in functions.

**Arithmetic functions**

    + - * / *. .* /.

**Mathematical functions**

    norm

**Vector constructor**

    int2 int3 int4 float2 float3 float4 double2 double3 double4 

**Vector accessors**

    int2-{x,y} int3-{x,y,z} int4-{x,y,z,w}
    float2-{x,y} float3-{x,y,z} float4-{x,y,z,w}
    double2-{x,y} double3-{x,y,z} double4-{x,y,z,w}

**Array accessor**

    aref

### Grammar

#### Syntactic grammar

Here shows the grammar of embedded language syntax.

    form := literal
          | reference
          | accessor
          | the
          | if
          | let
          | application

    literal := int literal | float literal | double literal

    int literal := x st. (INTEGERP x)

    float literal := x st. (TYPEP x 'SINGLE-FLOAT)

    double literal := x st. (TYPEP x 'DOUBLE-FLOAT)

    reference := symbol

    symbol := x st. (SYMBOLP x)

    accessor := array accessor

    array accessor := (AREF form form form*)

    the := (THE type form)

    type := int | int2 | int3 | int4
          | float | float2 | float3 | float4
          | double | double2 | double3 | double4

    if := (IF form form form)

    let := (LET ((var form)*) form)

    var := symbol

    application := (name form*)

    name := symbol

#### Internal type grammar

Here shows the grammar of type representation internally used.

    type := scalar type
          | vector type
          | array type
          | type variable

    scalar type := BOOL | INT | FLOAT | DOUBLE

    vector type := (:VECTOR scalar-type size)
                 | (:VECTOR type-variable size)

    size := 2 | 3 | 4 | _ | type size variable

    array type := (:ARRAY scalar-type)
                | (:ARRAY vector-type)
                | (:ARRAY type-variable)

    type variable := ?T0 | ?T1 | ...

#### Internal type scheme

Here shows the grammer of type scheme representation internally used.

    type scheme := (:TYPE-SCHEME type-symbol type-symbol*)

    type symbol := scalar type symbol
                 | vector type symbol
                 | array type symbol

    scalar type symbol := scalar type
                        | type scheme variable

    type scheme variable := A | B | C | ...

    vector type symbol := (:VECTOR scalar-type-symbol size)

    size := 2 | 3 | 4 | _ | type scheme variable

    array type symbol := (:ARRAY scalar-type-symbol)
                       | (:ARRAY vector-type-symbol)

## API

### [Macro] eval-mini-lang

    EVAL-MINI-LANG

### [Function] compile-mini-lang

    COMPILE-MINI-LANG

### [Function] make-int{,2,3,4}-array, make-float{,2,3,4}-array, make-double{,2,3,4}-array

    MAKE-INT{,2,3,4}-ARRAY
    MAKE-FLOAT{,2,3,4}-ARRAY
    MAKE-DOUBLE{,2,3,4}-ARRAY

### [Accessor] int{,2,3,4}-aref\*, float{,2,3,4}-aref\*, double{,2,3,4}-aref\*

    INT{,2,3,4}-AREF*
    FLOAT{,2,3,4}-AREF*
    DOUBLE{,2,3,4}-AREF*

### [Function] int{,2,3,4}-array-dimensions, float{,2,3,4}-array-dimensions, double{,2,3,4}-array-dimensions

    INT{,2,3,4}-ARRAY-DIMENSIONS
    FLOAT{,2,3,4}-ARRAY-DIMENSIONS
    DOUBLE{,2,3,4}-ARRAY-DIMENSIONS

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the MIT License.
