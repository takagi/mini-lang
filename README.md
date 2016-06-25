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

#### Literals

To be described.

#### References

To be described.

#### THE

To be described.

#### IF

To be described.

#### LET

To be described.

#### Function application

To be described.

### Built-in functions

- Arithmetic functions

    + - * / *. .* /.

- Mathematical functions

    norm

- Vector constructor

    int2 int3 int4 float2 float3 float4 double2 double3 double4 

- Vector accessor

    int2-{x,y} int3-{x,y,z} int4-{x,y,z,w}
    float2-{x,y} float3-{x,y,z} float4-{x,y,z,w}
    double2-{x,y} double3-{x,y,z} double4-{x,y,z,w}

- Array accessor

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

### [Macro] EVAL-MINI-LANG

### [Function] COMPILE-MINI-LANG

### [Function] MAKE-INT{,2,3,4}-ARRAY, MAKE-FLOAT{,2,3,4}-ARRAY, MAKE-DOUBLE{,2,3,4}-ARRAY

### [Accessor] INT{,2,3,4}-AREF*, FLOAT{,2,3,4}-AREF*, DOUBLE{,2,3,4}-AREF*

### [Function] INT{,2,3,4}-ARRAY-DIMENSIONS, FLOAT{,2,3,4}-ARRAY-DIMENSIONS, DOUBLE{,2,3,4}-ARRAY-DIMENSIONS


## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the MIT License.
