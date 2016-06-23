#|
  This file is a part of mini-lang project.
  Copyright (c) 2011 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage mini-lang
  (:use :cl
        :mini-lang.lang.data
        :mini-lang.lang.symbol
        :mini-lang.lang.type
        :mini-lang.lang.built-in
        :mini-lang.lang.unification-environment
        :mini-lang.lang.type-environment
        :mini-lang.lang.variable-environment
        :mini-lang.lang.free-variables
        :mini-lang.lang.infer
        :mini-lang.lang.compile
        :mini-lang.lang)
  (:export ;; Built-in
           :*. :.* :/.
           :int2 :int3 :int4
           :float2 :float3 :float4
           :double2 :double3 :double4
           :int2-x :int2-y
           :int3-x :int3-y :int3-z
           :int4-x :int4-y :int4-z :int4-w
           :float2-x :float2-y
           :float3-x :float3-y :float3-z
           :float4-x :float4-y :float4-z :float4-w
           :double2-x :double2-y
           :double3-x :double3-y :double3-z
           :double4-x :double4-y :double4-z :double4-w
           :norm
           ;; API
           :eval-mini-lang
           :compile-mini-lang
           :make-int-array :make-float-array :make-double-array
           :int-array-dimensions :float-array-dimensions
           :double-array-dimensions
           :make-int2-array :make-int3-array :make-int4-array
           :make-float2-array :make-float3-array :make-float4-array
           :make-double2-array :make-double3-array :make-double4-array
           :int2-array-dimensions :float2-array-dimensions
           :double2-array-dimensions
           :int3-array-dimensions :float3-array-dimensions
           :double3-array-dimensions
           :int4-array-dimensions :float4-array-dimensions
           :double4-array-dimensions
           :int2-aref* :int3-aref* :int4-aref*
           :float2-aref* :float3-aref* :float4-aref*
           :double2-aref* :double3-aref* :double4-aref*
           ))
(in-package :mini-lang)
