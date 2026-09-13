# Otus Lisp Type System

## reference & value

All entities in Otus Lisp (Ol) are divided into two main classes: *reference* and *value*. These names reflect the method by which these entities are passed along the Ol computational pipeline.

The *value* class represents small numbers and constants that fit entirely within the virtual machine's register and are always passed by value (between registers, as argument values or function results, as components of lists or vectors, etc.). When passed, the connection between the original value and the passed value is broken.

All other entities belong to the *reference* class and are passed only using references to their actual location in the heap. If you modify the value of such an object using a mutator, you will also modify the original object.

## type-.....'s

At the computation level, entities are further divided into types. This division includes constants, numbers, strings, pairs, vectors, and more. Types influence how a specific computation is interpreted by the evaluator. For example, the length of a string can be calculated for a string but not for a number (although a number can be converted to a string, and then its length can be calculated). The type of an entity can be obtained using the basic function `type`, and the type name using `typename`.

```scheme
> (type 123)
0

> (type -123)
32

> (typename 32)
'type-value-

> (type "123")
3

> (typename 3)
'type-string

```

You can also use more specialized functions (predicates) to simply check if an entity belongs to a specific type, such as `boolean?`, `string?`, etc.
```scheme
> (string? 123)
#false

> (string? "123")
#true

```

The following table provides a detailed list of type names, descriptions of the values of each type, and the class (reference or value) to which each type belongs.

| typename         | description                                         | class       | comment |
|------------------|-----------------------------------------------------|-------------|---------|
| type-value+      | Positive number that fits within a value            | *value*     | [0 .. +16777215] or [0 .. +72057594037927935] depending on the bitness of the target platform |
| type-value+      | Negative number that fits within a value            | *value*     | [-16777215 .. -1] or [-72057594037927935 .. -1] depending on the bitness of the target platform |
| type-integer+    | Positive number that does not fit within a value     | *reference* | [+72057594037927936 .. ∞], the actual limit depends on the available RAM |
| type-integer-    | Negative number that does not fit within a value     | *reference* | [-∞ .. -72057594037927936], the actual limit depends on the available RAM |
| type-rational    | Rational number, a pair of numerator/denominator     | *reference* | always in the form of an irreducible fraction |
| type-complex     | Complex number, a pair of real and imaginary parts | *reference* | |
| type-inexact     | Inexact number, a *floating-point* number         | *reference* | the precision depends on the target platform and build options of olvm |
| type-pair        | Dot pair                                       | *reference* | |
| type-vector      | Vector                                              | *reference* | |
| type-symbol      | Symbol                                              | *reference* | |
| type-bytevector  | Bytevector                                          | *reference* | |
| type-string      | ANSI string                                         | *reference* | each character is one byte |
| type-string-wide | Unicode string                                      | *reference* | each character (rune in Ol terminology) is one value |
| type-superstring | Composite "smart" string                            | *reference* | is a concatenation of other strings, possibly a mix of ANSI and Unicode |
| type-const       | Language constant                                  | *value*     | *#true*, *#false*, *#null*, etc. |
| type-bytecode    | Bytecode of the Ol virtual machine                       | *reference* | |
| type-procedure   | Procedure, Function                                  | *reference* | |
| type-closure     | Closure                                           | *reference* | |

* *-0* does not exist in the Ol syntax, but can be obtained as a result of calling a system function.
* A separate "character" type does not exist in Ol. It is the same as type-value+ due to the desire to avoid multiplying entities unnecessarily and to optimize computation speed.

