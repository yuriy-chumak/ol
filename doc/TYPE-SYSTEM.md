# Otus Lisp Type System

## reference & value

The Otus Lisp Virtual Machine (Olvm) works with two data classes: *reference* and *value*. These names reflect the method of passing corresponding entities through the Ol virtual machine pipeline.

The *value* class represents small numbers and constants that fit entirely within a virtual machine register and are always passed by value (between registers, as argument or result values of functions, as components of lists or vectors, etc.). When passed, the connection between the original value and the passed value is broken.

All other entities belong to the *reference* class and are passed only using references to their actual location in the heap. If you modify the value of a copy of such an object using a mutator, you will also modify the original object.

## types

At the computation level, entities in Ol are further divided into types. This division includes constants, numbers, strings, pairs, vectors, and so on. Types influence how a specific value is interpreted by the evaluator. For example, the length of a string can be computed for a string but not for a number (although a number can be converted to a string, and then its length can be computed). The type of an entity can be obtained using the basic `type` function, and the type name can be obtained from its numerical value using `typename`.

Here are some examples of obtaining and interpreting types:
```scheme
(type 123)         ==>  0
(type -123)        ==> 32
(typename 32)      ==> 'type-value-
(type "123")       ==>  3
(typename 3)       ==> 'type-string
```

For convenience and readability of program code, specialized functions (predicates) can be used to simply check if an entity belongs to a specific type, such as `boolean?`, `string?`, and so on.

```scheme
(string? 123)      ==> #false
(string? "123")    ==> #true
```

There is no automatic type conversion in Ol, so be careful.

The following table provides a detailed list of type names, descriptions of the values of each type, and the global classes (*reference* and *value*) to which these types belong.

| typename           | description                                         | class       | comment |
|--------------------|-----------------------------------------------------|-------------|---------|
| *type-value+*      | Positive number that fits within a *value*          | *value*     | [0 .. +16777215] or [0 .. +72057594037927935] depending on the bitness of the target platform |
| *type-value-*      | Negative number that fits within a *value*          | *value*     | [-16777215 .. -1] or [-72057594037927935 .. -1] depending on the bitness of the target platform |
| *type-integer+*    | Positive number that does NOT fit within a *value* | *reference* | [+72057594037927936 .. ∞], the actual limit depends on the available RAM |
| *type-integer-*    | Negative number that does NOT fit within a *value* | *reference* | [-∞ .. -72057594037927936], the actual limit depends on the available RAM |
| *type-rational*    | Rational number, a numerator/denominator pair      | *reference* | always in the form of an irreducible fraction |
| *type-complex*     | Complex number, a real and imaginary part pair      | *reference* | |
| *type-inexact*     | Inexact number, a floating-point number            | *reference* | the precision depends on the target platform and build options of olvm |
| *type-pair*        | Dot pair                                            | *reference* | |
| *type-vector*      | Vector                                              | *reference* | |
| *type-symbol*      | Symbol                                              | *reference* | |
| *type-bytevector*  | Bytevector                                          | *reference* | |
| *type-string*      | ANSI string                                         | *reference* | each character is one byte |
| *type-string-wide* | Unicode string                                      | *reference* | each character (rune in Ol terminology) is *value* in size |
| *type-superstring* | Composite "smart" string                            | *reference* | is a concatenation of other strings, possibly a mix of ANSI and Unicode |
| *type-const*       | Language constant                                   | *value*     | *#true*, *#false*, *#null*, etc. |
| *type-bytecode*    | Virtual machine bytecode                            | *reference* | |
| *type-procedure*   | Procedure, Function                                 | *reference* | |
| *type-closure*     | Closure                                             | *reference* | |

* A separate type "character" does not exist in Ol. It is the same as *type-value+* due to the desire to avoid multiplying entities unnecessarily and to optimize computation speed.

