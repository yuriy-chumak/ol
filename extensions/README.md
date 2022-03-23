# FFI Extension

* [Basic example](#basic-example)
* [Advanced usage](#advanced-usage)
* [Function declaration](#function-declaration)
* [Type system](#type-system)
  * [Value type system](#value-type-system)
  * [Reference type system](#reference-type-system)


## Basic Example

If you want to import some functions from the system library, follow this steps:

1. Import ol ffi library: `(import (otus ffi))`
1. Load a system dynamic library: `(define libm (load-dynamic-library "libm.so.6"))`
1. Declare external function: `(define asin (libm fft-double "asin" fft-double))`
1. Use external function as a regular: `(print (asin 0.5))`

Easy, huh?

All steps demo:
```scheme
$ ol
Welcome to Otus Lisp 2.3.1-3172-43e20773
type ',help' to help, ',quit' to end session.
> (import (otus ffi))
> ;; Library (otus ffi) added
> ;; Imported (otus ffi)
> (define libm (load-dynamic-library "libm.so.6"))
;; Defined libm
> (define asin (libm fft-double "asin" fft-double))
;; Defined asin
> (print (asin 0.5))
0.523598775
#true
> ,quit
bye-bye.
```


## Advanced usage

An OS-independent solution requires loading different libraries for different OSes. You can do this with `or`, for example:
```scheme
(define libm (or
   (load-dynamic-library "libm.so.6")
   (load-dynamic-library "user32.dll")))
(define asin (libm fft-double "asin" fft-double))
```

But I recommend creating a complex solution:
```scheme
(define-library (mylib)
(import
   (scheme core)
   (otus ffi))
(export
   asin)
(cond-expand
   ((or Linux Android)
      (begin
         (define libm (load-dynamic-library "libm.so.6"))))
   (Windows
      (begin
         (define libm (load-dynamic-library "kernel32.dll"))))
   (else
      (begin
         ; (syscall 63) is a `uname` function
         (runtime-error "Unsupported OS" (syscall 63)))))
(begin
   (define asin (libm fft-double "asin" fft-double))
))
```

Then very simple usage like any other ol functions:
```scheme
(import (mylib))
(print (asin 0.5))
```


## Function Declaration

Every external function declared using *return-type*, *function-name*, and list of *type-of* arguments (if any):
`(libm return-type "function-name" type-of-first-argument type-of-second-argument ...)`.


## Type System

### Value Type System

* `"C" Type` - native type of imported function. In terms of "ANSI C" language.
* `FFI Type` - equivalent type in terms of ol ffi.
* `Applicable LISP Types` - which ol objects can be safely marshalled to and from an imported function.
  * note: void (fft-void) is only the type of function result (means no result is expected).

|          "C" Type           |               FFI Type              |                    Applicable LISP Types                    |
| :-------------------------- | :---------------------------------- | :---------------------------------------------------------: |
| void                        | fft-void                            |                              -                              |
| char                        | fft-char                            | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to long) |
| signed char                 | fft-signed-char                     | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| unsigned char               | fft-unsigned-char                   | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| short, signed short         | fft-short, fft-signed-short         | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| unsigned short              | fft-unsigned-short                  | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| int, signed int             | fft-int, fft-signed-int             | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| unsigned int                | fft-unsigned-int                    | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| long, signed long           | fft-long, fft-signed-long           | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| unsigned long               | fft-unsigned-long                   | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| long long, signed long long | fft-long-long, fft-signed-long-long | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| unsigned long long          | fft-unsigned-long-long              | type-enum+, type-enum-, type-int+, type-int-, type-rational (truncation to int) |
| float                       | fft-float                           | type-enum+, type-enum-, type-int+, type-int-, type-rational, type-complex, type-inexact |
| double                      | fft-double                          | type-enum+, type-enum-, type-int+, type-int-, type-rational, type-complex, type-inexact |

#### Exact bitwise types are available.

| "C" Type | FFI type   | Applicable LISP Types                                       |
| :------- | :--------- | :---------------------------------------------------------- |
| int8_t   | fft-int8   | type-enum+, type-enum-, type-int+, type-int-, type-rational |
| uint8_t  | fft-uint8  | type-enum+, type-enum-, type-int+, type-int-, type-rational |
| uint16_t | fft-uint16 | type-enum+, type-enum-, type-int+, type-int-, type-rational |
| uint16_t | fft-uint16 | type-enum+, type-enum-, type-int+, type-int-, type-rational |
| uint32_t | fft-uint32 | type-enum+, type-enum-, type-int+, type-int-, type-rational |
| uint32_t | fft-uint32 | type-enum+, type-enum-, type-int+, type-int-, type-rational |
| uint64_t | fft-uint64 | type-enum+, type-enum-, type-int+, type-int-, type-rational |
| uint64_t | fft-uint64 | type-enum+, type-enum-, type-int+, type-int-, type-rational |

#### Special ffi types are available.


| "C" Type | FFI type  | Applicable LISP Types |
| :------- | :-------- | :-------------------- |
|          | fft-any   | ... |
|          | fft-void* | ... |


### Reference Type System (TBD.)

Pointers are also supported by ffi marshaller. Ol prepared a native raw array filled with requested type.
Ol may marshal pointers using two different ways: just as a pointer or as a reference. What that's mean:
  * as a pointer `(ffi* fft-xxx)`: any change of this array data by the callee side will have no effects to the Ol side
  * as a reference `(ffi& fft-xxx)`: any change of this array data by the callee will change original Ol array

As a pointer can be sent lists and vectors.

TBD.

| "C" Type                            | FFI type    | Applicable LISP Types    | Notes |
| :---------------------------------- | :---------- | :----------------------- | :---- |
| char*, signed char*, unsigned char* | type-vector | type-vector, type-string | UNSAFE! Pass a raw pointer to the internal memory |
| char*, signed char*, unsigned char* | type-string | type-string, type-string-wide, type-string-dispatch | Correctly encode and decode utf-8 strings |

TBD.
