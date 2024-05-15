# FFI olvm Extension

FFI (Foreign Function Interface) is an OLVM (Otus Lisp Virtual Machine) extension
by which a program written in Ol (Otus Lisp) can call routines or make use of services
provided by the OS (Operation System) or third-party libraries.

For example, you can use Sqlite library without including support for it directly in olvm nor writing any C code.

### Glossary

* `value` is an integer number fits in olvm word (machine word minus 8 bits),
  - -16777215 .. +16777215 integers for 32-bit machines,
  - -72057594037927935 .. +72057594037927935 integers for 64-bit machines,
  - #true, #false, #null, other olvm constants.
* `reference` is any other Ol object (big and floating point numbers, strings, vectors, functions, etc.),
* `callable` is a special formed Ol function that can be called from the external (native) code.

We use some common names for types in tables:
  * `integer-types` means type-enum+, type-enum-, type-int+, and type-int-.
  * `number-types` means integer-types, type-rational, type-inexact, and type-complex.
  * `string-types` means type-string, type-string-wide, and type-string-dispatch.
  * `structure-type` means a list (possibly with nested lists) of structure types in consistent order.

You can get the type of any Ol object using `type` function (`(type -12)` produces 32),
you can get type name for any type value using `typename` function (`(typename 32)` produces 'type-enum-).

### Limitations

* no *half precision floating point* support yet,
* no *quad-word integer* support yet,
* no *unions* in *structures* support yet.

## TOC
* [Basic example](#basic-example)
* [Function declaration](#function-declaration)
* [Function result types](#function-result-types)
  * [Integer types](#integer-types)
  * [Floating point types](#floating-point-types)
  * [String types](#string-types)
  * [Other cases](#other-cases)
* [Argument Types](#argument-types)
  * [Integer types](#integer-types-1)
  * [Floating point types](#floating-point-types-1)
  * [String types](#string-types-1)
  * [Pointer types](#pointer-types)
  * [Other cases](#other-cases-1)
* [C Types Default Mapping](#c-types-default-mapping)
* [Advanced usage](#advanced-usage)
  * [Variable Arguments](#variable-arguments)
  * [Multiplatform](#multiplatform)
  * [More Examples](#more-examples)


## Basic Example

If you want to import function(s) from the native dynamic library, follow these steps:

1. Import the ffi library: `(import (otus ffi))`,
1. Load a native dynamic library: `(define libm (load-dynamic-library "libm.so.6"))`,  
   (I assume you use kind of Linux or any Unix flavor, for Windows type "user32.dll" library name).  
   Now "libm" became a function loader for "libm.so" shared library.
1. Link an external function: `(define asin (libm fft-double "asin" fft-double))`,
1. Use external function in a regular way: `(print (asin 0.5))`

Easy, huh?

All-in-one steps demo:
```scheme
$ ol
Welcome to Otus Lisp 2.3.1-3172-43e20773
type ',help' to help, ',quit' to end session.
> (import (otus ffi))
;; Library (otus ffi) added
;; Imported (otus ffi)
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


## Function declaration

Function declaration (the linking occurs at the time of declaration, so library should be loaded and valid at this point) consists of:
1. the library (`libm` in the example above),
1. the data type of the value the function returns in terms of ffi (first `fft-double` in the example above),
1. the function name (`"asin"` in the example above),
1. the function parameters, if any (`fft-double` in the example above).


## Function result types

List of function returning types that ffi supports, with a short description:

### Integer types:

| Result Type     | FFI Type   | Possible Ol Result Types | Comments |
|-----------------|------------|--------------------------|----------|
| signed 8 bit    | fft-int8   | type-enum+, type-enum- | |
| unsigned 8 bit  | fft-uint8  | type-enum+ | |
| signed 16 bit   | fft-int16  | type-enum+, type-enum- | |
| unsigned 16 bit | fft-uint16 | type-enum+ | |
| signed 32 bit   | fft-int32  | type-enum+, type-enum-, type-int+ (only 32-bit machines), type-int- (only 32-bit machines) | |
| unsigned 32 bit | fft-uint32 | type-enum+, type-int+ (only 32-bit machines) | |
| signed 64 bit   | fft-int64  | type-enum+, type-enum-, type-int+, type-int- | |
| unsigned 64 bit | fft-uint64 | type-enum+, type-int+ | |

### Floating Point Types

| Result Type     | FFI Type   | Possible Ol Result Types | Comments |
|-----------------|------------|--------------------------|----------|
| 32-bit float    | fft-float  | type-inexact | `+nan.0`, `-inf.0`, `+inf.0` are valid results |
| 64-bit float    | fft-double | type-inexact | `+nan.0`, `-inf.0`, `+inf.0` are valid results |

### String Types

| Result Type     | FFI Type    | Possible Ol Result Types | Comments |
|-----------------|-------------|--------------------------|----------|
| ansi string     | type-string | type-string, type-string-wide | Depends on ansi/unicode result type |
| unicode string  | type-string-wide | type-string-wide | Means 16-bit wide characters string |
| utf-8 string    | type-string | type-string, type-string-wide | Depends on ansi/unicode result type |

### Other Cases

| Result Type     | FFI Type  | Possible Ol Result Types | Comments |
|-----------------|-----------|--------------------------|----------|
| void            | fft-void  | type-const | Always returns `#true` |
| port            | type-port | type-port (value or reference) | |
| boolean         | fft-bool  | type-const | Returns `#false` for a zero result, otherwise `#true` |
| any pointer     | type-vptr | type-vptr | |
| structure       | type-pair | type-pair | Note 3 below. |


## Argument Types

List of argument types that ffi supports is wider than return types and include pointers, callables, and bytevectors.
Also, the `#false` can be used as the "default" for any argument (typically as 0).

List of argument types with a short description:

### Integer types:

| Parameter Type    | FFI Type     | Possible Ol Parameter Types | Comments |
|-------------------|--------------|-----------------------------|----------|
| (un)signed 8 bit  | fft-(u)int8  | integer-types | Value truncated onto 8 bit |
|                   |              | type-rational, type-inexact | Value truncated onto 8 bit, not rounded! |
|                   |              | type-complex | Real part used with rules above. Imaginary part discarded. |
| (un)signed 16 bit | fft-(u)int16 | integer-types | Value truncated onto 16 bit |
|                   |              | type-rational, type-inexact | Value truncated onto 16 bit, not rounded! |
|                   |              | type-complex | Real part used with rules above. Imaginary part discarded. |
| (un)signed 32 bit | fft-(u)int32 | integer-types | Value truncated onto 32 bit |
|                   |              | type-rational, type-inexact | Value truncated onto 32 bit, not rounded! |
|                   |              | type-complex | Real part used with rules above. Imaginary part discarded. |
| (un)signed 64 bit | fft-(u)int64 | integer-types | Value truncated onto 64 bit |
|                   |              | type-rational, type-inexact | Value truncated onto 64 bit, not rounded! |
|                   |              | type-complex | Real part used with rules above. Imaginary part discarded. |

### Floating Point Types

| Parameter Type  | FFI Type   | Possible Ol Parameter Types | Comments |
|-----------------|------------|-----------------------------|----------|
| 32-bit float    | fft-float  | integer-types, type-rational, type-inexact | Exact numbers may lose exactness. |
|                 |            | type-complex | Real part used with rules above. May lose exactness if exact. |
| 64-bit float    | fft-double | integer-types, type-rational, type-inexact | Exact numbers may lose exactness. |
|                 |            | type-complex | Real part used with rules above. May lose exactness if exact. |

### String Types

| Parameter Type  | FFI Type    | Possible Ol Parameter Types | Comments |
|-----------------|-------------|-----------------------------|----------|
| ansi string     | type-string | string-types, type-symbol | Unicode strings encoded as utf-8 |
| unicode string  | type-string | string-types, type-symbol | Means 16-bit wide characters string |
| utf-8 string    | type-string | string-types, type-symbol | |

### Pointer Types

Ffi accepts two pointer types named `pointers` and `references`.
Only difference that references reflect their changes during the execution of foreign function.
Pointers remain unchanged even if the foreign function changes their value. References are slower.

Note: References can fit only applicable values. For example, if you pass '(#i1 2 #3) as (fft* fft-float) to the
c-function `void function(float* p) { for (int i = 0; i < 3; i++) p[i]*=2;}` you'll get error. But `(#i1 #i2 #3) will be ok.

Pointer declared using function `fft*`, reference using `fft&`.
Some pointer/reference types already defined in (otus ffi), `fft-int8*` as `(fft* fft-int8)` for example.

| Parameter Type      | FFI Type    | Possible Ol Parameter Types | Comments |
|---------------------|-------------|-----------------------------|----------|
| (un)signed 8 bit *  | fft-(u)int8*, fft-(u)int8&   | list of number-types | Value conversion same as for integer types |
| (un)signed 16 bit * | fft-(u)int16*, fft-(u)int16& | list of number-types | Value conversion same as for integer types |
| (un)signed 32 bit * | fft-(u)int32*, fft-(u)int32& | list of number-types | Value conversion same as for integer types |
| (un)signed 64 bit * | fft-(u)int64*, fft-(u)int64& | list of number-types | Value conversion same as for integer types |
| 32-bit float *      | (fft* fft-float), (fft& fft-float)  | list or vector of number-types  | Value conversion same as for integer types |
| 32-bit float *      | (fft* fft-float), (fft& fft-float)  | type-bytevector  | Deprecated. |
| 64-bit float *      | (fft* fft-double) | list of number-types            | Value conversion same as for integer types |
| system pointer *    | (fft* type-vptr) | list of type-vptr or string-types | |
| system pointer *    | (fft* type-vptr) | list of string-types | Deprecated |
| string pointer *    | (fft* type-vptr) | list of string-types | |

### Other Cases

| Parameter Type   | FFI Type    | Possible Ol Parameter Types | Comments |
|------------------|-------------|-----------------------------|----------|
| void             | fft-void    | any | argument ignored, always interpret as 0 |
| system pointer   | type-vptr   | type-vptr, type-bytevector | Note 2.|
| port             | type-port   | type-port | |
| boolean          | fft-bool    | type-const | `#false` interpreted as false, all others as `#true` |
| any              | fft-any     | any | Note 1. |
| raw olvm pointer | fft-unknown | any | Don't use, because for internal purposes and can be changed at any moment. |
| internal pointer | type-bytevector | type-bytevector, type-string | Deprecated |
| structure        | type-pair   | structure-type | Note 3. |

* Note 1: `fft-any` interpret argument depending on argument type with rules:
  * integer-types interpret as 32-bit integer for 32-bit machine, 64-bit integer for 64-bit machine,
  * string-types interpret as utf8 string,
  * type-vptr interpret as system pointer,
  * type-callable interpret as ol callable (platform function pointer),
  * dot pair '(type . argument) as type.
* Note 2. Bytevectors interpret as pointer to this bytevector. Please don't use because of risky.
* Note 3. Structures TBD a bit later.


## C Types Default Mapping

For the 99% of function declarations you can use c-like ffi types freely.
This is the table of correspondence C types to Ol types, with the internal type mapping noted.

| C Typename         | Ol FFI Typename    | Mapping Type | Comments |
|--------------------|--------------------|--------------|----------|
| char (signed)      | fft-char           | fft-int8 | char in (otus ffi) is signed by default |
| signed char        | fft-signed-char    | fft-int8 | |
| unsigned char      | fft-unsigned-char  | fft-uint8 | |
| short              | fft-short          | fft-int16 | |
| short int          | fft-short          | fft-int16 | |
| signed short       | fft-short, fft-signed-short | fft-int16 | |
| signed short int   | fft-short, fft-signed-short | fft-int16 | |
| unsigned short     | fft-unsigned-short | fft-uint16 | |
| unsigned short int | fft-unsigned-short | fft-uint16 | |
| int                | fft-int            | fft-int32 | |
| signed             | fft-signed-int     | fft-int32 | |
| signed int         | fft-signed-int     | fft-int32 | |
| unsigned           | fft-unsigned-int   | fft-uint32 | |
| unsigned int       | fft-unsigned-int   | fft-uint32 | |
| long               | fft-long           | fft-int32 | all 32-bit machines, 64-bit windows |
| long               | fft-long           | fft-int64 | all 64-bit machines, except windows |
| long int           | fft-long           | fft-int32 | all 32-bit machines, 64-bit windows |
| long int           | fft-long           | fft-int64 | all 64-bit machines, except windows |
| signed long        | fft-signed-long    | fft-int32 | all 32-bit machines, 64-bit windows |
| signed long        | fft-signed-long    | fft-int64 | all 64-bit machines, except windows |
| signed long int    | fft-signed-long    | fft-int32 | all 32-bit machines, 64-bit windows |
| signed long int    | fft-signed-long    | fft-int64 | all 64-bit machines, except windows |
| unsigned long      | ffr-unsigned-long  | fft-uint32 | all 32-bit machines, 64-bit windows |
| unsigned long      | ffr-unsigned-long  | fft-uint64 | all 64-bit machines, except windows |
| unsigned long int  | ffr-unsigned-long  | fft-uint32 | all 32-bit machines, 64-bit windows |
| unsigned long int  | ffr-unsigned-long  | fft-uint64 | all 64-bit machines, except windows |
| long long              | fft-long-long      | fft-int64 | |
| long long int          | fft-long-long      | fft-int64 | |
| signed long long       | fft-signed-long-long   | fft-int64 | |
| signed long long int   | fft-signed-long-long   | fft-int64 | |
| unsigned long long     | fft-unsigned-long-long | fft-uint64 | |
| unsigned long long int | fft-unsigned-long-long | fft-uint64 | |
| | | |
| float       | fft-float  | fft-float  | |
| double      | fft-double | fft-double | |
| long double | no         | no         | |
| | | |
| _Bool       | fft-bool   | fft-bool   | |
| size_t      | fft-size_t | fft-int32  | all 32-bit machines |
| size_t      | fft-size_t | fft-int64  | all 64-bit machines |
| ptrdiff_t   | fft-size_t | fft-int32  | all 32-bit machines |
| ptrdiff_t   | fft-size_t | fft-int64  | all 64-bit machines |
| enum        | fft-enum   | fft-int    | |


## Advanced usage

### Variable Arguments

Simply push additional arguments in form `'(type . argument)`. Any "extra" arguments got type "fft-any".
```scheme
> (import (otus ffi))
> ;; Library (otus ffi) added
> ;; Imported (otus ffi)
> (define libc (load-dynamic-library "libc.so.6"))
;; Defined libc
> (define printf (libc fft-int "printf" type-string))
;; Defined printf

> (printf "%c\n" (cons fft-char #\A))
A
2
> (let*((a 17)
        (b 42)
        (c (+ a b)))
     (printf "%d + %d = %d\n" (cons fft-int a) (cons fft-int b) (cons fft-int c)))
17 + 42 = 59
13
> (printf "%s\n" (cons type-string "hello!"))
hello!
7
> (printf "%f\n" (cons fft-double 123.456))
123.456000
11
```

Note: for the integers, strings and floats you can omit type declaration in form of dot-pair.

### Multiplatform

An OS-independent solution requires loading different libraries for different OSes.
You can do this with `or`, for example:
```scheme
(define libm (or
   (load-dynamic-library "libm.so.6")
   (load-dynamic-library "user32.dll")))
(define asin (libm fft-double "asin" fft-double))
```

But I recommend creating a comprehensive solution:
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
         (runtime-error "Unsupported platform" (syscall 63)))))
(begin
   (define asin (libm fft-double "asin" fft-double))
))
```

Then very simple usage like any other ol functions under all supported OS:
```scheme
(import (mylib))
(print (asin 0.5))
```

### More Examples

Examples in form of "C declaration, Ol declaration, maybe usage example(s)".
Both Linux, *BSD, macOS, and Windows are supported.

```scheme
(import (otus ffi))
(define SO (or (load-dynamic-library "libsqlite3.so")
               (load-dynamic-library "libsqlite3.dylib")
               (load-dynamic-library "sqlite3.dll")))
(unless SO
   (runtime-error "Sqlite3 error:" "library not found"))


; int sqlite3_libversion_number(void);
(define sqlite3_libversion_number (SO fft-int "sqlite3_libversion_number"))

(print "sqlite3 version number: " (sqlite3_libversion_number))


; const char *sqlite3_libversion(void);
(define sqlite3_libversion (SO type-string "sqlite3_libversion"))

(print "sqlite3 version: " (sqlite3_libversion))


; typedef struct sqlite3 sqlite3
; int sqlite3_open(
;   const char *filename,   /* Database filename (UTF-8) */
;   sqlite3 **ppDb          /* OUT: SQLite db handle */
; );
(define sqlite3_open (SO fft-int "sqlite3_open" type-string fft-void**))

(define database (make-vptr)) ; (make-vptr) is an Ol function to make empty vptr
(sqlite3_open "database.b" database)


; typedef struct sqlite3_str sqlite3_str;
; sqlite3_str *sqlite3_str_new(sqlite3*);
(define sqlite3_str_new (SO type-vptr "sqlite3_str_new" type-vptr))

(define str (sqlite3_str_new database))


; void sqlite3_str_appendf(sqlite3_str*, const char *zFormat, ...);
(define sqlite3_str_appendf (SO fft-void "sqlite3_str_appendf" type-vptr type-string))

(sqlite3_str_appendf str "int: %d, string: [%s], float: %f"
   (cons fft-int 123456)             ; all additional function arguments should be sent
   (cons type-string "I'm a string") ;     using dot-pair '(type . value)
   (cons fft-double 123.456))        ; '%f' receives doubles, not floats


; char *sqlite3_str_finish(sqlite3_str*);
(define sqlite3_str_finish (SO type-string "sqlite3_str_finish" type-vptr))

(print (sqlite3_str_finish str))

;; ------------------------------------------------------------------------
;; C->Ol callback example
;; provides SHA1 function
(import (lib sha1))

; SQLITE_API int sqlite3_create_function_v2(
;   sqlite3 *db,
;   const char *zFunctionName,
;   int nArg,
;   int eTextRep,
;   void *pApp,
;   void (*xFunc)(sqlite3_context*,int,sqlite3_value**),
;   void (*xStep)(sqlite3_context*,int,sqlite3_value**),
;   void (*xFinal)(sqlite3_context*),
;   void(*xDestroy)(void*)
; );
(define sqlite3_create_function_v2 (SO fft-int "sqlite3_create_function_v2" type-vptr type-string fft-int fft-int
   type-vptr type-callable type-callable type-callable type-vptr))

; typedef struct sqlite3_value sqlite3_value;
; unsigned char *sqlite3_value_text(sqlite3_value*);
(define sqlite3_value_text (SO type-string "sqlite3_value_text" type-vptr))

; typedef struct sqlite3_context sqlite3_context;
; void sqlite3_result_text(sqlite3_context*, const char*, int, void(*)(void*));
(define sqlite3_result_text (SO fft-void "sqlite3_result_text" type-vptr type-string fft-int type-callable))

; void (*xFunc)(sqlite3_context*,int,sqlite3_value**)
; callback is a dot-pair '(types . lambda)
;   where types is a dot-pair (return-type . list-of-argument-types)
(define calculate (vm:pin (cons             ; this pair should be pinned, 
                                            ; so it will not be moved during possible garbage collecting
   ; return-type
   (cons fft-int
         ;     sqlite3_context* int     sqlite3_value**
         (list type-vptr        fft-int (list type-vptr)))
   ; function
   (lambda (context argc argv)
      (define v (sqlite3_value_text (car argv)))
      (define r (base64:encode (sha1:digest v)))
      (sqlite3_result_text context r -1 #false))
)))

; actually create a callback
(define xFunc (make-callback calculate))

; use it
(define SQLITE_UTF8 1)
(sqlite3_create_function_v2 database "SHA1" 1 SQLITE_UTF8 #f xFunc #f #f #f)

```

More examples can be found in:
* FFI autotest (the [ffi.c](../tests/ffi.c) and [ffi.scm](../tests/ffi.scm)) source files,
  * the [ffi.scm.ok](../tests/ffi.scm.ok) is etalon test output.
* Sqlite3 library interface in the [sqlite.scm](../libraries/lib/sqlite.scm),
* OpenGL library interface in the [version-1-0](../libraries/OpenGL/version-1-0.scm),
* Lua library interface in the [lua.scm](../libraries/lib/lua.scm),
* [etc.](../libraries/lib/).