Exceptions
==========

Ol and olvm throw two classes of exceptions: *error*s and *crash*es.

*Error* is a typical error that breaks the normal execution of a program. It could be calling of a lambda with an invalid number of arguments, or trying to evaluate an non-evaluable things, or getting `modulo` for strings, etc.

*Crash* is an exceptional situation that you should never see. It could be running invalid bytecode, or reaching an out-of-memory situation, or something similarly critical.

You can't handle compile-time exceptions (e.g. `(cons 1 2 3)`), but runtime exceptions can be handled with `with-exception-handler`. You can describe the handled exception using default Ol describer named `error-object-message`.

And you can throw (with `raise`) your own exceptions if needed. These exceptions cannot be described with `error-object-message`.

TOC
---

# with-exception-handler
`(with-exception-handler handler thunk)`, *procedure*

Returns the results of invoking *thunk*. *Handler* is a lambda with excatly one argument, installed as the current exception handler in the dynamic environment used for the invocation of *thunk*.

```scheme
> (with-exception-handler
     (lambda (err)
        (print "we got exception"))
     (lambda ()
        (mod 123 "17")))
we got exception
```

# raise
`(raise obj)`, *procedure*

Raises an exception by invoking the current exception handler on *obj*.

```scheme
> (with-exception-handler
     (lambda (err)
        (print err))
     (lambda ()
        (raise "error error error")))
error error error
```

# error
`(error message obj ...)`, *procedure*

*Message* should be a string.  
Raises an exception as if by calling raise on a newly allocated object which encapsulates the information provided by *message*, as well as all *obj*.

```scheme
> (with-exception-handler
     (lambda (err)
        (print "we got error"))
     (lambda ()
        (error "some error" 1 2 3 4 5)))
we got error
```

# error-object?
`(error-object? obj)`, *procedure*

Returns *#true* if *obj* is an object created by [error](#error) (or by `runtime-error`) or raised by olvm.

```
> (with-exception-handler
     (lambda (err)
        (if (error-object? err)
           (print "this is error object")
           (print "this is not an error object")))
     (lambda ()
        (error "some error" 1 2 3 4 5)))
this is error object
```

# error-object-message
`(error-object-message error-object)`, *macro*

Returns the Ol-provided description for *error-object*. Will not describe unkown errors (i.e. user-thrown).

Require `(scheme repl)` `interaction-environment`.

```
> (import (only (scheme repl) interaction-environment))
> (define (f x y) (+ x y))
> (with-exception-handler
     (lambda (err)
        (print (error-object-message err)))
     (lambda ()
        (f)))
(error 17 -> (wrong number of arguments: 0, but #<f> expects 2))
```