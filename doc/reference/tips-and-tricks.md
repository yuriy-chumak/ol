Tips and Tricks
===============
1. How to assemble a string from parts in the nicest way?
1. How to break a recursion or nested loop in the nicest and functional way?
1. How to handle errors at runtime?
1. How to use default values for a function arguments in easiest way?
1. How to find a function if you only remember part of its name?
1. How to split a strings by "," for example?
1. How to declare and use variables like in imperative languages, but without any hacks?
1. I need a hack to change the value of received function argument without returning new value as a function result.

Answers
-------

1. How to assemble a string from parts in the nicest way?
   * Note: this is not a functional way! For a functional way, use conventional `string-append`.
   ```scheme
   ; use open-output-string and get-output-string functions:
   (let ((buffer (open-output-string)))
      (for-each (lambda (i)
            (display-to buffer "0x")
            (display-to buffer (number->string i 16))
            (display-to buffer ", "))
         (append
            (list 1 2 3 4 5)
            (string->list "Hello")))
      (display-to buffer "0")

      (get-output-string buffer))
   ==> "0x1, 0x2, 0x3, 0x4, 0x5, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0"
   ```

1. How to break a recursion or nested loop in the nicest and functional way?
   * Use current-continuation (`call/cc` is a short for `call-with-current-continuation`).
   ```scheme
   ; a stupid way to factorize a 1633 :)
   (call/cc (lambda (return)
      (let loop ((n 2)) ; infinite loop
         (let subloop ((m 2))
            (unless (= m n)
               (if (= (* m n) 1633)
                  (return (cons m n))) ; breaks the loop
               (subloop (+ m 1))))
         (loop (+ n 1)))))
   ==> '(23 . 71)
   ```

1. How to handle errors at runtime?
   * Use `with-exception-handler`.
   ```scheme
   > (with-exception-handler
        (lambda (reason)
           "looks like you'r trying to get a car of a non pair?")
        (lambda ()
           (car 7)))
   "looks like you'r trying to get a car of a non pair?"

   ; you can fire an error manually using `raise` or `runtime-error` functions
   > (define (square-root x)
        (with-exception-handler
           (lambda (reason)
              (list "error:" reason))
           (lambda ()
              (if (< x 0)
                 (raise 'sqrt-error)
                 (sqrt x)))))

   > (square-root 4)
   2

   > (square-root -4)
   '("error:" sqrt-error)
   ```


1. How to use default values for a function arguments in easiest way?
   * Use `case-apply`.
   ```scheme
   ; make a sequence of numbers
   ; (1 is a default value to step)
   > (define seq
        (define (seq count step)
           (iota count 1 step))
        (case-lambda
           ((count step) (seq count step))
           ((count)      (seq count 1))))
   
   (seq 5)    ==>  '(1 2 3 4 5)
   (seq 5 2)  ==>  '(1 3 5 7 9)
   ```

1. How to find a function if you only remember part of its name?
   * Use `,find` repl command.
   ```
   > ,find rev
   '(rrev str-rev reverse)

   > ,find file->
   '(file->string file->bytestream file->bytevector file->list)
   ```

1. How to split a strings by "," for example?
   * Use builtin regexps ([a lot of examples](https://github.com/yuriy-chumak/ol/blob/master/tests/regex.scm) in the tests folder).
   ```scheme
   (c/,/ "a,b,c,hallo,z")  ==>  '("a" "b" "c" "hallo" "z")
   (c/-/ "2-12-1933")      ==>  '("2" "12" "1933")
   ```

1. How to declare and use variables like in imperative languages, but without any hacks?
   * Use `make-parameter` function from `(scheme dynamic-bindings)` library.
   ```scheme
   > (import (scheme dynamic-bindings))
   > (define m (make-parameter 0))
   > (for-each (lambda (x)
           (if (integer? (sqrt x)) (m x)))
        '(2 3 4 5 6 7))
   > (m)
   4
   ```

1. I need a hack to change the value of received function argument without returning new value as a function result.
   * I don't recommend any hacks, but.. pass an argument boxed in list and use `set-car!` function (works only with *symbols* and *value*s!).
   ```scheme
   > (define x (list 123))
   > (car x)
   123

   > (define (f a)
        (set-car! a (+ (car a) 5000)))
   > (f x)
   > (car x)
   5123

1. 

...
