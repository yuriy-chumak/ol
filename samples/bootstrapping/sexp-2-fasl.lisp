#!/usr/bin/env ol
(import (scheme read))
(import (scheme cxr))

(define (encode-integer num)
   (let loop ((num num) (tail #null))
      (if (less? num 128)
         (cons (vm:cast num type-enum+) tail)
         (cons (vm:ior (band num #x7F) #x80)
               (loop (>> num 7) tail)))))

; write either a byte or a bytestream
(define (@ x)
   (if (list? x)
      (write-bytestream x)
      (write-bytestream (list x))))

(define (tag object)
   (if (ref object 0) 2 1))

(define (encode-item item)
   (cond
      ((equal? item 'NULL)
         (@ 0)                         ; not a reference
         (@ type-const)                ; type const (13)
         (@ (encode-integer #null)))   ; value
      ((equal? item 'EMPTY)
         (@ 0)                         ; not a reference
         (@ type-const)                ; type const (13)
         (@ (encode-integer #empty)))  ; value
      ((equal? item 'EOF)
         (@ 0)                         ; not a reference
         (@ type-const)                ; type const (13)
         (@ (encode-integer #eof)))    ; value
      ((equal? item 'TRUE)
         (@ 0)                         ; not a reference
         (@ type-const)                ; type const (13)
         (@ (encode-integer #true)))   ; value
      ((equal? item 'FALSE)
         (@ 0)                         ; not a reference
         (@ type-const)                ; type const (13)
         (@ (encode-integer #false)))  ; value

      ((equal? item 'STDIN)
         (@ 0)                         ; not a reference
         (@ type-port)                 ; type port (12)
         (@ (encode-integer stdin)))   ; value
      ((equal? item 'STDOUT)
         (@ 0)                         ; not a reference
         (@ type-port)                 ; type port (12)
         (@ (encode-integer stdout)))  ; value
      ((equal? item 'STDERR)
         (@ 0)                         ; not a reference
         (@ type-port)                 ; type port (12)
         (@ (encode-integer stderr)))  ; value
      ((pair? item)
         (cond
            ((equal? (car item) 'N)
               (@ (encode-integer (negate (cadr item)))))
            ((equal? (car item) 'INTEGER)
               (define value (cadr item))
               (@ 0)                                            ; not a reference
               (@ (if (positive? value) type-enum+ type-enum-)) ; type, positive (0) or negative (32)
               (@ (encode-integer (abs value))))                ; value
            (else
               (runtime-error "Unknown item" item))))
      (else
         (runtime-error "Unknown item" item))))

(define (encode-bytecode bytecode)
   (for-each (lambda (opcode)
         (@ (if (value? opcode)
               opcode
               (cond
                  ((equal? opcode 'RET) 24)
                  ((equal? opcode 'GOTO) 2)
                  ((equal? opcode 'CLOS) 3)

                  ((equal? opcode 'REFI) 1)
                  ((equal? opcode 'MOVE) 9)
                  ((equal? opcode 'MOV2) 5)

                  ((equal? opcode 'CAST) 22)
                  ((equal? opcode 'SET!) 43)

                  ((equal? opcode 'JZ)  16)
                  ((equal? opcode 'JN)  80)
                  ((equal? opcode 'JE) 144)
                  ((equal? opcode 'JF) 208)

                  ((equal? opcode 'JEQ)  8)
                  ((equal? opcode 'JAF) 11)
                  ((equal? opcode 'JAX) 12)

                  ((equal? opcode 'EQ?) 54)
                  ((equal? opcode 'LESS?) 44)

                  ((equal? opcode 'LD)  14)
                  ((equal? opcode 'LDI)  13)
                  ((equal? opcode 'LDN)  77)
                  ((equal? opcode 'LDT) 141)
                  ((equal? opcode 'LDF) 205)

                  ((equal? opcode 'CAR) 52)
                  ((equal? opcode 'CDR) 53)
                  ((equal? opcode 'REF) 47)

                  ((equal? opcode 'CONS) 51)
                  ((equal? opcode 'TYPE) 15)
                  ((equal? opcode 'SIZE) 36)

                  ((equal? opcode 'ADD) 38)
                  ((equal? opcode 'SUB) 40)
                  ((equal? opcode 'DIV) 26)
                  ((equal? opcode 'MUL) 39)

                  ((equal? opcode 'AND) 55)
                  ((equal? opcode 'IOR) 56)
                  ((equal? opcode 'XOR) 57)
                  ((equal? opcode 'SHR) 58)
                  ((equal? opcode 'SHL) 59)

                  ((equal? opcode 'FP1) 33)
                  ((equal? opcode 'FP2) 34)

                  ((equal? opcode 'FF-APPLY)   49)
                  ((equal? opcode 'FF:BLACK)   42)
                  ((equal? opcode 'FF:RED)    106)
                  ((equal? opcode 'FF:TOGGLE)  46)
                  ((equal? opcode 'FF:RED?)    41)
                  ((equal? opcode 'FF:RIGHT?) 105)

                  ((equal? opcode 'CLOCK) 61)
                  ((equal? opcode 'SYSCALL) 63)

                  ((equal? opcode 'SETREF) 10)
                  ((equal? opcode 'SETREF!) 74)

                  ((equal? opcode 'NEW) 23)
                  ((equal? opcode 'MAKE) 18)
                  ((equal? opcode 'ALLOC) 82)

                  ((equal? opcode 'MCP) 27)
                  ((equal? opcode 'RUN) 50)

                  ((equal? opcode 'APPLY) 20)
                  ((equal? opcode 'ARITY-ERROR) 17)
                  ((equal? opcode 'APPLY/CC) 84)
                  ((equal? opcode 'VECTOR-APPLY) 32)

                  ((equal? opcode 'VERSION) 28)
                  ((equal? opcode 'FEATURES) 29)
                  ((equal? opcode 'VMAX) 30)
                  ((equal? opcode 'VSIZE) 31)

                  ((equal? opcode 'PIN) 35)
                  ((equal? opcode 'UNPIN) 60)
                  ((equal? opcode 'DEREF) 25)

                  ((equal? opcode 'EXIT) 37)
                  (else
                     (runtime-error "Unknown opcode" opcode))))))
      bytecode))

(define (encode-object-as record o-type)
   (define items (cadr record))
   (@ 1)                               ; tag = object
   (@ o-type)                          ; type
   (@ (encode-integer (length items))) ; size
   (for-each encode-item items))       ; vector items

; main
(for-each (lambda (record)
      (define what (car record))
      (cond
         ; (STRING "display")
         ((equal? what 'STRING)
            ; decode string
            ; strings encoded using '2 for " character and '1 for '.
            (define str (s/'1/'/g (s/'2/"/g (cadr record)))) ;"

            (case (type str)
               ; ansi strings, no encoding required
               (type-string
                  (@ (tag str))                            ; tag = raw
                  (@ type-string)                          ; type = 3
                  (@ (encode-integer (string-length str))) ; size
                  (@ (string->list str)))                  ; string itself

               ; unicode string, should write array of integers
               (type-string-wide
                  (@ 1)                                    ; tag = object
                  (@ type-string-wide)                     ; type = 5
                  (@ (encode-integer (string-length str))) ; size
                  (lfor-each (lambda (rune)                ; string itself
                        (@ 0)                              ;   not a reference
                        (@ (type rune))                    ;   type enum+ (0)
                        (@ (encode-integer rune)))         ;   rune itself
                     (str-iter str)))
               (else
                  (runtime-error "Invalid string value" record))))
         
         ; (SYMBOL ((N x)))
         ((equal? what 'SYMBOL)
            (define x (car (cdaadr record)))
            (@ 1)                                          ; tag = object
            (@ type-symbol)                                ; type = 4
            (@ 1)                                          ; size = 1
            (@ (encode-integer (negate x))))               ; reference to a name of symbol

         ; (INEXACT (0 0 0 0 0 0 240 127))
         ((equal? what 'INEXACT)
            (define fpvalues (cadr record))
            (@ 2)                                          ; tag = raw
            (@ type-inexact)                               ; type = 44
            (@ (encode-integer (length fpvalues)))         ; size
            (@ fpvalues))                                  ; inexact value

         ; (BYTECODE ((JAF 3 0 15) (REFI 1 2 6) (MOVE 5 8) (MOV2 4 5 8 4) (GOTO 6 3) (ARITY-ERROR)))
         ((equal? what 'BYTECODE)
            (define bytecodes (cadr record))
            (@ 2)                                          ; tag = raw
            (@ type-bytecode)                              ; type = 16
            (@ (encode-integer                             ; size
                  (fold + 0 (map length bytecodes))))
            (for-each encode-bytecode bytecodes))          ; bytecodes itself

         ; (PAIR ((INTEGER 1) NULL))
         ((equal? what 'PAIR)
            (encode-object-as record type-pair))           ; type = 1

         ; (PROCEDURE ((N -3556) (N -2)))
         ((equal? what 'PROCEDURE)
            (encode-object-as record type-procedure))      ; type = 17

         ; CLOSURE, same as procedure, but closure
         ((equal? what 'CLOSURE)
            (encode-object-as record type-closure))        ; type = 18

         ; (RATIONAL ((INTEGER 1) (INTEGER 100000000)))
         ((equal? what 'RATIONAL)
            (encode-object-as record type-rational))       ; type = 42

         ; (VECTOR ((N -6574) (INTEGER 52) (INTEGER 1) (INTEGER 1) (N -3847)))
         ((equal? what 'VECTOR)
            (encode-object-as record type-vector))         ; type = 2

         ; ff with internal types
         ; (RED ((N -5954) (N -3)))
         ((equal? what 'RED)
            (encode-object-as record 26))                  ; type = 26
         ; (BLACK-RIGHT ((N -6545) (N -42) (N -1)))
         ((equal? what 'BLACK-RIGHT)
            (encode-object-as record 25))                  ; type = 25
         ; (BLACK ((N -5272) (N -19)))
         ((equal? what 'BLACK)
            (encode-object-as record 24))                  ; type = 24

         ; (CONSTRUCTOR ((N -1)))
         ((equal? what 'CONSTRUCTOR)
            (encode-object-as record 63))                  ; type = 24


         (else
            (runtime-error "Unhandled record" record))))
   (read))
(display "\0")
