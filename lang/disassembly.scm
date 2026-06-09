(define-library (lang disassembly)
   (export
      disassembly)
   (import
      (scheme base)
      (scheme bytevector)
      (owl ff)
      (owl list-extra)
      (owl io))
(begin
   (define (disassembly func)
      (define output
         (case (type func)
            (type-bytecode ; 16, direct bytecode - (lambda (x y) (cons x y))
               (define bytecode (bytevector->list func))
               {
                  'type 'bytecode ; (bytecode ...)
                  'code bytecode
                  'bytecode bytecode
               })
            (type-procedure ; 17, bytecode with external dependency - (lambda (x y) (+ x y))
               (define procedure func)
               (define bytecode (bytevector->list (ref procedure 1)))
               {
                  'type 'procedure ; (ref bytecode, ref external1, ref external2, ref external3, ...)
                  'code (vm:cast (set-ref procedure 1 bytecode) type-vector)
                  'bytecode bytecode
               })
            (type-closure ; 18, (define (make x) (lambda (y) (+ x y))), (make 7) is a closure
               (define closure func)
               (define procedure (ref closure 1))
               (define bytecode (bytevector->list (ref procedure 1)))
               {
                  'type 'closure
                  'code (vm:cast (set-ref closure 1
                                    (vm:cast (set-ref procedure 1 bytecode) type-vector))
                           type-vector)
                  'bytecode bytecode
               })))
      (if output
         (put output 'disassembly
            (let loop ((src (output 'bytecode #null)) (out #null))
               (define (DIS len name)
                  (loop (drop src len)
                        (cons
                           (append (list len name) (cdr (take src len)))
                           out)))
               (define (D name len)
                  (loop (drop src len)
                        (cons
                           (append (list len name) (cdr (take src len)))
                           out)))

               (if (null? src)
                  (reverse out)
               else
                  (print src)
                  (case (car src)
                     (#o0  (DIS 1 "ERROR"))
                     ;; (62 (DIS 1 "INVALID"))

                     (24 (DIS 2 "RET"))     (#o130 (D "WRET" 3))
                     
                     (27 (DIS 6 "MCP"))
                     (50 (DIS 3 "RUN"))

                     (51 (DIS 4 "CONS"))    (#o163 (D "WCONS" 7))
                     (15 (DIS 3 "TYPE"))
                     (36 (DIS 3 "SIZE"))

                     (#o2  (D "GOTO" 3))    (#o102 (D "WGOTO" 5))
                     (#o3  (D "CLOS" (+ (caddr src) 4))) ; todo: decode first closure argument in type

                     (#o4  (D (case (cadr src)
                                 (0 "BF") (1 "BT")
                                 (2 "BN") (3 "BE")
                                 (4 "BZ")
                                 (else "B?")) 5))
                     (#o5  (D "BEQ" 5))
                     (#o6  (D "BNA" 4))
                     (#o7  (D "BNAV" 4))


                     (54 (DIS 4 "EQ?"))
                     (44 (DIS 4 "LESS?"))

                     (#o14 (D (case (cadr src)
                                    (0 "LDF") (1 "LDT")
                                    (2 "LDN") (3 "LDE")
                                    (else "LD?"))
                              3))
                     (#o114 (D (case (cadr src)
                                    (0 "WLDF") (1 "WLDT")
                                    (2 "WLDN") (3 "WLDE")
                                    (else "WLD?"))
                              4))
                     (#o15 (D "LD8" 3))     (#o115 (D "WLD8" 4))

                     (52 (DIS 3 "CAR"))
                     (53 (DIS 3 "CDR"))
                     (47 (DIS 4 "REF"))

                     (#o1  (D "REFI" 4))    (#o101 (D "WREFI" 7))
                     (#o11 (D "MOVE" 3))    (#o111 (D "WMOVE" 5))
                     (#o12 (D "MOV2" 5))

                     (48 (DIS 4 "CAST"))
                     (43 (DIS 7 "SET!"))

                     (#o61 (DIS 5 "SETREF")) ; TODO: change
                     (#o13 (DIS 5 "SETREF!")) ; TODO: change

                     ; memory allocators
                     (#o20 (D "NEW" (if (null? (cdr src)) 1 (+ (caddr src) 5))))
                     (#o22 (D "MAKE"  (if (null? (cdr src)) 1 (+ (cadr src) 3))))
                     (#o23 (D "ALLOC" (if (null? (cdr src)) 1 (+ (cadr src) 3))))
                     ; special
                     (#o24 (D "APPLY" 1))
                     (#o25 (D "APPLY/CC" 1))
                     (#o26 (if (null? (cdr src))
                           (D "VECTOR-APPLY" 1)
                           (D "VECTOR-APPLY" (+ (caddr src) 3))))
                     (#o27 (D "FF-APPLY" 6))
                     (17 (DIS 1 "ARITY-ERROR"))


                     (38 (DIS 5 "ADD"))
                     (40 (DIS 5 "SUB"))
                     (26 (DIS 7 "DIV"))
                     (39 (DIS 5 "MUL"))

                     (55 (DIS 4 "AND"))
                     (56 (DIS 4 "IOR"))
                     (57 (DIS 4 "XOR"))
                     (58 (DIS 5 "SHR"))
                     (59 (DIS 5 "SHL"))

                     (33 (DIS 4 "FP1"))
                     (34 (DIS 5 "FP2"))

                     (42 (DIS 6 "FF:BLACK"))
                     ( 8 (DIS 6 "FF:RED"))
                     (46 (DIS 3 "FF:TOGGLE"))
                     (41 (DIS 3 "FF:RED?"))
                     (32 (DIS 3 "FF:RIGHT?"))

                     (61 (DIS 3 "CLOCK"))

                     (63 (DIS (+ (cadr src) 3) "SYSCALL"))

                     (28 (DIS 2 "VERSION"))
                     (29 (DIS 2 "FEATURES"))
                     (30 (DIS 2 "VMAX"))
                     (31 (DIS 2 "VSIZE"))

                     (35 (DIS 3 "PIN"))
                     (60 (DIS 3 "UNPIN"))
                     (25 (DIS 3 "DEREF"))

                     (37 (DIS 3 "EXIT"))

                     (else
                        (DIS "?"))))))))

))
