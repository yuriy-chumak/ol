;; vm primops

;; todo: convert tuple to variable arity
;; todo: convert arity checks 17 -> 25

;; todo: переделать так, чтобы регистр возврата был самый первый, тогда можно будет обойтись без RET

(define-library (src vm)
   (export
      apply apply/cc arity-error
      call-with-current-continuation

      *primops* ; global list of primitive operations with parameters
      multiple-return-variable-primops
      variable-input-arity-primops special-bind-primops

      ; low level commands (used by assemply etc.)
      GOTO RET ARITY-ERROR
      JEQ JZ JE JN JF JAF JAFX
      CLOS0 CLOC0 CLOS1 CLOC1
      LD LDE LDN LDT LDF
      MOVE REFI MOVE2

      ; todo: change TFIX to TSHORT, TINT to TLONG
      ; or to TINTEGER, TBIGNUM
      ; types
      TFIX+ TFIX- TINT+ TINT- TRATIONAL TCOMPLEX TINEXACT
      TPAIR TTUPLE TSTRING TSYMBOL
      TBYTECODE TPROCEDURE TCLOSURE
      TVPTR TCALLABLE
      TSTRINGWIDE

      ; public primitives
      CAR CDR NEW    ; used by (lang compile) and (lang eval)

      ; internal commands, don't act as primop
      vm:run vm:sys)

   (begin
;     пример выполнение raw-кода прямо в интерпретаторе:
;     создадим альтернативную "native" команду cons
;      > (define construct (raw type-bytecode (list 51 4 5 6 24 6)))
;      ;; Defined construct
;      > (construct 1 2)
;      '(1 . 2)
;      >

      ; -------------------------------------------
      ;; Список типов данных вирутальной машины

      (setq TFIX+              0) ; value
      (setq TFIX-             32) ; value
      (setq TINT+             40) ; reference
      (setq TINT-             41) ; reference
      (setq TRATIONAL         42) ; reference
      (setq TCOMPLEX          43) ; reference
      (setq TINEXACT          44) ; reference, IEEE 754 64-bit binary

      (setq TPAIR              1) ; reference
      (setq TTUPLE             2) ; reference
      (setq TSTRING            3) ; reference, raw -> 35 (#b100000 + 3)?
      (setq TSYMBOL            4) ; reference
      ; 5   TODO(?): (define type-string-wide      5) ; reference, raw
      ; 6
      ; 7
      (setq type-ff-black-leaf 8) ; reference ; TODO: move to 28
      ; 9

      (setq type-rlist-spine  10) ; reference
      (setq type-vector-leaf  11) ; reference

      (setq TPORT             12) ; value
      (setq TCONST            13) ; value

      (setq type-rlist-node   14) ; reference
      (setq type-vector-dispatch 15) ; reference

      (setq TBYTECODE         16) ; reference, raw bytecode
      (setq TPROCEDURE        17) ; reference, pure function
      (setq TCLOSURE          18) ; reference, function with closures
      (setq TBYTEVECTOR       19) ; reference, blob    ; see also TBVEC in c/ovm.c

      ; 20
      (setq type-string-dispatch 21) ; reference
      (setq TSTRINGWIDE       22) ; reference, raw
      ; 23

      ;; transitional trees or future ffs
      (setq type-ff           24) ; reference
      (setq type-ff-r         25) ; reference
      (setq type-ff-red       26) ; reference
      (setq type-ff-red-r     27) ; reference
      ; + type-ff-red, type-ff-right

      ;28
      ;29
      ;30

      (setq type-thread-state     31) ; reference

      (setq TVPTR             49) ; reference,  raw
      (setq TCALLABLE         61) ; reference,  raw


      ; -------------------------------------------
      ;; Список кодов виртуальной машины:

      (setq GOTO 2) ; jmp a, nargs    call Ra with nargs args
      ;(setq GOTO-CODE 18) ; not used for now, check (fn-type)
      ;(setq GOTO-PROC 19) ; not used for now, check (fn-type)
      ;(setq GOTO-CLOS 21) ; not used for now, check (fn-type)
      (setq RET   24)

      ; set
      (setq MOVE  9) ; move a, t:      Ra -> Rt
      (setq REFI  1) ; refi a, p, t:   Ra[p] -> Rt, p unsigned
      (setq MOVE2 5) ; two moves, 4 args

      ; load
      (setq LD   14)  ; ld a, t:        Rt = a, signed byte
      (setq LDE  13)  ; (+ 13 (<< 0 6))) ; 13
      (setq LDN  77)  ; (+ 13 (<< 1 6))) ; 77
      (setq LDT  141) ; (+ 13 (<< 2 6))) ; 141  ldt t:          Rt = true
      (setq LDF  205) ; (+ 13 (<< 3 6))) ; 205  ldf t:          Rt = false

      ; 
      (setq CLOS0 3) ; clos lp, o, nenv, e0 ... en, t:
      (setq CLOC0 4) ; cloc lp, o, nenv, e0 ... en, t:
      (setq CLOS1 6)
      (setq CLOC1 7)

      ; conditional jumps
      (setq JEQ   8)  ; jeq a b o1 o2
      (setq JZ   16)  ; (+ 16 (<< 0 6))) ; jump-imm[0] if zero
      (setq JN   80)  ; (+ 16 (<< 1 6))) ; jump-imm[0] if null
      (setq JE   144) ; (+ 16 (<< 2 6))) ; jump-imm[0] if empty
      (setq JF   208) ; (+ 16 (<< 3 6))) ; jump-imm[0] if false
      (setq JAF  11)  ; jump if arity failed
      (setq JAFX 12)  ; JAF with packing extra arguments in list


      ; -------------------------------------------
      ;; Примитивные операции/операторы
      ; internal helper
      (setq new-bytecode   (lambda (bytecode) (make-blob TBYTECODE bytecode)))

      ; memory allocators (no default bytecode exists, should generate on-the-fly)
      (setq NEW 23)        ; no real (vm:new) command required, check rtl-primitive in (lang compile)
      (setq MAKE 18)       ;(setq vm:make   (new-bytecode '(18))) ; stub for (prim-opcodes)
      (setq MAKE-BLOB 19)  ;(setq make-blob (new-bytecode '(19))) ; stub for (prim-opcodes)
      (setq CAST 22)       ;(setq vm:cast   (new-bytecode '(22 4 5 6  24 6))) ; cast object type (works for immediates and allocated)

      ; vm:new - simplest and fastest allocator, creates only objects, can't create objects with more than 256 elements length
      ; vm:make - smarter allocator, can create objects with size and default element

      ; 
      (setq APPLY 20)       (setq apply (new-bytecode '(20))) ; stub for (prim-opcodes)
      (setq ARITY-ERROR 17) (setq arity-error (new-bytecode '(17))) ; stub for (prim-opcodes)
      (setq APPLY/CC 84)    (setq apply/cc (new-bytecode '(84))) ; stub for (prim-opcodes)

      ; other instructions
      (setq NOP 21)      (setq vm:nop  (new-bytecode '(21)))
      (setq SYS 27)      (setq vm:sys  (new-bytecode '(27 4 5 6 7 8  24 8)))
      (setq RUN 50)      (setq vm:run  (new-bytecode '(50 4 5)))

      ; primops:

      ; арифметические операции, которые возвращают пару(тройку) значений, использовать через let*/values-apply
      (setq ADD 38)      ;(setq vm:add  (new-bytecode '(38 4 5       6 7)))
      (setq MUL 39)      ;(setq vm:mul  (new-bytecode '(39 4 5       6 7)))
      (setq SUB 40)      ;(setq vm:sub  (new-bytecode '(40 4 5       6 7)))
      (setq DIV 26)      ;(setq vm:div  (new-bytecode '(26 4 5 6     7 8 9)))
      (setq SHR 58)      ;(setq vm:shr  (new-bytecode '(58 4 5       6 7)))
      (setq SHL 59)      ;(setq vm:shl  (new-bytecode '(59 4 5       6 7)))

      (setq AND 55)      ;(setq vm:and  (new-bytecode '(55 4 5 6  24 6)))
      (setq OR 56)       ;(setq vm:or   (new-bytecode '(56 4 5 6  24 6)))
      (setq XOR 57)      ;(setq vm:xor  (new-bytecode '(57 4 5 6  24 6)))

      ; инструкции поддержки арифметики с плавающей точкой (inexact math)
      (setq FP1 33)      ;(setq vm:fp1 (new-bytecode '(33 4 5 6    24 6)))
      (setq FP2 34)      ;(setq vm:fp2 (new-bytecode '(34 4 5 6 7  24 7)))

      ; cons:
      ; https://www.gnu.org/software/emacs/manual/html_node/eintr/Strange-Names.html#Strange-Names
      ; The name of the cons function is not unreasonable: it is an abbreviation of the word `construct'.
      (setq CONS 51)     ;(setq cons    (new-bytecode '(51 4 5 6  24 6)))

      ; The origins of the names for car and cdr, on the other hand, are esoteric: car is an acronym from
      ; the phrase `Contents of the Address part of the Register'; and cdr (pronounced `could-er') is an
      ; acronym from the phrase `Contents of the Decrement part of the Register'. These phrases refer to
      ; specific pieces of hardware on the very early computer on which the original Lisp was developed.
      ; Besides being obsolete, the phrases have been completely irrelevant for more than 25 years to anyone
      ; thinking about Lisp. Nonetheless, although a few brave scholars have begun to use more reasonable
      ; names for these functions, the old terms are still in use.
      (setq CAR 52)      ;(setq car     (new-bytecode '(52 4 5    24 5)))
      (setq CDR 53)      ;(setq cdr     (new-bytecode '(53 4 5    24 5)))
      (setq REF 47)      ;(setq ref     (new-bytecode '(47 4 5 6  24 6)))
      
      (setq TYPE 15)     ;(setq type    (new-bytecode '(15 4 5    24 5))) ;; get just the type bits (new)
      (setq SIZE 36)     ;(setq size    (new-bytecode '(36 4 5    24 5))) ;; get object data size (- hdr)

      (setq EQ? 54)      ;(setq eq?     (new-bytecode '(54 4 5 6  24 6)))
      (setq LESS? 44)    ;(setq less?   (new-bytecode '(44 4 5 6  24 6)))

      ; deprecated:
      ;(define clock   (make-blob type-bytecode '(61 4 5)))            ;; must add 61 to the multiple-return-variable-primops list

      (setq SET-REF 45)  ;(setq set-ref  (new-bytecode '(45 4 5 6 7  24 7)))
      (setq SET-REF! 10) ;(setq set-ref! (new-bytecode '(10 4 5 6 7  24 7))) ; todo: change to like set-ref

      ; primitives
      (setq TUPLE-APPLY 32)
      (setq FF-APPLY 49) ;(setq ff-apply (new-bytecode '(49 4)))

      ;;(define ff:red     (make-blob type-bytecode '(43 4 5 6 7  8  24 8)))
      ;;(define ff:black   (make-blob type-bytecode '(42 4 5 6 7  8  24 8)))
      ;;(define ff:toggle  (make-blob type-bytecode '(46 4        5  24 5)))
      ;;(define ff:red?    (make-blob type-bytecode '(41 4        5  24 5)))
      ;;(define ff:right?  (make-blob type-bytecode '(37 4        5  24 5)))

      (setq vm:pin    (new-bytecode '(35 4 5  24 5)))
      (setq vm:unpin  (new-bytecode '(19 4 5  24 5)))
      (setq vm:deref  (new-bytecode '(25 4 5  24 5)))

      ;(setq syscall (new-bytecode '(63 4 5 6 7 8  24 8)))

      ;(setq vm:endianness (new-bytecode '(28 4))) ; TODO: remove from commands
      ;(setq vm:wordsize   (new-bytecode '(29 4))) ; TODO: merge next three commands into one
      ;(setq vm:valuewidth (new-bytecode '(31 4)))
      ;(setq vm:maxvalue   (new-bytecode '(30 4)))

      ;(setq vm:version    (new-bytecode '(62 4)))

      ; todo: план по слиянию new-object и new-raw-object, с одновременным
      ;       внесением бита "rawness" в числовое значение типа
      ;  1. добавить параметр "rawness" к аргументам new-object
      ;  2. удалить new-raw-object
      ;  3. изменить нумерацию типов
      ;  4. удалить параметр rawness
      ;  5. переименовать vm:new-object в vm:make

      (setq *primops*
         ; прямые аллокаторы
         (cons (vm:new TTUPLE 'vm:new    23 'any 1 (new-bytecode '(23)))
         (cons (vm:new TTUPLE 'vm:make   18 'any 1 (new-bytecode '(18)))
         (cons (vm:new TTUPLE 'vm:cast   22  2 1 vm:cast)  ;; cast object type (works for immediates and allocated)

         (cons (vm:new TTUPLE 'make-blob 19 'any 1 (new-bytecode '(19)))

         ; конструкторы
         (cons (vm:new TTUPLE 'cons     CONS 2 1 cons)

         ; геттеры
         (cons (vm:new TTUPLE 'car      CAR  1 1 car)   ; (make-blob type-bytecode '(52 4 5    24 5))
         (cons (vm:new TTUPLE 'cdr      CDR  1 1 cdr)   ; (make-blob type-bytecode '(53 4 5    24 5))
         (cons (vm:new TTUPLE 'ref      REF  2 1 ref)   ; (make-blob type-bytecode '(47 4 5 6  24 6))   ; op47 = ref t o r = prim_ref(A0, A1)

         (cons (vm:new TTUPLE 'type     TYPE  1 1 type)  ;; get just the type bits
         (cons (vm:new TTUPLE 'size     SIZE  1 1 size)  ;; get object size (- 1)

         ; сеттеры
         (cons (vm:new TTUPLE 'set-ref  SET-REF  3 1 set-ref)
         (cons (vm:new TTUPLE 'set-ref! SET-REF! 3 1 set-ref!)

         ; компараторы
         (cons (vm:new TTUPLE 'eq?      EQ?   2 1 eq?)
         (cons (vm:new TTUPLE 'less?    LESS? 2 1 less?)

         ; базовая арифметика
         (cons (vm:new TTUPLE 'vm:add   ADD  2 2 vm:add)
         (cons (vm:new TTUPLE 'vm:mul   MUL  2 2 vm:mul)
         (cons (vm:new TTUPLE 'vm:sub   SUB  2 2 vm:sub)
         (cons (vm:new TTUPLE 'vm:div   DIV  3 3 vm:div) ; todo: change (vm:div hi lo b) to (vm:div lo hi b)
         ; сдвиги
         (cons (vm:new TTUPLE 'vm:shr   SHR  2 2 vm:shr)
         (cons (vm:new TTUPLE 'vm:shl   SHL  2 2 vm:shl)
         ; бинарная арифметика
         (cons (vm:new TTUPLE 'vm:and   AND  2 1 vm:and)
         (cons (vm:new TTUPLE 'vm:or    OR   2 1 vm:or)
         (cons (vm:new TTUPLE 'vm:xor   XOR  2 1 vm:xor)

         (cons (vm:new TTUPLE 'vm:fp1   FP1 2 1 vm:fp1)
         (cons (vm:new TTUPLE 'vm:fp2   FP2 3 1 vm:fp2)

         ; системный таймер
         (cons (vm:new TTUPLE 'clock    61  0 2 clock) ;; todo: удалить            must add 61 to the multiple-return-variable-primops list
         ; системные вызовы
         (cons (vm:new TTUPLE 'syscall  63  4 1 syscall)

         ; vm-specific constants
         (cons (vm:new TTUPLE 'vm:endianness 28  0 1 vm:endianness)
         (cons (vm:new TTUPLE 'vm:wordsize   29  0 1 vm:wordsize)
         (cons (vm:new TTUPLE 'vm:maxvalue   30  0 1 vm:maxvalue)
         (cons (vm:new TTUPLE 'vm:valuewidth 31  0 1 vm:valuewidth)

         (cons (vm:new TTUPLE 'vm:version    62  0 1 vm:version)

         ; todo: add macro for call-with-tuple in r5rs
         (cons (vm:new TTUPLE 'tuple-apply   32 1 #f tuple-apply)

         ; поддержка finite functions (как red-black деревьев)
         (cons (vm:new TTUPLE 'ff-apply   49 1 #f  ff-apply)

         (cons (vm:new TTUPLE 'ff:red     43 4  1  ff:red)
         (cons (vm:new TTUPLE 'ff:black   42 4  1  ff:black)
         (cons (vm:new TTUPLE 'ff:toggle  46 1  1  ff:toggle)
         (cons (vm:new TTUPLE 'ff:red?    41 1  1  ff:red?)
         (cons (vm:new TTUPLE 'ff:right?  37 1  1  ff:right?)

         (cons (vm:new TTUPLE 'vm:pin    35 1  1  vm:pin)
         (cons (vm:new TTUPLE 'vm:unpin  60 1  1  vm:unpin)
         (cons (vm:new TTUPLE 'vm:deref  25 1  1  vm:deref)
         #null)))))))))))))))))))))))))))))))))))))))))))

      ;; fixme: handle multiple return value primops sanely (now a list)
      ; для этих команд НЕ вставляется аргументом длина списка команд
      (setq multiple-return-variable-primops
         (cons FF-APPLY
         (cons ADD
         (cons MUL
         (cons SUB
         (cons DIV
         (cons SHR
         (cons SHL
         (cons 61  ; (clock)
         #null)))))))))

      ; todo: check this and opcode-arity-ok-2? - maybe should merge this entities?
      (setq variable-input-arity-primops ; todo: move to other module
         (cons NEW
         (cons MAKE
         (cons MAKE-BLOB
         #null))))

      (setq special-bind-primops
         (cons TUPLE-APPLY
         (cons FF-APPLY
         #null)))


      ; the best place for call/cc is here
      (setq call-with-current-continuation
         ('_sans_cps (lambda (k f)
                        (f k (lambda (c . x) (apply/cc k x))))))
                        ; accelerated version:
                        ;(f k
                        ;   (either (lambda (c a) (k a))
                        ;   (either (lambda (c a b) (k a b))
                        ;   (either (lambda (c . x) (apply/cc k x))
                        ;           (lambda () (arity-error)))))))))

))
