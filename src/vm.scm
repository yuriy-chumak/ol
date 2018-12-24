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
;      > (define construct (vm:makeb type-bytecode '(51 4 5  6  24 6)))
;        ; notes:
;        ; 51 - command bytecode
;        ; 4 5 - two registers for input values
;        ; 6 - register to store result
;        ; 24 6 - RET 6, where 6 is results register
;
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
      (setq TBYTEVECTOR       19) ; reference, bytevector

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

      (setq TVPTR             49) ; reference,  blob
      (setq TCALLABLE         61) ; reference,  blob


      ; -------------------------------------------
      ;; Список кодов виртуальной машины:

      (setq GOTO 2) ; jmp a, nargs    call Ra with nargs args
      ;(setq GOTO-CODE 18) ; not used for now, check (fn-type)
      ;(setq GOTO-PROC 19) ; not used for now, check (fn-type)
      ;(setq GOTO-CLOS 21) ; not used for now, check (fn-type)
      (setq RET  24)

      ; set
      (setq MOVE  9) ; move a, t:      Ra -> Rt
      (setq REFI  1) ; refi a, p, t:   Ra[p] -> Rt, p unsigned
      (setq MOVE2 5) ; two moves, 4 args

      ; load
      (setq LD   14)  ; ld a, t:        Rt = a, signed byte
      (setq LDE  13)  ; (+ 13 (<< 0 6))) ; 13
      (setq LDN  77)  ; (+ 13 (<< 1 6))) ; 77
      (setq LDT 141)  ; (+ 13 (<< 2 6))) ; 141  ldt t:          Rt = true
      (setq LDF 205)  ; (+ 13 (<< 3 6))) ; 205  ldf t:          Rt = false

      ; 
      (setq CLOS0 3) ; clos lp, o, nenv, e0 ... en, t:
      (setq CLOC0 4) ; cloc lp, o, nenv, e0 ... en, t:
      (setq CLOS1 6)
      (setq CLOC1 7)

      ; conditional jumps
      (setq JEQ   8)  ; jeq a b o1 o2
      (setq JZ   16)  ; (+ 16 (<< 0 6))) ; jump-imm[0] if zero
      (setq JN   80)  ; (+ 16 (<< 1 6))) ; jump-imm[0] if null
      (setq JE  144)  ; (+ 16 (<< 2 6))) ; jump-imm[0] if empty
      (setq JF  208)  ; (+ 16 (<< 3 6))) ; jump-imm[0] if false
      (setq JAF  11)  ; jump if arity failed
      (setq JAFX 12)  ; JAF with packing extra arguments in list


      ; -------------------------------------------
      ;; Примитивные операции/операторы
      ; internal helper
      (setq make-bytecode (lambda (bytecode) (vm:makeb TBYTECODE bytecode)))

      ; memory allocators (no default bytecode exists, should generate on-the-fly)
      (setq NEW 23)        ; no real (vm:new) command required, check rtl-primitive in (lang compile)
      (setq MAKE 18)       ;(setq vm:make  (fake-bytecode '(18))) ; stub for (prim-opcodes)
      (setq MAKEB 19)      ;(setq vm:makeb (fake-bytecode '(19))) ; stub for (prim-opcodes)
      (setq CAST 22)       ;(setq vm:cast  (make-bytecode '(22 4 5 6  24 6))) ; cast object type (works for immediates and allocated)

      ; vm:new - simplest and fastest allocator, creates only objects, can't create objects with more than 256 elements length
      ; vm:make - smarter allocator, can create objects with size and default element

      ; 
      (setq APPLY 20)       (setq apply (make-bytecode '(20))) ; stub for (prim-opcodes)
      (setq ARITY-ERROR 17) (setq arity-error (make-bytecode '(17))) ; stub for (prim-opcodes)
      (setq APPLY/CC 84)    (setq apply/cc (make-bytecode '(84))) ; stub for (prim-opcodes)

      ; other instructions
      (setq NOP 21)      (setq vm:nop  (make-bytecode '(21)))
      (setq SYS 27)      (setq vm:sys  (make-bytecode '(27 4 5 6 7 8  24 8)))
      (setq RUN 50)      (setq vm:run  (make-bytecode '(50 4 5)))

      ; primops:

      ; арифметические операции, которые возвращают пару(тройку) значений, использовать через let*/values-apply
      (setq ADD 38)      ;(setq vm:add  (make-bytecode '(38 4 5       6 7)))
      (setq MUL 39)      ;(setq vm:mul  (make-bytecode '(39 4 5       6 7)))
      (setq SUB 40)      ;(setq vm:sub  (make-bytecode '(40 4 5       6 7)))
      (setq DIV 26)      ;(setq vm:div  (make-bytecode '(26 4 5 6     7 8 9)))
      (setq SHR 58)      ;(setq vm:shr  (make-bytecode '(58 4 5       6 7)))
      (setq SHL 59)      ;(setq vm:shl  (make-bytecode '(59 4 5       6 7)))

      (setq AND 55)      ;(setq vm:and  (make-bytecode '(55 4 5 6  24 6)))
      (setq OR 56)       ;(setq vm:or   (make-bytecode '(56 4 5 6  24 6)))
      (setq XOR 57)      ;(setq vm:xor  (make-bytecode '(57 4 5 6  24 6)))

      ; инструкции поддержки арифметики с плавающей точкой (inexact math)
      (setq FP1 33)      ;(setq vm:fp1 (make-bytecode '(33 4 5 6    24 6)))
      (setq FP2 34)      ;(setq vm:fp2 (make-bytecode '(34 4 5 6 7  24 7)))

      ; cons:
      ; https://www.gnu.org/software/emacs/manual/html_node/eintr/Strange-Names.html#Strange-Names
      ; The name of the cons function is not unreasonable: it is an abbreviation of the word `construct'.
      (setq CONS 51)     ;(setq cons    (make-bytecode '(51 4 5 6  24 6)))

      ; The origins of the names for car and cdr, on the other hand, are esoteric: car is an acronym from
      ; the phrase `Contents of the Address part of the Register'; and cdr (pronounced `could-er') is an
      ; acronym from the phrase `Contents of the Decrement part of the Register'. These phrases refer to
      ; specific pieces of hardware on the very early computer on which the original Lisp was developed.
      ; Besides being obsolete, the phrases have been completely irrelevant for more than 25 years to anyone
      ; thinking about Lisp. Nonetheless, although a few brave scholars have begun to use more reasonable
      ; names for these functions, the old terms are still in use.
      (setq CAR 52)      ;(setq car     (make-bytecode '(52 4 5    24 5)))
      (setq CDR 53)      ;(setq cdr     (make-bytecode '(53 4 5    24 5)))
      (setq REF 47)      ;(setq ref     (make-bytecode '(47 4 5 6  24 6)))
      
      (setq TYPE 15)     ;(setq type    (make-bytecode '(15 4 5    24 5))) ;; get just the type bits (new)
      (setq SIZE 36)     ;(setq size    (make-bytecode '(36 4 5    24 5))) ;; get object data size (- hdr)

      (setq EQ? 54)      ;(setq eq?     (make-bytecode '(54 4 5 6  24 6)))
      (setq LESS? 44)    ;(setq less?   (make-bytecode '(44 4 5 6  24 6)))

      ; deprecated:
      ;(define clock   (vm:makeb type-bytecode '(61 4 5)))            ;; must add 61 to the multiple-return-variable-primops list

      (setq SET-REF 45)  ;(setq set-ref  (make-bytecode '(45 4 5 6 7  24 7)))
      (setq SET-REF! 10) ;(setq set-ref! (make-bytecode '(10 4 5 6 7  24 7))) ; todo: change to like set-ref

      ; primitives
      (setq TUPLE-APPLY 32)
      (setq FF-APPLY 49) ;(setq ff-apply (make-bytecode '(49 4)))

      ; associative array
      (setq ff:red    (make-bytecode '(106 4 5 6 7  8  24 8))) ;106 = 42+(1<<6)
      (setq ff:black  (make-bytecode '(42  4 5 6 7  8  24 8)))
      (setq ff:toggle (make-bytecode '(46  4        5  24 5)))
      (setq ff:red?   (make-bytecode '(41  4        5  24 5)))
      (setq ff:right? (make-bytecode '(37  4        5  24 5)))

      ; pinned objects
      (setq vm:pin    (make-bytecode '(35 4 5  24 5)))
      (setq vm:unpin  (make-bytecode '(60 4 5  24 5)))
      (setq vm:deref  (make-bytecode '(25 4 5  24 5)))

      ;(setq syscall (make-bytecode '(63 4 5 6 7 8  24 8)))

      ;(setq vm:valuewidth (make-bytecode '(31 4)))
      ;(setq vm:maxvalue   (make-bytecode '(30 4)))

      ;(setq vm:version    (make-bytecode '(62 4)))

      ; todo: план по слиянию new-object и new-raw-object, с одновременным
      ;       внесением бита "rawness" в числовое значение типа
      ;  1. добавить параметр "rawness" к аргументам new-object
      ;  2. удалить new-raw-object
      ;  3. изменить нумерацию типов
      ;  4. удалить параметр rawness
      ;  5. переименовать vm:new-object в vm:make
      (setq primop (lambda (name in out code)
         (vm:new TTUPLE name  (ref code 0)  in out code)))
      (setq fake-bytecode (lambda (n)
         (make-bytecode n)))

      (setq *primops*
         ; прямые аллокаторы
         (cons (primop 'vm:new   'any 1 (fake-bytecode '(23)))
         (cons (primop 'vm:make  'any 1 (fake-bytecode '(18))) ; make object
         (cons (primop 'vm:makeb 'any 1 (fake-bytecode '(19))) ; make blob object

         (cons (primop 'vm:cast     2 1 vm:cast)

         ; конструкторы
         (cons (primop 'cons     2 1 cons)

         ; геттеры
         (cons (primop 'car      1 1 car)
         (cons (primop 'cdr      1 1 cdr)
         (cons (primop 'ref      2 1 ref)

         (cons (primop 'type     1 1 type)  ;; get just the type bits
         (cons (primop 'size     1 1 size)  ;; get object size (- 1)

         ; сеттеры
         (cons (primop 'set-ref  3 1 set-ref)
         (cons (primop 'set-ref! 3 1 set-ref!)

         ; компараторы
         (cons (primop 'eq?      2 1 eq?)
         (cons (primop 'less?    2 1 less?)

         ; базовая арифметика
         (cons (primop 'vm:add   2 2 vm:add)
         (cons (primop 'vm:mul   2 2 vm:mul)
         (cons (primop 'vm:sub   2 2 vm:sub)
         (cons (primop 'vm:div   3 3 vm:div) ; todo: change (vm:div hi lo b) to (vm:div lo hi b)
         ; сдвиги
         (cons (primop 'vm:shr   2 2 vm:shr)
         (cons (primop 'vm:shl   2 2 vm:shl)
         ; бинарная арифметика
         (cons (primop 'vm:and   2 1 vm:and)
         (cons (primop 'vm:or    2 1 vm:or)
         (cons (primop 'vm:xor   2 1 vm:xor)

         (cons (primop 'vm:fp1   2 1 vm:fp1)
         (cons (primop 'vm:fp2   3 1 vm:fp2)

         ; системный таймер
         (cons (primop 'clock    0 2 clock) ;; todo: удалить            must add 61 to the multiple-return-variable-primops list
         ; системные вызовы
         (cons (primop 'syscall  4 1 syscall)

         ; vm-specific constants
         (cons (primop 'vm:maxvalue   0 1 vm:maxvalue)
         (cons (primop 'vm:valuewidth 0 1 vm:valuewidth)

         (cons (primop 'vm:version    0 1 vm:version)

         ; todo: add macro for call-with-tuple in r5rs
         (cons (primop 'tuple-apply   1 #f tuple-apply)

         ; поддержка finite functions (как red-black деревьев)
         (cons (primop 'ff-apply      1 #f  ff-apply)

         (cons (primop 'ff:black  4 1 ff:black)
         (cons (primop 'ff:red    4 1 ff:red)
         (cons (primop 'ff:toggle 1 1 ff:toggle)
         (cons (primop 'ff:red?   1 1 ff:red?)
         (cons (primop 'ff:right? 1 1 ff:right?)

         (cons (primop 'vm:pin    1 1 vm:pin)
         (cons (primop 'vm:unpin  1 1 vm:unpin)
         (cons (primop 'vm:deref  1 1 vm:deref)
         #null)))))))))))))))))))))))))))))))))))))))))

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
      ; todo: dynamically generate based on *primops*
      (setq variable-input-arity-primops ; todo: move to other module
         (cons NEW
         (cons MAKE
         (cons MAKEB
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
