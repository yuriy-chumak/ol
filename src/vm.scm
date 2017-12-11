;; vm primops

;; todo: convert tuple to variable arity
;; todo: convert arity checks 17 -> 25

;; todo: maybe move ncar, and other "n" to the normal but with macroses on top level with type checking.
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

      (setq type-vector-raw   19) ; reference, raw     ; see also TBVEC in c/ovm.c

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

      ; memory allocators
      (setq NEW 23)        ; no real vm:new command required, check rtl-primitive in (lang compile)
      (setq RAW-OBJECT 60) ;(setq vm:new-raw-object (vm:new-raw-object TBYTECODE '(60 4 5 6  24 6)))
      (setq NEW-OBJECT 35) ;(setq vm:new-object     (vm:new-raw-object TBYTECODE '(35 4 5 6  24 6)))
                            (setq vm:new-bytecode   (lambda (bytecode) (vm:new-raw-object TBYTECODE bytecode)))

      ; 
      (setq APPLY 20)       (setq apply (vm:new-bytecode '(20)))
      (setq ARITY-ERROR 17) (setq arity-error (vm:new-bytecode '(17)))
      (setq APPLY/CC 84)    (setq apply/cc (vm:new-bytecode '(84)))

      ; other instructions
      (setq NOP 21)
      (setq SYS 27)      (setq vm:sys  (vm:new-bytecode '(27 4 5 6 7 8  24 8)))
      (setq RUN 50)      (setq vm:run  (vm:new-bytecode '(50 4 5)))

      ; primops:

      (setq RAW? 48)     ;(setq vm:raw? (vm:new-bytecode '(48 4 5    24 5)))
      (setq CAST 22)     ;(setq vm:cast (vm:new-bytecode '(22 4 5 6  24 6))) ;; cast object type (works for immediates and allocated)

      ; арифметические операции, которые возвращают пару(тройку) значений, использовать через let*/values-apply
      (setq ADD 38)      ;(setq vm:add  (vm:new-bytecode '(38 4 5       6 7)))
      (setq MUL 39)      ;(setq vm:mul  (vm:new-bytecode '(39 4 5       6 7)))
      (setq SUB 40)      ;(setq vm:sub  (vm:new-bytecode '(40 4 5       6 7)))
      (setq DIV 26)      ;(setq vm:div  (vm:new-bytecode '(26 4 5 6     7 8 9)))
      (setq SHR 58)      ;(setq vm:shr  (vm:new-bytecode '(58 4 5       6 7)))
      (setq SHL 59)      ;(setq vm:shl  (vm:new-bytecode '(59 4 5       6 7)))

      (setq AND 55)      ;(setq vm:and  (vm:new-bytecode '(55 4 5 6  24 6)))
      (setq OR 56)       ;(setq vm:or   (vm:new-bytecode '(56 4 5 6  24 6)))
      (setq XOR 57)      ;(setq vm:xor  (vm:new-bytecode '(57 4 5 6  24 6)))

      ; инструкции поддержки арифметики с плавающей точкой (inexact math)
      (setq FPU1 33)     ;(setq vm:fpu1 (vm:new-bytecode '(33 4 5 6    24 6)))
      (setq FPU2 34)     ;(setq vm:fpu2 (vm:new-bytecode '(34 4 5 6 7  24 7)))

      ; cons:
      ; https://www.gnu.org/software/emacs/manual/html_node/eintr/Strange-Names.html#Strange-Names
      ; The name of the cons function is not unreasonable: it is an abbreviation of the word `construct'.
      ; The origins of the names for car and cdr, on the other hand, are esoteric: car is an acronym from
      ; the phrase `Contents of the Address part of the Register'; and cdr (pronounced `could-er') is an
      ; acronym from the phrase `Contents of the Decrement part of the Register'. These phrases refer to
      ; specific pieces of hardware on the very early computer on which the original Lisp was developed.
      ; Besides being obsolete, the phrases have been completely irrelevant for more than 25 years to anyone
      ; thinking about Lisp. Nonetheless, although a few brave scholars have begun to use more reasonable
      ; names for these functions, the old terms are still in use.
      (setq CONS 51)     ;(setq cons    (vm:new-bytecode '(51 4 5 6  24 6)))

      (setq CAR 52)      ;(setq car     (vm:new-bytecode '(52 4 5    24 5)))
      (setq CDR 53)      ;(setq cdr     (vm:new-bytecode '(53 4 5    24 5)))
      (setq REF 47)      ;(setq ref     (vm:new-bytecode '(47 4 5 6  24 6)))
      
      (setq TYPE 15)     ;(setq type    (vm:new-bytecode '(15 4 5    24 5))) ;; get just the type bits (new)
      (setq SIZE 36)     ;(setq size    (vm:new-bytecode '(36 4 5    24 5))) ;; get object size (- 1)

      (setq EQ? 54)      ;(setq eq?     (vm:new-bytecode '(54 4 5 6  24 6)))
      (setq LESS? 44)    ;(setq less?   (vm:new-bytecode '(44 4 5 6  24 6)))

      ; deprecated:
      ;(define clock   (vm:new-raw-object type-bytecode '(61 4 5)))            ;; must add 61 to the multiple-return-variable-primops list

      (setq SET-REF 45)  ;(setq set-ref  (vm:new-bytecode '(45 4 5 6 7  24 7)))
      (setq SET-REF! 10) ;(setq set-ref! (vm:new-bytecode '(10 4 5 6 7  24 7))) ; todo: change to like set-ref

      ; primitives
      (setq TUPLE-APPLY 32)
      (setq FF-APPLY 49) ;(setq ff-apply (vm:new-bytecode '(49 4)))

      ;;(define ff:red     (vm:new-raw-object type-bytecode '(43 4 5 6 7  8  24 8)))
      ;;(define ff:black   (vm:new-raw-object type-bytecode '(42 4 5 6 7  8  24 8)))
      ;;(define ff:toggle  (vm:new-raw-object type-bytecode '(46 4        5  24 5)))
      ;;(define ff:red?    (vm:new-raw-object type-bytecode '(41 4        5  24 5)))
      ;;(define ff:right?  (vm:new-raw-object type-bytecode '(37 4        5  24 5)))

      ;(setq syscall (vm:new-bytecode '(63 4 5 6 7 8  24 8)))

      ;(setq vm:endianness (vm:new-bytecode '(28 4)))
      ;(setq vm:wordsize   (vm:new-bytecode '(29 4)))
      ;(setq vm:valuewidth (vm:new-bytecode '(31 4)))
      ;(setq vm:maxvalue   (vm:new-bytecode '(30 4)))

      ;(setq vm:version    (vm:new-bytecode '(62 4)))

      (setq *primops*
         ; аллокаторы
         (cons (vm:new TTUPLE 'vm:new-object     NEW-OBJECT  2 1 vm:new-object)       ; create reference object (vm:new-object type '(v1 .. vn))
         (cons (vm:new TTUPLE 'vm:new-raw-object RAW-OBJECT  2 1 vm:new-raw-object)   ; create raw reference object (vm:new-raw-object type '(v0 .. vn)) or (vm:new-raw-object type size)

         (cons (vm:new TTUPLE 'vm:new   NEW 'any 1 #f)   ; fast creation of small (less than 128 elements) reference object (vm:new type v1 .. vn)

         (cons (vm:new TTUPLE 'vm:raw?  RAW? 1 1 vm:raw?)  ;; временное решение, пока не придумаю как удалить совсем ; todo: change to rawq?
         (cons (vm:new TTUPLE 'vm:cast  CAST 2 1 vm:cast)  ;; cast object type (works for immediates and allocated)

         ; конструкторы
         (cons (vm:new TTUPLE 'cons     CONS 2 1 cons)

         ; геттеры
         (cons (vm:new TTUPLE 'car      CAR  1 1 car)   ; (vm:new-raw-object type-bytecode '(52 4 5    24 5))
         (cons (vm:new TTUPLE 'cdr      CDR  1 1 cdr)   ; (vm:new-raw-object type-bytecode '(53 4 5    24 5))
         (cons (vm:new TTUPLE 'ref      REF  2 1 ref)   ; (vm:new-raw-object type-bytecode '(47 4 5 6  24 6))   ; op47 = ref t o r = prim_ref(A0, A1)

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

         (cons (vm:new TTUPLE 'vm:fpu1  FPU1 2 1 vm:fpu1)
         (cons (vm:new TTUPLE 'vm:fpu2  FPU2 3 1 vm:fpu2)

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
         (cons (vm:new TTUPLE 'tuple-apply 32 1 #false tuple-apply)

         ; поддержка finite functions (как red-black деревьев)
         (cons (vm:new TTUPLE 'ff-apply   49 1 #f  ff-apply)

         (cons (vm:new TTUPLE 'ff:red     43 4  1  ff:red)
         (cons (vm:new TTUPLE 'ff:black   42 4  1  ff:black)
         (cons (vm:new TTUPLE 'ff:toggle  46 1  1  ff:toggle)
         (cons (vm:new TTUPLE 'ff:red?    41 1  1  ff:red?)
         (cons (vm:new TTUPLE 'ff:right?  37 1  1  ff:right?)
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

      (setq variable-input-arity-primops
         (cons NEW
         #null))

      (setq special-bind-primops
         (cons TUPLE-APPLY
         (cons FF-APPLY
         #null)))


      ; the best place for call/cc is here :(
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
