; possible todos
; todo: convert arity checks 17 -> 25
; todo: переделать так, чтобы регистр возврата был самый первый, тогда можно будет обойтись без RET
; todo: план по слиянию new-object и new-raw-object, с одновременным
;       внесением бита "rawness" в числовое значение типа
;  1. добавить параметр "rawness" к аргументам new-object
;  2. удалить new-raw-object
;  3. изменить нумерацию типов
;  4. удалить параметр rawness
; todo: change (vm:div hi lo b) to (vm:div lo hi b)

(define-library (src vm)
   (export
      apply apply/cc arity-error
      call-with-current-continuation
      vm:run vm:mcp ; internal commands, don't act as primop

      *primops* ; global list of primitive operations with parameters
      multiple-return-variable-primops
      variable-input-arity-primops special-bind-primops

      ; low level (assembly) commands
      GOTO RET ARITY-ERROR  ; used by (lang assemply) etc.
      JEQ JZ JE JN JF JAF JAX
      CLOS
      LD LDE LDN LDT LDF
      MOVE REFI MOV2

      NEW    ; used by (lang rtl)

      ; types
      TENUM+ TENUM- TINT+ TINT- TRATIONAL TCOMPLEX TINEXACT
      TPAIR TSYMBOL TVECTOR TBYTEVECTOR
      TSTRING TSTRINGWIDE
      TBYTECODE TPROCEDURE TCLOSURE TCONSTRUCTOR
      TVPTR TCALLABLE)

   (begin
      ; -----------------------------------------------------------------------
      ; Virtual Machine Types List       Список типов данных виртуальной машины

      (setq TENUM+             0) ; value
      (setq TENUM-            32) ; value
      (setq TINT+             40) ; reference
      (setq TINT-             41) ; reference
      (setq TRATIONAL         42) ; reference
      (setq TCOMPLEX          43) ; reference
      (setq TINEXACT          44) ; reference, IEEE 754 64-bit binary

      (setq TPAIR              1) ; reference
      (setq TVECTOR            2) ; reference
      (setq TSTRING            3) ; reference, raw -> 35 (#b100000 + 3)?
      (setq TSYMBOL            4) ; reference
      (setq TSTRINGWIDE        5) ; reference, raw
      ; 6
      ; 7
      ;setq type-ff-black-leaf 8) ; reference ; TODO: move to 28
      ; 9

      ;setq type-rlist-spine  10) ; reference, used by rlist (not retested)
      (setq type-blob-leaf    11) ; reference

      (setq TPORT             12) ; value or reference
      (setq TCONST            13) ; value

      ;setq type-rlist-node   14) ; reference, used by rlist (not retested)
      (setq type-blob-dispatch 15) ; reference

      (setq TBYTECODE         16) ; reference, a bytecode
      (setq TPROCEDURE        17) ; reference, pure function
      (setq TCLOSURE          18) ; reference, function with closures

      (setq TBYTEVECTOR       19) ; reference, raw
      ; 20
      (setq type-superstring  21) ; reference
      ; 22 ?
      ; 23

      ; 24..27 ff types
      ; 28
      ; 29
      ; 30

      (setq type-thread-state 31) ; reference

      (setq TVPTR             49) ; reference, blob
      (setq TCALLABLE         61) ; reference, blob
      (setq TCONSTRUCTOR      63) ; reference, autorun function(constructor)


      ; -------------------------------------------
      ;; Список кодов виртуальной машины, используемых
      ;;  компилятором, которые не экспозятся на сам язык

      ; procedure/closure support
      (setq GOTO  2) ; jmp a, nargs - call Ra with nargs args
      (setq CLOS  3) ; clos type, size, o, nenv, e0 ... en, t
      (setq RET  24)

      ; set
      (setq MOVE  9) ; move a, t:      Ra -> Rt
      (setq REFI  1) ; refi a, p, t:   Ra[p] -> Rt, p unsigned
      (setq MOV2  5) ; two moves, 4 args

      ; load
      (setq LD   14)  ; ld a, t:        Rt = a, signed byte
      (setq LDE  13)  ; (+ 13 (<< 0 6))) ; 13
      (setq LDN  77)  ; (+ 13 (<< 1 6))) ; 77
      (setq LDT 141)  ; (+ 13 (<< 2 6))) ; 141  ldt t:          Rt = true
      (setq LDF 205)  ; (+ 13 (<< 3 6))) ; 205  ldf t:          Rt = false

      ; conditional jumps
      (setq JEQ   8)  ; jeq a b o1 o2
      (setq JZ   16)  ; (+ 16 (<< 0 6))) ; jump-imm[0] if zero
      (setq JN   80)  ; (+ 16 (<< 1 6))) ; jump-imm[0] if null
      (setq JE  144)  ; (+ 16 (<< 2 6))) ; jump-imm[0] if empty
      (setq JF  208)  ; (+ 16 (<< 3 6))) ; jump-imm[0] if false
      (setq JAF  11)  ; jump if arity failed
      (setq JAX  12)  ; JAF with packing extra arguments in list


      ; -------------------------------------------
      ;; Примитивные операции/операторы

      ; internal helpers
      (setq primop (lambda (name in out code)
         (vm:new TVECTOR name (ref code 0) in out code))) ; * makes primop record
      (setq make-bytecode (lambda (bytecode)
         (vm:alloc TBYTECODE bytecode))) ; * makes bytecode from list
      (setq list (lambda args args))

      ; -----------------------------------------------------------------------
      ; we already have (out of the box):    У нас изначально есть (из коробки):
      ;  quote lambda values brae             quote lambda values brae
      ;  setq let-eval ifeq values-apply      setq let-eval ifeq values-apply

      ; Список примитивных операций виртуальной машины:
      ;  эти операции не надо экспортировать из модуля, они как бы "вшиты" в базовый язык
      ; note: uncomment 'make-bytecode' if you want to change a primop bytecode
      (setq *primops* (list
         ; прямые аллокаторы
         (primop 'vm:new   'any 1 vm:new)   ; (make-bytecode '(23 N ...))) ;; make new object, simplest and fastest allocator
         (primop 'vm:make  'any 1 vm:make)  ; (make-bytecode '(18 N ...))) ;; make new object. slower, but smarter
         (primop 'vm:alloc 'any 1 vm:alloc) ; (make-bytecode '(82 N ...))) ;; make new binary object
         ; косвенные аллокаторы
         (primop 'vm:cast   2 1 vm:cast) ; (make-bytecode '(22 4 5 6    24 6)))
         (primop 'set-ref   3 1 set-ref) ; (make-bytecode '(10 4 5 6 7  24 7)))
         ; ну и мутаторы сюда же добавим
         (primop 'set-ref!  3 1 set-ref!) ; (make-bytecode '(74 4 5 6 7    24 7)))
         (primop 'vm:set! 'any 1 vm:set!) ; (make-bytecode '(43 N 4 5 . x  24 x)))

         ; описатели
         (primop 'type   1 1 type) ; (make-bytecode '(15 4 5    24 5)))  ;; get type bits
         (primop 'size   1 1 size) ; (make-bytecode '(36 4 5    24 5)))  ;; get object size (without header)

         ; конструкторы
         (primop 'cons   2 1 cons) ; (make-bytecode '(51 4 5 6  24 6)))

         ; геттеры
         (primop 'ref    2 1 ref) ; (make-bytecode '(47 4 5 6  24 6)))
         (primop 'car    1 1 car) ; (make-bytecode '(52 4 5    24 5)))  ;; speedup for (ref o 1)
         (primop 'cdr    1 1 cdr) ; (make-bytecode '(53 4 5    24 5)))  ;; speedup for (ref o 2)

         ; компараторы
         (primop 'eq?    2 1 eq?)   ; (make-bytecode '(54 4 5 6  24 6)))
         (primop 'less?  2 1 less?) ; (make-bytecode '(44 4 5 6  24 6)))

         ; базовая арифметика
         ; арифметические операции, которые возвращают пару(тройку) значений, использовать через let*/values-apply
         (primop 'vm:add   2 2 vm:add) ; (make-bytecode '(38 4 5     6 7)))
         (primop 'vm:mul   2 2 vm:mul) ; (make-bytecode '(39 4 5     6 7)))
         (primop 'vm:sub   2 2 vm:sub) ; (make-bytecode '(40 4 5     6 7)))
         (primop 'vm:div   3 3 vm:div) ; (make-bytecode '(26 4 5 6   7 8 9)))
         ; сдвиги
         (primop 'vm:shr   2 2 vm:shr) ; (make-bytecode '(58 4 5     6 7)))
         (primop 'vm:shl   2 2 vm:shl) ; (make-bytecode '(59 4 5     6 7)))
         ; бинарная арифметика
         (primop 'vm:and   2 1 vm:and) ; (make-bytecode '(55 4 5 6      24 6))) ;; 
         (primop 'vm:ior   2 1 vm:ior) ; (make-bytecode '(56 4 5 6      24 6))) ;; inclusive OR
         (primop 'vm:xor   2 1 vm:xor) ; (make-bytecode '(57 4 5 6      24 6))) ;; exclusive OR

         ; инструкции поддержки арифметики с плавающей точкой (inexact math)
         (primop 'vm:fp1   2 1 vm:fp1) ; (make-bytecode '(33 4 5 6      24 6)))
         (primop 'vm:fp2   3 1 vm:fp2) ; (make-bytecode '(34 4 5 6 7    24 7)))

         ; системный таймер  (deprecated, но остается как пример операции не принимающей параметров м возвращающей values)
         (primop 'clock    0 2 clock)  ; (make-bytecode '(61 4 5))) ; clock, todo: удалить
         ; системные вызовы
         (primop 'syscall 'any 1 syscall) ; (make-bytecode '(63 0 0))) ;; 63, system call
         ; additional applies
         (primop 'vector-apply 1 #f vector-apply) ; (make-bytecode '(32 0 0)))
         (primop 'ff-apply     1 #f ff-apply) ; (make-bytecode '(49 0 0 0 0 0)))

         ; associative array
         (primop 'ff:black  4 1 ff:black)  ; (make-bytecode '(42  4 5 6 7  8  24 8)))
         (primop 'ff:red    4 1 ff:red)    ; (make-bytecode '(106 4 5 6 7  8  24 8))) ; 106 = 42+(1<<6)
         (primop 'ff:toggle 1 1 ff:toggle) ; (make-bytecode '(46  4        5  24 5)))
         (primop 'ff:red?   1 1 ff:red?)   ; (make-bytecode '(41  4        5  24 5)))
         (primop 'ff:right? 1 1 ff:right?) ; (make-bytecode '(105 4        5  24 5))) ; 105 = 41+(1<<6)

         ; vm-specific constants
         (primop 'vm:version  0 1 vm:version)  ; (make-bytecode '(28 4)))
         (primop 'vm:features 0 1 vm:features) ; (make-bytecode '(29 4)))
         (primop 'vm:vmax     0 1 vm:vmax)     ; (make-bytecode '(30 4)))
         (primop 'vm:vsize    0 1 vm:vsize)    ; (make-bytecode '(31 4)))

         ; pinned objects
         (primop 'vm:pin    1 1 vm:pin)   ; (make-bytecode '(35 4 5  24 5)))
         (primop 'vm:unpin  1 1 vm:unpin) ; (make-bytecode '(60 4 5  24 5)))
         (primop 'vm:deref  1 1 vm:deref) ; (make-bytecode '(25 4 5  24 5)))

         ; stop
         (primop 'vm:exit   1 1 vm:exit)  ; (make-bytecode '(37 4 5  24 5)))
      ))

      ; additional
      (setq apply (make-bytecode '(20))) ; stub for (prim-opcodes)
      (setq arity-error (make-bytecode '(17))) ; stub for (prim-opcodes)
      (setq apply/cc (make-bytecode '(84))) ; stub for (prim-opcodes)

      ; other instructions
      (setq vm:nop  (make-bytecode '(21)))
      (setq vm:mcp  (make-bytecode '(27 4 5 6 7 8  24 8)))
      (setq vm:run  (make-bytecode '(50 4 5)))


      ; -----------------------------------
      ; internal, get primop opcode by name
      (setq opcode (lambda (op) ; * internal
         (let-eval (do) ((lambda (l)
                           (ifeq l #null
                              #false
                              (ifeq (ref (car l) 1) op
                                 (ref (car l) 2)
                                 (do (cdr l))))))
            (do *primops*))))

      ; notes:
      ;  vm:new - simplest and fastest allocator, creates only objects, can't create objects with more than 256 elements
      ;  vm:make - smarter allocator, can create sized objects with default element
      ;  vm:alloc - like vm:make, but for binary (raw, blob) objects
      (setq NEW (opcode 'vm:new))        ; no real (vm:new) command required, check rtl-primitive in (lang rtl)

      ; The origins of the names for CAR and CDR, on the other hand, are esoteric: CAR is an acronym from
      ; the phrase `Contents of the Address part of the Register'; and CDR (pronounced `could-er') is an
      ; acronym from the phrase `Contents of the Decrement part of the Register'. These phrases refer to
      ; specific pieces of hardware on the very early computer on which the original Lisp was developed.
      ; Besides being obsolete, the phrases have been completely irrelevant for more than 25 years to anyone
      ; thinking about Lisp. Nonetheless, although a few brave scholars have begun to use more reasonable
      ; names for these functions, the old terms are still in use.
      (setq CAR (opcode 'car))
      (setq CDR (opcode 'cdr))

      ; https://www.gnu.org/software/emacs/manual/html_node/eintr/Strange-Names.html#Strange-Names
      ; The name of the cons function is not unreasonable: it is an abbreviation of the word `construct'.
      ;(setq cons (make-bytecode '(51 4 5 6  24 6)))
      ;; notes:
      ;; 51 - CONS command bytecode
      ;; 4 5 - two default registers for input values (cons have two arguments)
      ;; 6 - one default register to store result (and one result)
      ;; 24 6 - RET 6, where 6 is results register


      ;; fixme: handle multiple return value primops sanely (now a list)
      ; для этих команд НЕ вставляется аргументом длина списка команд
      (setq multiple-return-variable-primops (list
         (opcode 'ff-apply)
         (opcode 'vm:add)
         (opcode 'vm:mul)
         (opcode 'vm:sub)
         (opcode 'vm:div)
         (opcode 'vm:shr)
         (opcode 'vm:shl)
         (opcode 'clock)))

      ; todo: check this and opcode-arity-ok-2? - maybe should merge this entities?
      ; todo: dynamically generate based on *primops*
      (setq variable-input-arity-primops (list
         (opcode 'vm:new)
         (opcode 'vm:make)
         (opcode 'vm:alloc)
         (opcode 'syscall)
         (opcode 'vm:set!)))

      (setq special-bind-primops (list
         (opcode 'vector-apply)
         (opcode 'ff-apply)))


      ; the best place for call/cc is here
      (setq call-with-current-continuation
         ('_sans_cps (lambda (k f)
                        (f k (lambda (c . x) (apply/cc k x))))))
                        ; accelerated version:
                        ;(f k
                        ;   (brae (lambda (c a) (k a))
                        ;   (brae (lambda (c a b) (k a b))
                        ;   (brae (lambda (c . x) (apply/cc k x))
                        ;         (lambda () (arity-error)))))))))

      (setq ARITY-ERROR (ref arity-error 0))

))
