; possible todos
; todo: convert tuple to variable arity
; todo: convert arity checks 17 -> 25
; todo: переделать так, чтобы регистр возврата был самый первый, тогда можно будет обойтись без RET
; todo: план по слиянию new-object и new-raw-object, с одновременным
;       внесением бита "rawness" в числовое значение типа
;  1. добавить параметр "rawness" к аргументам new-object
;  2. удалить new-raw-object
;  3. изменить нумерацию типов
;  4. удалить параметр rawness
; todo: change (vm:div hi lo b) to (vm:div lo hi b)
; todo: change TFIX to TSHORT, TINT to TLONG
;  or to TINTEGER, TBIGNUM

(define-library (src vm)
   (export
      apply apply/cc arity-error
      call-with-current-continuation
      vm:run vm:sys ; internal commands, don't act as primop

      *primops* ; global list of primitive operations with parameters
      multiple-return-variable-primops
      variable-input-arity-primops special-bind-primops

      ; low level (assembly) commands
      GOTO RET ARITY-ERROR  ; used by (lang assemply) etc.
      JEQ JZ JE JN JF JAF JAFX
      CLOS0 CLOC0 CLOS1 CLOC1
      LD LDE LDN LDT LDF
      MOVE REFI MOVE2

      CAR CDR NEW    ; used by (lang compile), (lang eval)

      ; types
      TFIX+ TFIX- TINT+ TINT- TRATIONAL TCOMPLEX TINEXACT
      TPAIR TSYMBOL TTUPLE TVECTOR
      TSTRING TSTRINGWIDE
      TBYTECODE TPROCEDURE TCLOSURE
      TVPTR TCALLABLE)

   (begin
      ; -----------------------------------------------------------------------
      ; Virtual Machine Types List       Список типов данных виртуальной машины

      (setq TFIX+              0) ; value
      (setq TFIX-             32) ; value
      (setq TINT+             40) ; reference
      (setq TINT-             41) ; reference
      (setq TRATIONAL         42) ; reference
      (setq TCOMPLEX          43) ; reference
      (setq TINEXACT          44) ; reference, IEEE 754 64-bit binary

      (setq TPAIR              1) ; reference
      (setq TTUPLE             2) ; reference
      (setq TVECTOR            2) ; reference
      (setq TSTRING            3) ; reference, raw -> 35 (#b100000 + 3)?
      (setq TSYMBOL            4) ; reference
      (setq TSTRINGWIDE        5) ; reference, raw
      ; 5   TODO(?): (define type-string-wide      5) ; reference, raw
      ; 6
      ; 7
      ;(setq type-ff-black-leaf 8) ; reference ; TODO: move to 28
      ; 9

      ;(setq type-rlist-spine  10) ; reference, used by rlist (not retested)
      (setq type-vector-leaf  11) ; reference

      (setq TPORT             12) ; value
      (setq TCONST            13) ; value

      ;(setq type-rlist-node   14) ; reference, used by rlist (not retested)
      (setq type-vector-dispatch 15) ; reference

      (setq TBYTECODE         16) ; reference, raw bytecode
      (setq TPROCEDURE        17) ; reference, pure function
      (setq TCLOSURE          18) ; reference, function with closures

      (setq TBYTEVECTOR       19) ; reference, bytevector
      ; 20
      (setq type-string-dispatch 21) ; reference
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

      (setq type-thread-state 31) ; reference

      (setq TVPTR             49) ; reference, blob
      (setq TCALLABLE         61) ; reference, blob


      ; -------------------------------------------
      ;; Список кодов виртуальной машины, используемых
      ;;  компилятором, которые не экспозятся на сам язык

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

      ; internal helpers
      (setq primop (lambda (name in out code)
         (vm:new TTUPLE name (ref code 0) in out code))) ; * makes primop record
      (setq make-bytecode (lambda (bytecode)
         (vm:makeb TBYTECODE bytecode))) ; * makes bytecode from list
      (setq alist (lambda args args))

      ; -----------------------------------------------------------------------
      ; we already have (out of the box):    У нас изначально есть (из коробки):
      ;  quote lambda values either           quote lambda values either
      ;  setq letq ifeq values-apply          setq letq ifeq values-apply

      ; Список примитивных операций виртуальной машины:
      ;  эти операции не надо экспортировать из модуля, они как бы "вшиты" в базовый язык
      (setq *primops* (alist
         ; прямые аллокаторы
         (primop 'vm:new   'any 1 #(23)) ; make new object, simplest and fastest allocator
         (primop 'vm:make  'any 1 #(18)) ; make object
         (primop 'vm:makeb 'any 1 #(19)) ; make blob (binary, raw) object
         ; косвенные аллокаторы
         (primop 'vm:cast     2 1 (make-bytecode '(22 4 5 6    24 6)))
         (primop 'set-ref     3 1 (make-bytecode '(45 4 5 6 7  24 7)))

         ; ну и мутатор сюда же добавим
         (primop 'set-ref!    3 1 (make-bytecode '(10 4 5 6 7  24 7)))

         ; описатели
         (primop 'type   1 1 type) ; (make-bytecode '(15 4 5    24 5))  ;; get type bits
         (primop 'size   1 1 size) ; (make-bytecode '(36 4 5    24 5))  ;; get object size (without header)

         ; конструкторы
         (primop 'cons   2 1 cons) ; (make-bytecode '(51 4 5 6  24 6))

         ; геттеры
         (primop 'car    1 1 car)  ; (make-bytecode '(52 4 5    24 5))
         (primop 'cdr    1 1 cdr)  ; (make-bytecode '(53 4 5    24 5))
         (primop 'ref    2 1 ref)  ; (make-bytecode '(47 4 5 6  24 6))

         ; компараторы
         (primop 'eq?    2 1 eq?)  ; (make-bytecode '(54 4 5 6  24 6))
         (primop 'less?  2 1 less?); (make-bytecode '(44 4 5 6  24 6))

         ; базовая арифметика
         ; арифметические операции, которые возвращают пару(тройку) значений, использовать через let*/values-apply
         (primop 'vm:add   2 2 (make-bytecode '(38 4 5     6 7)))  ; vm:add
         (primop 'vm:mul   2 2 (make-bytecode '(39 4 5     6 7)))  ; vm:mul
         (primop 'vm:sub   2 2 (make-bytecode '(40 4 5     6 7)))  ; vm:sub
         (primop 'vm:div   3 3 (make-bytecode '(26 4 5 6   7 8 9))); vm:div
         ; сдвиги
         (primop 'vm:shr   2 2 (make-bytecode '(58 4 5     6 7)))  ; vm:shr
         (primop 'vm:shl   2 2 (make-bytecode '(59 4 5     6 7)))  ; vm:shl
         ; бинарная арифметика
         (primop 'vm:and   2 1 (make-bytecode '(55 4 5 6      24 6))) ; vm:and
         (primop 'vm:or    2 1 (make-bytecode '(56 4 5 6      24 6))) ; vm:or
         (primop 'vm:xor   2 1 (make-bytecode '(57 4 5 6      24 6))) ; vm:xor

         ; инструкции поддержки арифметики с плавающей точкой (inexact math)
         (primop 'vm:fp1   2 1 (make-bytecode '(33 4 5 6      24 6))) ; vm:fp1
         (primop 'vm:fp2   3 1 (make-bytecode '(34 4 5 6 7    24 7))) ; vm:fp2

         ; системный таймер  (deprecated, но остается как пример операции не принимающей параметров м возвращающей values)
         (primop 'clock    0 2 (make-bytecode '(61 4 5))) ; clock, todo: удалить
         ; системные вызовы
         (primop 'syscall  4 1 (make-bytecode '(63 4 5 6 7 8  24 8)))

         ; vm-specific constants
         (primop 'vm:maxvalue   0 1 (make-bytecode '(30 4)))
         (primop 'vm:valuewidth 0 1 (make-bytecode '(31 4)))
         (primop 'vm:version    0 1 (make-bytecode '(28 4)))
         (primop 'vm:features   0 1 (make-bytecode '(29 4)))

         (primop 'tuple-apply  1 #f (make-bytecode '(32 4)))
         (primop 'ff-apply     1 #f (make-bytecode '(49 4)))

         ; associative array
         (primop 'ff:black  4 1 (make-bytecode '(42  4 5 6 7  8  24 8)))
         (primop 'ff:red    4 1 (make-bytecode '(106 4 5 6 7  8  24 8))) ; 106 = 42+(1<<6)
         (primop 'ff:toggle 1 1 (make-bytecode '(46  4        5  24 5)))
         (primop 'ff:red?   1 1 (make-bytecode '(41  4        5  24 5)))
         (primop 'ff:right? 1 1 (make-bytecode '(105 4        5  24 5))) ; 105 = 41+(1<<6)

         ; pinned objects
         (primop 'vm:pin    1 1 (make-bytecode '(35 4 5  24 5)))
         (primop 'vm:unpin  1 1 (make-bytecode '(60 4 5  24 5)))
         (primop 'vm:deref  1 1 (make-bytecode '(25 4 5  24 5)))
      ))

      ; additional
      (setq apply (make-bytecode '(20))) ; stub for (prim-opcodes)
      (setq arity-error (make-bytecode '(17))) ; stub for (prim-opcodes)
      (setq apply/cc (make-bytecode '(84))) ; stub for (prim-opcodes)

      ; other instructions
      (setq vm:nop  (make-bytecode '(21)))
      (setq vm:sys  (make-bytecode '(27 4 5 6 7 8  24 8)))
      (setq vm:run  (make-bytecode '(50 4 5)))


      ; -----------------------------------
      ; internal, get primop opcode by name
      (setq opcode (lambda (op) ; * internal
         (letq (do) ((lambda (l)
                        (ifeq l #null #false
                           (ifeq (ref (car l) 1) op (ref (car l) 2) (do (cdr l))))))
            (do *primops*))))

      ; notes:
      ;  vm:new - simplest and fastest allocator, creates only objects, can't create objects with more than 256 elements
      ;  vm:make - smarter allocator, can create objects with size and default element
      ;  vm:makeb - same as vm:make, but for binary (raw, blob) objects
      (setq NEW (opcode 'vm:new))        ; no real (vm:new) command required, check rtl-primitive in (lang compile)

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
      (setq multiple-return-variable-primops (alist
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
      (setq variable-input-arity-primops (alist ; todo: move to other module
         (opcode 'vm:new)
         (opcode 'vm:make)
         (opcode 'vm:makeb)))

      (setq special-bind-primops (alist
         (opcode 'tuple-apply)
         (opcode 'ff-apply)))


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

      (setq ARITY-ERROR (opcode 'arity-error))

))
