;; vm rimops

;; todo: convert tuple to variable arity
;; todo: convert arity checks 17 -> 25

;; todo: maybe move ncar, and other "n" to the normal but with macroses on top level with type checking.
;; todo: переделать так, чтобы регистр возврата был самый первый, тогда можно будет обойтись без RET

(define-library (src vm)
   (export
      primops
      multiple-return-variable-primops
      variable-input-arity-primops
      special-bind-primops


      ; commands
      MOVE REFI MOVE2
      LD LDN LDT LDF
      CLOS0 CLOC0 CLOS1 CLOC1
      JEQ JZ JE JN JF JF2 JF2x
      GOTO

      RET ARITY-ERROR

      ; types
      TPAIR TTUPLE TSTRING TSYMBOL
      TBYTECODE TVPTR

      ; primitives
      MKT RAW
      CONS CAR CDR REF
      SET-REF SET-REF!
      EQ? LESS?
      ADD MUL SUB DIV
      SHR SHL
      AND OR XOR

      FF-APPLY


      TUPLE-APPLY
      )

   (begin
;      (define-syntax list
;         (syntax-rules ()
;            ((list) '())
;            ((list a . b)
;               (cons a (list . b)))))

;          итак, процесс замены кода операции на другой:
;          1. заводим новую операцию (например, как я сделал с raw)
;           (tuple 'raw2       62  2 1 (raw2 type-bytecode (list JF2 2 0 6  62 4 5 6 24 6  17)))
;          2. добавляем ее код в виртуальную машину
;          3. добавляем ее в список *src-olvm* в lang/eval.scm
;          4. пересобираем boot
;          5. переименовываем все вхождения старой команды в новую
;          6. пересобираем boot
;          7. меняем код старой команды на новый, пересобираем виртуальную машину
;          8. полностью(!) удаляем старую команду (из lang/eval.scm тоже)
;          9. пересобираем boot два раза
;          A. добавляем старую команду как новую, пересобираем, меняем raw2 на raw, пересобираем, удаляем raw2 полностью

;           пример добавления новой функции:
;           параметры: код функции, in параметров, out параметров, непосредственно код
;             код предваряется опкодом JF2 для проверки арности и (если не входит в список multiple-return-variable-primops)
;             возвратом результата и выводом ошибки при неправильной арности
;            (tuple 'cons       51  2 1 (raw (list JF2 3 0 6  51 4 5 6  RET 6  17) type-bytecode #false))  ;; 17 == ARITY-ERROR
;           вот еще несколько примеров
;            (tuple 'clock      61  0 2 (raw (list JF2 2 0 3  61 4 5           17) type-bytecode #false))  ;; must add 61 to the multiple-return-variable-primops list
;            (primop 'raw       '(60 4 5 6    7  24 7)  3 1)
;            (primop 'sys       '(27 4 5 6 7  8  24 8)  4 1)
;
;           пример выполнение raw-кода прямо в интерпретаторе:
;            > (define construct (raw type-bytecode (list 51 4 5 6 24 6)))
;            ;; Defined construct
;            > (construct 1 2)
;            '(1 . 2)
;            >
      ;(define listuple (raw type-bytecode '(35 4 5 6 7  24 7)))
      ;ff-bind

      ; todo: rename to TPAIR, TTUPLE, etc.

      (setq TPAIR              1) ; reference
      (setq TTUPLE             2) ; reference
      (setq TSTRING            3) ; reference, raw -> 35 (#b100000 + 3)?
      (setq TSYMBOL            4) ; reference
      ; 5   TODO(?): (define type-string-wide      5) ; reference, raw
      ; 6
      ; 7
      (setq type-ff-black-leaf     8) ; reference ; TODO: move to 28
      ; 9

      (setq type-rlist-spine      10) ; reference
      (setq type-vector-leaf      11) ; reference

      (setq TPORT             12) ; value
      (setq TCONST            13) ; value

      (setq type-rlist-node       14) ; reference
      (setq type-vector-dispatch  15) ; reference

      (setq TBYTECODE         16) ; reference, raw     ; declared functions (?)
      (setq type-proc             17) ; reference          ; from otus lisp bin (?)
      (setq type-clos             18) ; reference          ; from (import smth) (?)

      (setq type-vector-raw       19) ; reference, raw     ; see also TBVEC in c/ovm.c

      ; 20
      (setq type-string-dispatch  21) ; reference
      (setq type-string-wide      22) ; reference, raw
      ; 23

      ;; transitional trees or future ffs
      (setq type-ff               24) ; reference
      (setq type-ff-r             25) ; reference
      (setq type-ff-red           26) ; reference
      (setq type-ff-red-r         27) ; reference
      ; + type-ff-red, type-ff-right

      ;28
      ;29
      ;30

      (setq type-thread-state     31) ; reference
      (setq TVPTR             49) ; reference,  raw


      ;; Список кодов виртуальной машины:
      (setq RET 24)
      (setq ARITY-ERROR 17)

      ; set
      (setq MOVE  9) ; move a, t:      Ra -> Rt
      (setq REFI  1) ; refi a, p, t:   Ra[p] -> Rt, p unsigned
      (setq MOVE2 5) ; two moves, 4 args

      ; load
      (setq LD   14)  ; ld a, t:        Rt = a, signed byte
      (setq LDN  77)  ; (+ 13 (<< 1 6))) ; 77
      (setq LDT  141) ; (+ 13 (<< 2 6))) ; 141  ldt t:          Rt = true
      (setq LDF  205) ; (+ 13 (<< 3 6))) ; 205  ldf t:          Rt = false

      ; 
      (setq CLOS0 3) ; clos lp, o, nenv, e0 ... en, t:
      (setq CLOC0 4) ; cloc lp, o, nenv, e0 ... en, t:
      (setq CLOS1 6)
      (setq CLOC1 7)

      ; conditional jumps
      (setq JEQ   8) ; jeq a b o1 o2
      (setq JZ   16)  ;(+ 16 (<< 0 6))) ; jump-imm[0] if zero
      (setq JN   80)  ;(+ 16 (<< 1 6))) ; jump-imm[0] if null
      (setq JE   144) ;(+ 16 (<< 2 6))) ; jump-imm[0] if empty
      (setq JF   208) ;(+ 16 (<< 3 6))) ; jump-imm[0] if false
      (setq JF2  25) ; jump if arity failed
      (setq JF2x 89) ; (+ JF2 (<< 1 6))) ; JF2 with extra flag

      ; executions
      (setq GOTO 2) ; jmp a, nargs    call Ra with nargs args
      ;(setq GOTO-CODE 18) ; not used for now, check (fn-type)
      ;(setq GOTO-PROC 19) ; not used for now, check (fn-type)
      ;(setq GOTO-CLOS 21) ; not used for now, check (fn-type)

      ; primitives
      (setq TUPLE-APPLY 32)

      (setq MKT 23)      ;no real mkt command, check rtl-primitive in (lang compile)
      (setq RAW 60)      (setq raw     (raw TBYTECODE '(60 4 5 6  24 6)))

      ; https://www.gnu.org/software/emacs/manual/html_node/eintr/Strange-Names.html#Strange-Names
      ; The name of the cons function is not unreasonable: it is an abbreviation of the word `construct'.
      ; The origins of the names for car and cdr, on the other hand, are esoteric: car is an acronym from
      ; the phrase `Contents of the Address part of the Register'; and cdr (pronounced `could-er') is an
      ; acronym from the phrase `Contents of the Decrement part of the Register'. These phrases refer to
      ; specific pieces of hardware on the very early computer on which the original Lisp was developed.
      ; Besides being obsolete, the phrases have been completely irrelevant for more than 25 years to anyone
      ; thinking about Lisp. Nonetheless, although a few brave scholars have begun to use more reasonable
      ; names for these functions, the old terms are still in use.
      (setq CONS 51)     (setq cons    (raw TBYTECODE '(51 4 5 6  24 6)))

      (setq CAR 52)      (setq car     (raw TBYTECODE '(52 4 5    24 5)))
      (setq CDR 53)      (setq cdr     (raw TBYTECODE '(53 4 5    24 5)))
      (setq REF 47)      (setq ref     (raw TBYTECODE '(47 4 5 6  24 6)))
      
      (setq TYPE 15)     (setq type    (raw TBYTECODE '(15 4 5    24 5))) ;; get just the type bits (new)
      (setq SIZE 36)     (setq size    (raw TBYTECODE '(36 4 5    24 5))) ;; get object size (- 1)
      (setq CAST 22)     (setq cast    (raw TBYTECODE '(22 4 5 6  24 6))) ;; cast object type (works for immediates and allocated)
      (setq RAW? 48)     (setq raw?    (raw TBYTECODE '(48 4 5    24 5)))

      (setq SET-REF 45)  (setq set-ref  (raw TBYTECODE '(45 4 5 6 7  24 7)))
      (setq SET-REF! 10) (setq set-ref! (raw TBYTECODE '(10 4 5 6    24 6))) ; todo: change to like set-ref

      (setq EQ? 54)      (setq eq?     (raw TBYTECODE '(54 4 5 6  24 6)))
      (setq LESS? 44)    (setq less?   (raw TBYTECODE '(44 4 5 6  24 6)))

      ; арифметические операции, некоторые возвращают пару(тройку) значений, использовать через let*/apply-values
      (setq ADD 38)      (setq vm:add  (raw TBYTECODE '(38 4 5       6 7)))
      (setq MUL 39)      (setq vm:mul  (raw TBYTECODE '(39 4 5       6 7)))
      (setq SUB 40)      (setq vm:sub  (raw TBYTECODE '(40 4 5       6 7)))
      (setq DIV 26)      (setq vm:div  (raw TBYTECODE '(26 4 5 6     7 8 9)))
      (setq SHR 58)      (setq vm:shr  (raw TBYTECODE '(58 4 5       6 7)))
      (setq SHL 59)      (setq vm:shl  (raw TBYTECODE '(59 4 5       6 7)))

      (setq AND 55)      (setq vm:and  (raw TBYTECODE '(55 4 5 6  24 6)))
      (setq OR 56)       (setq vm:or   (raw TBYTECODE '(56 4 5 6  24 6)))
      (setq XOR 57)      (setq vm:xor  (raw TBYTECODE '(57 4 5 6  24 6)))

      ;(define vm:run  (raw type-bytecode '(50 4 5)))


      ; deprecated:
      ;(define clock   (raw type-bytecode '(61 4 5)))            ;; must add 61 to the multiple-return-variable-primops list

      (setq FF-APPLY 49) (setq ff-apply  (raw TBYTECODE '(49 4)))
      ;(define ff:red     (raw type-bytecode '(43 4 5 6 7  8  24 8)))
      ;(define ff:black   (raw type-bytecode '(42 4 5 6 7  8  24 8)))
      ;(define ff:toggle  (raw type-bytecode '(46 4        5  24 5)))
      ;(define ff:red?    (raw type-bytecode '(41 4        5  24 5)))
      ;(define ff:right?  (raw type-bytecode '(37 4        5  24 5)))

      ;(define syscall (raw type-bytecode '(63 4 5 6 7 8  24 8)))

      ;(define vm:version  (raw type-bytecode '(62 4)))
      ;(define fxmax       (raw type-bytecode '(33 4)))
      ;(define fxmbits     (raw type-bytecode '(34 4)))
      ;(define vm:wordsize (raw type-bytecode '(29 4)))


      (setq primops
         ; сейчас у этой операции нету проверки арности. возможно стоит ее вернуть (если ее будут использовать).
         ; todo: rename to vm:bytecode, vm:raw ?

         ; raw создает бинарную последовательность, mkt - последовательность объектов
         ; rename to vm:b и vm:o ?
         ; rename mkt to new?
         (cons (mkt TTUPLE 'mkt      MKT 'any 1 #f)  ;; mkt type v0 .. vn t (why #f?)
         (cons (mkt TTUPLE 'raw      RAW  2 1 raw)   ; (raw type-bytecode '(60 4 5 6  24 6)) ; '(JF2 2 0 6  60 4 5 6  RET 6  ARITY-ERROR)

         ; вторая по значимости команда
         (cons (mkt TTUPLE 'cons     CONS  2 1 cons)

         ; операции по работе с памятью
         (cons (mkt TTUPLE 'car      CAR  1 1 car)   ; (raw type-bytecode '(52 4 5    24 5))
         (cons (mkt TTUPLE 'cdr      CDR  1 1 cdr)   ; (raw type-bytecode '(53 4 5    24 5))
         (cons (mkt TTUPLE 'ref      REF  2 1 ref)   ; (raw type-bytecode '(47 4 5 6  24 6))   ; op47 = ref t o r = prim_ref(A0, A1)

         (cons (mkt TTUPLE 'type     TYPE  1 1 type)  ;; get just the type bits
         (cons (mkt TTUPLE 'size     SIZE  1 1 size)  ;; get object size (- 1)
         (cons (mkt TTUPLE 'cast     CAST  2 1 cast)  ;; cast object type (works for immediates and allocated)
         (cons (mkt TTUPLE 'raw?     RAW?  1 1 raw?)  ;; временное решение, пока не придумаю как удалить совсем

         (cons (mkt TTUPLE 'set-ref  SET-REF  3 1 set-ref)
         (cons (mkt TTUPLE 'set-ref! SET-REF! 3 1 set-ref!)

         ; компараторы
         (cons (mkt TTUPLE 'eq?      EQ?   2 1 eq?)
         (cons (mkt TTUPLE 'less?    LESS? 2 1 less?)

         ; базовая арифметика
         (cons (mkt TTUPLE 'vm:add   ADD  2 2 vm:add)
         (cons (mkt TTUPLE 'vm:mul   MUL  2 2 vm:mul)
         (cons (mkt TTUPLE 'vm:sub   SUB  2 2 vm:sub)
         (cons (mkt TTUPLE 'vm:div   DIV  3 3 vm:div) ; todo: change (vm:div hi lo b) to (vm:div lo hi b)
         ; сдвиги
         (cons (mkt TTUPLE 'vm:shr   SHR  2 2 vm:shr)
         (cons (mkt TTUPLE 'vm:shl   SHL  2 2 vm:shl)
         ; бинарная арифметика
         (cons (mkt TTUPLE 'vm:and   AND  2 1 vm:and)
         (cons (mkt TTUPLE 'vm:or    OR   2 1 vm:or)
         (cons (mkt TTUPLE 'vm:xor   XOR  2 1 vm:xor)

         ; системный таймер
         (cons (mkt TTUPLE 'clock    61  0 2 clock) ;; todo: удалить            must add 61 to the multiple-return-variable-primops list
         ; системные вызовы
         (cons (mkt TTUPLE 'syscall  63  4 1 syscall)

         ; vm-specific constants
         (cons (mkt TTUPLE 'vm:version  62  0 1 vm:version)
         (cons (mkt TTUPLE 'fxmax       30  0 1 fxmax)   ; todo: rename :may be vm:aimv - "atomic integer maximal value"?
         (cons (mkt TTUPLE 'fxmbits     31  0 1 fxmbits) ; todo: rename :may be vm:aimvl - "atomic integer maximal value length in bits"?
         (cons (mkt TTUPLE 'vm:wordsize 29  0 1 vm:wordsize)

         ; todo: add macro for call-with-tuple in r5rs
         (cons (mkt TTUPLE 'tuple-apply 32 1 #false tuple-apply)
         ; todo: rename to make-tuple,  vm:mkt?
         ; todo: rename to list->typedtuple ?
         (cons (mkt TTUPLE 'listuple    35 3 1  listuple)

         ; поддержка red-black деревьев
         (cons (mkt TTUPLE 'ff-apply   49 1 #f  ff-apply)

         (cons (mkt TTUPLE 'ff:red     43 4  1  ff:red)
         (cons (mkt TTUPLE 'ff:black   42 4  1  ff:black)
         (cons (mkt TTUPLE 'ff:toggle  46 1  1  ff:toggle)
         (cons (mkt TTUPLE 'ff:red?    41 1  1  ff:red?)
         (cons (mkt TTUPLE 'ff:right?  37 1  1  ff:right?)
         #null))))))))))))))))))))))))))))))))))))))
      ;(define *primitives* primops)


;      (define (get-primitive name)
;         (let loop ((p primops))
;            (if (eq? (ref (car p) 1) name)
;                (car p)
;                (loop (cdr p)))))


      ;; fixme: handle multiple return value primops sanely (now a list)
      ; для этих команд НЕ вставляется аргументом длина списка команд
      (setq multiple-return-variable-primops
         (cons FF-APPLY
         (cons 38 ; fx+, fx*, fx-, fx/, fx>>, fx<<
         (cons 39
         (cons 40
         (cons 26
         (cons 58
         (cons 59
         (cons 61 ; (clock)
         #null)))))))))

      (setq variable-input-arity-primops
         (cons MKT
         #null))

      (setq special-bind-primops
         (cons TUPLE-APPLY
         (cons FF-APPLY
         #null)))

;; Список sys-prim'ов
; поэтапный перевод sys-prim'ов в syscall'ы
; 1. добавить 100 к старым номерам
; 2. завести правильные новые
; 3. удалить старые

;      (define (__fsend) (sys-prim 0 #false #false #false))
;      1 __fopen
;      2 __close
;      3 __sopen
;      4 __accept

;      5 __fread
;      +6 __exit
;      +7 __set-memory-limit
;      +8 __get-machine-word-size
;      +9 __get-memory-limit
;      +10 __enter-linux-seccomp
;      +22 __set-ticker-value

;      30 __dlopen
;      31 __dlsym
;      32 __pinvoke
;      33 __gc ; TEMP

;      11 __sys-open-dir
;      12 __sys-read-dir
;      13 __sys-closedir
;      14 __set-ticks
;      15 __fsocksend
;      16 __getenv
;      17 __exec[v]
;      20 __chdir
;      19 wait <pid> <respair>
;      18 fork
;      21 kill

;      ;; special things exposed by the vm
;      (define (set-memory-limit n) (sys-prim 12 n #f #f))
;      (define (get-word-size)      (sys-prim 1008 #false #false #false))
;      (define (get-memory-limit)   (sys-prim 12 #false #false #false))
;      (define (start-seccomp)      (sys-prim 1010 #false #false #false)) ; not enabled by defa
;
;      ;; stop the vm *immediately* without flushing input or anything else with return value n
;      (define (halt n)             (sys-prim 60 n n n))
;      ;; make thread sleep for a few thread scheduler rounds
;      (define (set-ticker-value n) (sys-prim 1022 n #false #false))
;      (define (wait n)
;         (if (eq? n 0)
;            n
;            (let* ((n _ (fx- n 1)))
;               (set-ticker-value 0)
;               (wait n))))



; проверку типов вынесем на уровень компилятора!
; можно и в отдельный файл
;      ; from interop.scm
;      (define (interop op a b)
;         (call/cc (λ (resume) (sys resume op a b))))
;      (define (error reason info)
;         (interop 5 reason info))
;      (define (pair? x) (eq? type-pair (type x))) ; list.scm
;      (define (fixnum? x)
;         (let ((t (type x)))
;            (or
;               (eq? t type-fix+)
;               (eq? t type-fix-)
;               )))
))
