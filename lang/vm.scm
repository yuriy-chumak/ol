;; vm rimops

;; todo: convert tuple to variable arity
;; todo: convert arity checks 17 -> 25

;; todo: maybe move ncar, and other "n" to the normal but with macroses on top level with type checking.
;; todo: переделать так, чтобы регистр возврата был самый первый, тогда можно будет обойтись без RET

(define-library (lang vm)
   (export
      primops
      multiple-return-variable-primops
      variable-input-arity-primops
      special-bind-primops
      )

   (begin
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

      ;; *** если кому хочется заменить коды операций - это можно сделать тут ***
      ;(define raw     (raw type-bytecode '(60 4 5 6  24 6)))

      ;(define vm:version  (raw type-bytecode '(62 4)))
      ;(define fxmax       (raw type-bytecode '(33 4)))
      ;(define fxmbits     (raw type-bytecode '(34 4)))
      ;(define vm:wordsize (raw type-bytecode '(29 4)))

      ; https://www.gnu.org/software/emacs/manual/html_node/eintr/Strange-Names.html#Strange-Names
      ; The name of the cons function is not unreasonable: it is an abbreviation of the word `construct'.
      ; The origins of the names for car and cdr, on the other hand, are esoteric: car is an acronym from
      ; the phrase `Contents of the Address part of the Register'; and cdr (pronounced `could-er') is an
      ; acronym from the phrase `Contents of the Decrement part of the Register'. These phrases refer to
      ; specific pieces of hardware on the very early computer on which the original Lisp was developed.
      ; Besides being obsolete, the phrases have been completely irrelevant for more than 25 years to anyone
      ; thinking about Lisp. Nonetheless, although a few brave scholars have begun to use more reasonable
      ; names for these functions, the old terms are still in use.
      ;(define cons    (raw type-bytecode '(51 4 5 6  24 6)))

      ;(define type    (raw type-bytecode '(15 4 5    24 5))) ;; get just the type bits (new)
      ;(define size    (raw type-bytecode '(36 4 5    24 5))) ;; get object size (- 1)
      ;(define cast    (raw type-bytecode '(22 4 5 6  24 6))) ;; cast object type (works for immediates and allocated)
      ;(define raw?    (raw type-bytecode '(48 4 5    24 5)))

      ;(define car     (raw type-bytecode '(52 4 5    24 5)))
      ;(define cdr     (raw type-bytecode '(53 4 5    24 5)))
      ;(define ref     (raw type-bytecode '(47 4 5 6  24 6)))   ; op47 = ref t o r = prim_ref(A0, A1)
      ;(define set-ref  (raw type-bytecode '(45 4 5 6 7  24 7)))

      ;(define set-ref! (raw type-bytecode '(10 4 5 6  24 6)))
      ;moved to the r5rs, (define set-car!(raw type-bytecode '(11 4 5 6  24 6)))
      ;moved to the r5rs, (define set-cdr!(raw type-bytecode '(12 4 5 6  24 6)))

      ;(define eq?     (raw type-bytecode '(54 4 5 6  24 6)))
      ;(define less?   (raw type-bytecode '(44 4 5 6  24 6)))

      ; арифметические операции, некоторые возвращают пару(тройку) значений, использовать через let*/apply-values
      ;(define vm:add  (raw type-bytecode '(38 4 5       6 7)))     ;'(38 4 5    6 7  )
      ;(define vm:mul  (raw type-bytecode '(39 4 5       6 7)))
      ;(define vm:sub  (raw type-bytecode '(40 4 5       6 7)))
      ;(define vm:div  (raw type-bytecode '(26 4 5 6     7 8 9)))
      ;(define vm:shr  (raw type-bytecode '(58 4 5       6 7)))
      ;(define vm:shl  (raw type-bytecode '(59 4 5       6 7)))

      ;(define vm:and  (raw type-bytecode '(55 4 5 6  24 6)))
      ;(define vm:or   (raw type-bytecode '(56 4 5 6  24 6)))
      ;(define vm:xor  (raw type-bytecode '(57 4 5 6  24 6)))

      ;(define vm:run  (raw type-bytecode '(50 4 5)))


      ; deprecated:
      ;(define clock   (raw type-bytecode '(61 4 5)))            ;; must add 61 to the multiple-return-variable-primops list

      ;(define ff-apply  (raw type-bytecode '(49 4)))
      ;(define ff:red     (raw type-bytecode '(43 4 5 6 7  8  24 8)))
      ;(define ff:black   (raw type-bytecode '(42 4 5 6 7  8  24 8)))
      ;(define ff:toggle  (raw type-bytecode '(46 4        5  24 5)))
      ;(define ff:red?    (raw type-bytecode '(41 4        5  24 5)))
      ;(define ff:right?  (raw type-bytecode '(37 4        5  24 5)))

      ;(define syscall (raw type-bytecode '(63 4 5 6 7 8  24 8)))

      (define-syntax list
         (syntax-rules ()
            ((list) '())
            ((list a . b)
               (cons a (list . b)))))

      ;; Список кодов виртуальной машины:
      (setq JF2 25)
      (setq RET 24)
      (setq ARITY-ERROR 17)

      (setq TUPLE-APPLY 32)
      (setq MKT 23)
      (setq FF-APPLY 49)

      (setq primops
         ; сейчас у этой операции нету проверки арности. возможно стоит ее вернуть (если ее будут использовать).
         ; todo: rename to vm:bytecode, vm:raw ?

         ; raw создает бинарную последовательность, mkt - последовательность объектов
         ; rename to vm:b и vm:o ?
         (cons (mkt 2 'raw      60  2 1 raw)   ; (raw type-bytecode '(60 4 5 6  24 6)) ; '(JF2 2 0 6  60 4 5 6  RET 6  ARITY-ERROR)
         (cons (mkt 2 'mkt      23 'any 1 #f)  ;; mkt type v0 .. vn t (why #f?)

         ; вторая по значимости команда
         (cons (mkt 2 'cons     51  2 1 cons)

         ; операции по работе с памятью
         (cons (mkt 2 'car      52  1 1 car)   ; (raw type-bytecode '(52 4 5    24 5))
         (cons (mkt 2 'cdr      53  1 1 cdr)   ; (raw type-bytecode '(53 4 5    24 5))
         (cons (mkt 2 'ref      47  2 1 ref)   ; (raw type-bytecode '(47 4 5 6  24 6))   ; op47 = ref t o r = prim_ref(A0, A1)

         (cons (mkt 2 'type     15  1 1 type)  ;; get just the type bits
         (cons (mkt 2 'size     36  1 1 size)  ;; get object size (- 1)
         (cons (mkt 2 'cast     22  2 1 cast)  ;; cast object type (works for immediates and allocated)
         (cons (mkt 2 'raw?     48  1 1 raw?)  ;; временное решение, пока не придумаю как удалить совсем

         (cons (mkt 2 'set-ref  45  3 1 set-ref)
         (cons (mkt 2 'set-ref! 10  3 1 set-ref!)

         ; компараторы
         (cons (mkt 2 'eq?      54  2 1 eq?)
         (cons (mkt 2 'less?    44  2 1 less?)

         ; базовая арифметика
         (cons (mkt 2 'vm:add   38  2 2 vm:add)
         (cons (mkt 2 'vm:mul   39  2 2 vm:mul)
         (cons (mkt 2 'vm:sub   40  2 2 vm:sub)
         (cons (mkt 2 'vm:div   26  3 3 vm:div) ; todo: change (vm:div hi lo b) to (vm:div lo hi b)
         ; сдвиги
         (cons (mkt 2 'vm:shr   58  2 2 vm:shr)
         (cons (mkt 2 'vm:shl   59  2 2 vm:shl)
         ; бинарная арифметика
         (cons (mkt 2 'vm:and   55  2 1 vm:and)
         (cons (mkt 2 'vm:or    56  2 1 vm:or)
         (cons (mkt 2 'vm:xor   57  2 1 vm:xor)

         ; системный таймер
         (cons (mkt 2 'clock    61  0 2 clock) ;; todo: удалить            must add 61 to the multiple-return-variable-primops list
         ; системные вызовы
         (cons (mkt 2 'syscall  63  4 1 syscall)

         ; vm-specific constants
         (cons (mkt 2 'vm:version  62  0 1 vm:version)
         (cons (mkt 2 'fxmax       30  0 1 fxmax)   ; todo: rename :may be vm:aimv - "atomic integer maximal value"?
         (cons (mkt 2 'fxmbits     31  0 1 fxmbits) ; todo: rename :may be vm:aimvl - "atomic integer maximal value length in bits"?
         (cons (mkt 2 'vm:wordsize 29  0 1 vm:wordsize)

         ; todo: add macro for call-with-tuple in r5rs
         (cons (mkt 2 'tuple-apply 32 1 #false tuple-apply)
         ; todo: rename to make-tuple,  vm:mkt?
         ; todo: rename to list->typedtuple ?
         (cons (mkt 2 'listuple    35 3 1  listuple)

         ; поддержка red-black деревьев
         (cons (mkt 2 'ff-apply   49 1 #f  ff-apply)

         (cons (mkt 2 'ff:red     43 4  1  ff:red)
         (cons (mkt 2 'ff:black   42 4  1  ff:black)
         (cons (mkt 2 'ff:toggle  46 1  1  ff:toggle)
         (cons (mkt 2 'ff:red?    41 1  1  ff:red?)
         (cons (mkt 2 'ff:right?  37 1  1  ff:right?)
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
         (list
            FF-APPLY

            38 39 40 26 58 59 ; fx+, fx*, fx-, fx/, fx>>, fx<<
            61 ; (clock)
            ))
      (setq variable-input-arity-primops
         (list
            MKT))

      (setq special-bind-primops
         (list
            TUPLE-APPLY
            FF-APPLY))

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
