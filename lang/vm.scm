;; vm rimops

;; todo: convert tuple to variable arity
;; todo: convert arity checks 17 -> 25

;; todo: maybe move ncar, and other "n" to the normal but with macroses on top level with type checking.

(define-library (lang vm)
   (export
      primops
      *primitives*
      primop-name ;; primop → symbol | primop

      multiple-return-variable-primops
      variable-input-arity?
      special-bind-primop?)

   (import
      (r5rs base))

   (begin
;          итак, процесс замены кода операции на другой:
;          1. заводим новую операцию (например, как я сделал с raw)
;           (tuple 'raw2       62  2 1 (raw2 type-bytecode (list JF2 2 0 6  62 4 5 6 24 6  17)))
;          2. добавляем ее код в виртуальную машину
;          3. добавляем ее в список *owl-core* в lang/eval.scm
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
;            (define *interactive* #t)
;            ;; Defined *interactive*
;            > (define x (raw (list 51 4 5 6 24 6) type-bytecode #false))
;            ;; Defined x
;            > (x 1 2)
;            '(1 . 2)
;            >

      ;; Список кодов виртуальной машины:
      (define JF2 25)
      (define RET 24)
      (define ARITY-ERROR 17)

      (define BIND 32)
      (define FFBIND 49)
      (define MKT 23)

      ;(define bind    (raw type-bytecode '(32 4)))
      ;(define ff-bind (raw type-bytecode '(49 4)))
      ;ff-bind

      ;; *** если кому хочется заменить коды операций - это можно сделать тут ***

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

      ;(define car     (raw type-bytecode '(52 4 5    24 5)))
      ;(define cdr     (raw type-bytecode '(53 4 5    24 5)))
      ;(define ref     (raw type-bytecode '(47 4 5 6  24 6)))   ; op47 = ref t o r = prim_ref(A0, A1)
      ;(define set     (raw type-bytecode '(45 4 5 6 7  24 7))) ; (set tuple pos val) -> tuple'

      ;(define set-car!(raw type-bytecode '(11 4 5 6  24 6)))
      ;(define set-cdr!(raw type-bytecode '(12 4 5 6  24 6)))

      ;(define eq?     (raw type-bytecode '(54 4 5 6  24 6)))
      ;(define lesser? (raw type-bytecode '(44 4 5 6  24 6)))

      ;(define ncons   (raw type-bytecode '(29 4 5 6  24 6)))
      ;(define ncar    (raw type-bytecode '(30 4 5    24 5)))
      ;(define ncdr    (raw type-bytecode '(31 4 5    24 5)))

      ;(define fxband  (raw type-bytecode '(55 4 5 6  24 6)))
      ;(define fxbor   (raw type-bytecode '(56 4 5 6  24 6)))
      ;(define fxbxor  (raw type-bytecode '(57 4 5 6  24 6)))
      
      ;(define refb    (raw type-bytecode '(48 4 5 6  24 6)))
      ;(define sizeb   (raw type-bytecode '(28 4 5    24 5)))

      ; арифметические операции, возвращают пару(тройку) значений, использовать через let*/receive
      ;(define fx+   (raw type-bytecode '(38 4 5    6 7)))     ;'(38 4 5    6 7  )
      ;(define fx*   (raw type-bytecode '(39 4 5    6 7)))
      ;(define fx-   (raw type-bytecode '(40 4 5    6 7)))
      ;(define fx/   (raw type-bytecode '(26 4 5 6  7 8 9)))
      ;(define fx>>  (raw type-bytecode '(58 4 5    6 7)))
      ;(define fx<<  (raw type-bytecode '(59 4 5    6 7)))

      ; 
      ;(define clock   (raw type-bytecode '(61 4 5)))            ;; must add 61 to the multiple-return-variable-primops list

      ;(define listuple  (raw type-bytecode '(35 4 5 6 7  24 7)))
      ;(define mkblack   (raw type-bytecode '(42 4 5 6 7 8  24 8)))
      ;(define mkred     (raw type-bytecode '(43 4 5 6 7 8  24 8)))
      ;(define red?      (raw type-bytecode '(41 4 5  24 5)))
      ;(define ff-toggle (raw type-bytecode '(46 4 5  24 5)))

      ;(define sys-prim (raw type-bytecode '(63 4 5 6 7 8  24 8))) ; todo: rename sys-prim to syscall
      ;(define syscall (raw type-bytecode '(63 4 5 6 7 8  24 8))) ; todo: rename sys-prim to syscall

      (define primops (list
         ; пара специальных вещей (todo - переименовать их в что-то вроде %%bind, так как это внутренние команды компилятора)
         (tuple 'bind     BIND    1 #false bind)    ;; (bind thing (lambda (name ...) body)), fn is at CONT so arity is really 1
         (tuple 'mkt      MKT     'any   1 #f)      ;; mkt type v0 .. vn t (отдельно, похоже, не используется)
         (tuple 'ff-bind  FFBIND  1 #false ff-bind) ;; SPECIAL ** (ffbind thing (lambda (name ...) body)) 
         ; сейчас у этой операции нету проверки арности. возможно стоит ее вернуть (если ее будут использовать).
         (tuple 'raw      60  2 1 (raw type-bytecode '(60 4 5 6  24 6))) ; '(JF2 2 0 6  60 4 5 6  RET 6  ARITY-ERROR)))

         ; вторая по значимости команда
         (tuple 'cons     51  2 1 cons)

         ; операции по работе с памятью
         (tuple 'type     15  1 1 type)  ;; get just the type bits (new)
         (tuple 'size     36  1 1 size)  ;; get object size (- 1)
         (tuple 'cast     22  2 1 cast)  ;; cast object type (works for immediates and allocated)

         (tuple 'car      52  1 1 car)   ;(raw type-bytecode '(52 4 5    24 5)))
         (tuple 'cdr      53  1 1 cdr)   ;(raw type-bytecode '(53 4 5    24 5)))
         (tuple 'ref      47  2 1 ref)   ;(raw type-bytecode '(47 4 5 6  24 6)))   ; op47 = ref t o r = prim_ref(A0, A1)
         
         (tuple 'set      45  3 1 set)   ;(raw type-bytecode '(45 4 5 6 7  24 7))) ; (set tuple pos val) -> tuple'
         (tuple 'set-car! 11  2 1 set-car!) ;(raw type-bytecode '(11 4 5 6  24 6)))
         (tuple 'set-cdr! 12  2 1 set-cdr!) ;(raw type-bytecode '(12 4 5 6  24 6)))
         ;tuple 'set!     10  3 1 set!) ; '(10 4 5 6  7  24 7)  3 1) ; (set! tuple pos val)

         (tuple 'eq?      54  2 1 eq?)
         (tuple 'lesser?  44  2 1 lesser?)

         ; поддержка больших чисел
         ;; вопрос - а нахрена именно числовой набор функций? может обойдемся обычным - cons, car, cdr?
         (tuple 'ncons    29  2 1 ncons)
         (tuple 'ncar     30  1 1 ncar)
         (tuple 'ncdr     31  1 1 ncdr)
         
         ;; логика
         (tuple 'fxband   55  2 1 fxband)
         (tuple 'fxbor    56  2 1 fxbor)
         (tuple 'fxbxor   57  2 1 fxbxor)
         
         ;; строки
         (tuple 'refb     48  2 1 refb)
         (tuple 'sizeb    28  1 1 sizeb)

         ; базовая математика
         (tuple 'fx+      38  2 2 fx+)
         (tuple 'fx*      39  2 2 fx*)
         (tuple 'fx-      40  2 2 fx-)
         (tuple 'fx/      26  3 3 fx/)
         (tuple 'fx>>     58  2 2 fx>>)
         (tuple 'fx<<     59  2 2 fx<<)

         ; системный таймер
         (tuple 'clock    61  0 2 clock)            ;; must add 61 to the multiple-return-variable-primops list

         ; syscall интерфейс
         (tuple 'sys-prim 63  4 1 sys-prim)
         (tuple 'syscall  63  4 1 syscall)

         ; поддержка ff деревьев
         (tuple 'listuple  35  3 1 listuple)
         (tuple 'mkblack   42  4 1 mkblack)
         (tuple 'mkred     43  4 1 mkred)
         (tuple 'red?      41  1 1 red?)
         (tuple 'ff-toggle 46  1 1 ff-toggle)
      ))
      (define *primitives* primops)


      (define (get-primitive name)
         (let loop ((p primops))
            (if (eq? (ref (car p) 1) name)
                (car p)
                (loop (cdr p)))))


      ;; fixme: handle multiple return value primops sanely (now a list)
      (define multiple-return-variable-primops
         (list
            ; а почему bind не multiple-return?            (ref (get-primitive 'bind) 2) ; 32 (?)
            FFBIND

            38 39 40 26 58 59 ; fx+, fx*, fx-, fx/, fx>>, fx<<
            61 ; (clock)
            ))

      ;; from cps
      (define (special-bind-primop? op) ; bind ar ff-bind
         (or (eq? op BIND)
             (eq? op FFBIND)))

      (define (variable-input-arity? op)
         (or (eq? op MKT)))

      ;; non-primop instructions that can report errors
      (define (instruction-name op)
         (cond
            ((eq? op ARITY-ERROR) 'arity-error)
            (else #false)))
         
      ; используется в выводе сообщений "инструкция такая-то сфейлила"
      (define (primop-name pop)
         (let ((pop (fxband pop 63))) ; ignore top bits which sometimes have further data
            (or
               (instruction-name pop)
               (let loop ((primops primops))
                  (cond
                     ((null? primops) pop)
                     ((eq? pop (ref (car primops) 2))
                        (ref (car primops) 1))
                     (else
                        (loop (cdr primops))))))))

;; run, apply, apply-cont - moved to the right places (r5rs, lang/thread)
;      (define apply      (raw type-bytecode (list 20))) ;; <- no arity, just call 20
;      (define apply-cont (raw type-bytecode (list (fxbor 20 #x40))))
;      (define run        (raw type-bytecode (list JF2 3 0 6  50 4 5 6  24 6  ARITY-ERROR)))
;      (define apply      (raw type-bytecode (list 20))) ;; <- no arity, just call 20
;      (define apply-cont (raw type-bytecode (list (fxbor 20 #x40)))) - used in (r5rs base) in call-with-current-continuation
;      (define run        (raw type-bytecode (list JF2 3 0 6  50 4 5 6  24 6  ARITY-ERROR)))


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

      ;; used syscalls
      (define (exec function . args) (sys-prim 59 function args #false))

;      ;; special things exposed by the vm
;      (define (set-memory-limit n) (sys-prim 1007 n n n))
;      (define (get-word-size)      (sys-prim 1008 #false #false #false))
;      (define (get-memory-limit)   (sys-prim 1009 #false #false #false))
;      (define (start-seccomp)      (sys-prim 1010 #false #false #false)) ; not enabled by defa
;
;      ;; stop the vm *immediately* without flushing input or anything else with return value n
;      (define (halt n)             (sys-prim 1006 n n n))
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
;
;
;
;      (define (set-car! object value)
;         (if (and (pair? object) (fixnum? value))
;            (set-car! object value)
;            (error "set-car! first argument is not a pair")))
;      (define (set-cdr! object value)
;         (if (and (pair? object) (fixnum? value))
;            (set-cdr! object value)
;            (error "set-car! first argument is not a pair")))


))
