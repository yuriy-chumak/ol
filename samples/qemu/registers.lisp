; "monitor info registers" parser
(define (get-register-value name)
   (let-parses((skip (get-word name #t))
               (skip (get-imm #\=))
               (value get-hex)
               (skip maybe-whitespaces))
      value))
(define (get-selector-values name)
   (let-parses((skip (get-word name #t))
               (skip (get-imm #\=))
               (selector get-hex)
               (skip get-whitespaces)
               (base get-hex)
               (skip get-whitespaces)
               (limit get-hex)
               (skip get-whitespaces)
               (flags get-hex)
               (skip get-rest-of-line)) ; todo: check "DPL=" etc.
      [selector base limit flags]))
(define (get-xDT-values name)
   (let-parses((skip (get-word name #t))
               (skip (get-imm #\=))
               (skip get-whitespaces)
               (base get-hex)
               (skip get-whitespaces)
               (limit get-hex)
               (skip get-rest-of-line))
      [base limit]))

(define gdb-monitor-info-registers-parser
   (let-parses(; qemu/target/i386/helper.c
               (%eax (get-register-value "EAX"))
               (%ebx (get-register-value "EBX"))
               (%ecx (get-register-value "ECX"))
               (%edx (get-register-value "EDX"))
               (%esi (get-register-value "ESI"))
               (%edi (get-register-value "EDI"))
               (%ebp (get-register-value "EBP"))
               (%esp (get-register-value "ESP"))
               (%eip (get-register-value "EIP"))
               (%efl (get-register-value "EFL"))
               (skip get-rest-of-line) ; skip efl flags explanation as [DOSZAPC], cpl, ii, efl, smm and htl(halted)

               (%es (get-selector-values "ES "))
               (%cs (get-selector-values "CS "))
               (%ss (get-selector-values "SS "))
               (%ds (get-selector-values "DS "))
               (%fs (get-selector-values "FS "))
               (%gs (get-selector-values "GS "))

               (%ldt (get-selector-values "LDT"))
               (%tr  (get-selector-values "TR "))
               (%gdt (get-xDT-values "GDT"))
               (%idt (get-xDT-values "IDT"))

               (%cr0 (get-register-value "CR0"))
               (%cr2 (get-register-value "CR2"))
               (%cr3 (get-register-value "CR3"))
               (%cr4 (get-register-value "CR4"))

               (%dr0 (get-register-value "DR0"))
               (%dr1 (get-register-value "DR1"))
               (%dr2 (get-register-value "DR2"))
               (%dr3 (get-register-value "DR3"))
               (%dr6 (get-register-value "DR6"))
               (%dr7 (get-register-value "DR7"))
               
               (%efer (get-register-value "EFER"))

               ; ну и пока хватит..., остались
               ; FCW, FSW, ST, FTW, MXCSR
               ; FPR0 .. FPR7
               ; XMM00 .. XMM07

               )
      (pairs->ff `(
         ; регистры общего назначения
         (eax . ,(bytes->number %eax 16))
         (ebx . ,(bytes->number %ebx 16))
         (ecx . ,(bytes->number %ecx 16))
         (edx . ,(bytes->number %edx 16))
         (esi . ,(bytes->number %esi 16))
         (edi . ,(bytes->number %edi 16))
         (ebp . ,(bytes->number %ebp 16))
         (esp . ,(bytes->number %esp 16))

         (eip . ,(bytes->number %eip 16))
         (efl . ,(bytes->number %efl 16))

         ; сегментные регистры (селекторы)
         (es . ,[
                  (bytes->number (ref %es 1) 16) ; selector value
                  (bytes->number (ref %es 2) 16) ; base
                  (bytes->number (ref %es 3) 16) ; limit
                  (bytes->number (ref %es 4) 16)]); flags
         (cs . ,[
                  (bytes->number (ref %cs 1) 16)
                  (bytes->number (ref %cs 2) 16)
                  (bytes->number (ref %cs 3) 16)
                  (bytes->number (ref %cs 4) 16)])
         (ss . ,[
                  (bytes->number (ref %ss 1) 16)
                  (bytes->number (ref %ss 2) 16)
                  (bytes->number (ref %ss 3) 16)
                  (bytes->number (ref %ss 4) 16)])
         (ds . ,[
                  (bytes->number (ref %ds 1) 16)
                  (bytes->number (ref %ds 2) 16)
                  (bytes->number (ref %ds 3) 16)
                  (bytes->number (ref %ds 4) 16)])
         (fs . ,[
                  (bytes->number (ref %fs 1) 16)
                  (bytes->number (ref %fs 2) 16)
                  (bytes->number (ref %fs 3) 16)
                  (bytes->number (ref %fs 4) 16)])
         (gs . ,[
                  (bytes->number (ref %gs 1) 16)
                  (bytes->number (ref %gs 2) 16)
                  (bytes->number (ref %gs 3) 16)
                  (bytes->number (ref %gs 4) 16)])

         ; дескрипторы таблиц
         (ldt . ,%ldt)
         (gdt . ,%gdt)
         (idt . ,%idt)
         (tr  . ,%tr) ; tss

         ; контрольные регистры
         (cr0 . ,(bytes->number %cr0 16))
         (cr2 . ,(bytes->number %cr2 16))
         (cr3 . ,(bytes->number %cr3 16))
         (cr4 . ,(bytes->number %cr4 16))
         ; дебажные регистры
         (dr0 . ,(bytes->number %dr0 16))
         (dr1 . ,(bytes->number %dr1 16))
         (dr2 . ,(bytes->number %dr2 16))
         (dr3 . ,(bytes->number %dr3 16))
         (dr6 . ,(bytes->number %dr6 16))
         (dr7 . ,(bytes->number %dr7 16))
      ))))


; функция, возвращающая текущее значение регистра
(define (% reg)
   (await (mail 'config ['get reg])))

; селектор
(define (selector-value selector)
   (ref selector 1))
(define (selector-base selector)
   (ref selector 2))
(define (selector-limit selector)
   (ref selector 3))
(define (selector-flags selector)
   (ref selector 4))


; обновлятор регистров (актуальное состояние, из qemu)
(define (info-registers)
   (let ((registers (gdb gdb-monitor-info-registers-parser "monitor info registers")))
   (let (; регистры общего назначения
         (eax (get registers 'eax #f))
         (ebx (get registers 'ebx #f))
         (ecx (get registers 'ecx #f))
         (edx (get registers 'edx #f))
         (esp (get registers 'esp #f))
         (ebp (get registers 'ebp #f))
         (esi (get registers 'esi #f))
         (edi (get registers 'edi #f))
         (eip (get registers 'eip #f))
         (efl (get registers 'efl #f))
         ; селекторы
         (es  (get registers 'es #false))
         (cs  (get registers 'cs #false))
         (ss  (get registers 'ss #false))
         (ds  (get registers 'ds #false))
         (fs  (get registers 'fs #false))
         (gs  (get registers 'gs #false))
        )

      ; запомним значения регистров, чтобы к ним могли обращаться отовсюду (и даже из скриптов)
      (for-each (lambda (i)
            (mail 'config ['set (car i) (cdr i)]))
         `(
            (eax . ,eax) (ebx . ,ebx) (ecx . ,ecx) (edx . ,edx)
            (esp . ,esp) (ebp . ,ebp) (esi . ,esi) (edi . ,edi)

            (eip . ,eip) (efl . ,efl)

            (es . ,es) (cs . ,cs) (ss . ,ss) (ds . ,ds)
            (fs . ,fs) (gs . ,gs)
         ))

      ; ну и для удобства, просто так, заведем их как глобальные
      ; тоже синтаксический сахар
      ; но надо помнить, что в функциях их использовать нельзя
      (for-each (lambda (i)
            (await (mail 'evaluator (list 'setq (car i) (cdr i)))))
         `(
            (%eax . ,eax) (%ebx . ,ebx) (%ecx . ,ecx) (%edx . ,edx)
            (%esp . ,esp) (%ebp . ,ebp) (%esi . ,esi) (%edi . ,edi)

            (%eip . ,eip) (%efl . ,efl)

            (%es . ,es) (%cs . ,cs) (%ss . ,ss) (%ds . ,ds)
            (%fs . ,fs) (%gs . ,gs)
         )))
      registers))

; окошко с регистрами. обновляет, добывает, рисует
(define (display-reg reg value)
   (display (value->string value "????????")))

(define (display-seg seg value)
   (for-each display
      (list
         ; все это неправильно. переделать!
         (value->string (ref value 1) "????")     ; value, 16-bit
         " "
         (value->string (ref value 2) "????????") ; base, 32-bit
         "("
         (value->string (ref value 3) "????????") ; limit, 20-bit (?)
         ")"
         (value->string (ref value 4) "??????")))); flags, 24-bit

(define layout (pairs->ff `(
   (eax . (60 1 ,display-reg))
   (ecx . (60 2 ,display-reg))
   (edx . (60 3 ,display-reg))
   (ebx . (60 4 ,display-reg))

   (esp . (60 5 ,display-reg))
   (ebp . (60 6 ,display-reg))
   (esi . (60 7 ,display-reg))
   (edi . (60 8 ,display-reg))

   (eip .(60 10 ,display-reg))
   (efl .(60 11 ,display-reg))

   (es . (65 13 ,display-seg))
   (cs . (65 14 ,display-seg))
   (ss . (65 15 ,display-seg))
   (ds . (65 16 ,display-seg))
   (fs . (65 17 ,display-seg))
   (gs . (65 18 ,display-seg))

)))



(coroutine 'registers (lambda ()
(let loop ((ff #empty))
(let*((envelope (wait-mail))
      (sender msg envelope))
   (let show ((L (ff-iter layout)))
      (unless (null? L)
         (let ((reg (caar L))
               (layout (cdar L)))
         (let ((value (% reg)))
            (locate (car layout) (cadr layout))
            (set-color DARKGREY)
            (for-each display (list "%" reg " "))
            (set-color (if (equal? (get ff reg #f) value)
               GREY
               RED))
            ((caddr layout) reg value)
            (show (force (cdr L)))))))
   (mail sender 'ok)
   (loop (fold (lambda (ff key)
                  (put ff key (% key)))
            ff '(eax ecx edx ebx
                 esp ebp esi edi
                 eip efl
                 es cs ss ds fs gs
            )))))))
