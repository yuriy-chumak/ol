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
      (tuple selector base limit flags)))
(define (get-xDT-values name)
   (let-parses((skip (get-word name #t))
               (skip (get-imm #\=))
               (skip get-whitespaces)
               (base get-hex)
               (skip get-whitespaces)
               (limit get-hex)
               (skip get-rest-of-line))
      (tuple base limit)))

(define gdb-monitor-info-registers-parser
   (let-parses(; qemu/target/i386/helper.c
               ($eax (get-register-value "EAX"))
               ($ebx (get-register-value "EBX"))
               ($ecx (get-register-value "ECX"))
               ($edx (get-register-value "EDX"))
               ($esi (get-register-value "ESI"))
               ($edi (get-register-value "EDI"))
               ($ebp (get-register-value "EBP"))
               ($esp (get-register-value "ESP"))
               ($eip (get-register-value "EIP"))
               ($efl (get-register-value "EFL"))
               (skip get-rest-of-line) ; skip efl flags explanation as [DOSZAPC], cpl, ii, efl, smm and htl(halted)

               ($es (get-selector-values "ES "))
               ($cs (get-selector-values "CS "))
               ($ss (get-selector-values "SS "))
               ($ds (get-selector-values "DS "))
               ($fs (get-selector-values "FS "))
               ($gs (get-selector-values "GS "))

               ($ldt (get-selector-values "LDT"))
               ($tr  (get-selector-values "TR "))
               ($gdt (get-xDT-values "GDT"))
               ($idt (get-xDT-values "IDT"))

               ($cr0 (get-register-value "CR0"))
               ($cr2 (get-register-value "CR2"))
               ($cr3 (get-register-value "CR3"))
               ($cr4 (get-register-value "CR4"))

               ($dr0 (get-register-value "DR0"))
               ($dr1 (get-register-value "DR1"))
               ($dr2 (get-register-value "DR2"))
               ($dr3 (get-register-value "DR3"))
               ($dr6 (get-register-value "DR6"))
               ($dr7 (get-register-value "DR7"))
               
               ($efer (get-register-value "EFER"))

               ; ну и пока хватит..., остались
               ; FCW, FSW, ST, FTW, MXCSR
               ; FPR0 .. FPR7
               ; XMM00 .. XMM07

               )
      (list->ff `(
         ; регистры общего назначения
         (eax . ,(bytes->number $eax 16))
         (ebx . ,(bytes->number $ebx 16))
         (ecx . ,(bytes->number $ecx 16))
         (edx . ,(bytes->number $edx 16))
         (esi . ,(bytes->number $esi 16))
         (edi . ,(bytes->number $edi 16))
         (ebp . ,(bytes->number $ebp 16))
         (esp . ,(bytes->number $esp 16))

         (eip . ,(bytes->number $eip 16))
         (efl . ,(bytes->number $efl 16))

         ; сегментные регистры (селекторы)
         (es . ,$es) ;,(bytes->number $es 16))
         (cs . ,$cs)
         (ss . ,$ss)
         (ds . ,$ds)
         (fs . ,$fs)
         (gs . ,$gs)

         ; дескрипторы таблиц
         (ldt . ,$ldt)
         (gdt . ,$gdt)
         (idt . ,$idt)
         (tr  . ,$tr) ; tss

         ; контрольные регистры
         (cr0 . ,(bytes->number $cr0 16))
         (cr2 . ,(bytes->number $cr2 16))
         (cr3 . ,(bytes->number $cr3 16))
         (cr4 . ,(bytes->number $cr4 16))
         ; дебажные регистры
         (dr0 . ,(bytes->number $dr0 16))
         (dr1 . ,(bytes->number $dr1 16))
         (dr2 . ,(bytes->number $dr2 16))
         (dr3 . ,(bytes->number $dr3 16))
         (dr6 . ,(bytes->number $dr6 16))
         (dr7 . ,(bytes->number $dr7 16))
      ))))


; окошко с регистрами. обновляет, добывает, рисует
(fork-server 'registers (lambda ()
(let loop ((ff #empty))
(let*((envelope (wait-mail))
      (sender msg envelope))
   (let ((answer (gdb gdb-monitor-info-registers-parser "monitor info registers")))
   (let ((eax (get answer 'eax "?"))
         (ebx (get answer 'ebx "?"))
         (ecx (get answer 'ecx "?"))
         (edx (get answer 'edx "?"))
         (esp (get answer 'esp "?"))
         (ebp (get answer 'ebp "?"))
         (esi (get answer 'esi "?"))
         (edi (get answer 'edi "?"))
         (eip (get answer 'eip "?"))
         (show (lambda (x y reg value)
            (locate x y)
            (set-color DARKGREY)
            (for-each display (list "$" reg " "))
            (set-color (if (eq? value (get ff reg -1)) GREY RED))
            (display (if (string? value) value ($reg->string value))))))

      ; внимание, надо отправить в вычислитель новые значения регистров
      ; чтобы можно было использовать переменные непосредственно в скриптах
      (for-each (lambda (i)
         (interact 'evaluator (list 'setq (car i) (cdr i))))
         `(($eax . ,eax)
           ($ebx . ,ebx)
           ($ecx . ,ecx)
           ($edx . ,edx)))

      (show 60 1 'eax eax)
      (show 60 2 'ebx ebx)
      (show 60 3 'ecx ecx)
      (show 60 4 'edx edx)
      (show 60 5 'esp esp)
      (show 60 6 'ebp ebp)
      (show 60 7 'esi esi)
      (show 60 8 'edi edi)

      (show 60 10 'eip eip)

      (mail sender 'ok)
      (loop (fold (lambda (ff v)
                     (put ff (car v) (cdr v)))
               ff `(
                  (eax . ,eax) (ebx . ,ebx) (ecx . ,ecx) (edx . ,edx)
                  (esp . ,esp) (ebp . ,ebp) (esi . ,esi) (edi . ,edi)
                  (eip . ,eip)
               )))))))))
