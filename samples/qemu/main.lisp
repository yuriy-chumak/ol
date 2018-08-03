#!/usr/bin/ol

(import
   (lib rlutil)
   (only (lang sexp) fd->exp-stream)
   (only (lang eval) eval-repl evaluate)
   (only (scheme misc) string->number memv)
   (owl parse))
(cls)

; TEMP. will kill all previously forked qemu and gdb
;(syscall 1017 (c-string "killall qemu-system-i386") #f #f)
(syscall 1017 (c-string "killall gdb") #f #f)

(define (hide-cursor) (display "\x1B;[?25l")) ; temporary disabled
(define (show-cursor) (display "\x1B;[?25h")) ; temporary disabled

; window utils
(define (notify . args)
   (locate 1 21) (set-color DARKGREY)
   (display ":                                                                 ")
   (locate 3 21)
   (for-each display args))

,load "config.lisp"
;; #qemu-img create -f qcow2 win7.qcow2.hd 3G
;; qemu-system-i386 -enable-kvm -m 512 -cdrom /home/uri/Downloads/W7-Super-Lite-x86-Install-2017.iso -boot d -monitor stdio win7.qcow2.hd -s -S

;; #gdb
;; #? set architecture i8086
;; #target remote localhost:1234
;; #x/i $eip

; - parser utils ----------
(define (syntax-fail pos info lst)
   (print-to stderr "parser fail: " info)
   (print-to stderr ">>> " pos "-" (runes->string lst) " <<<")
   '(() (())))

(define ff-digit-to-value
   (list->ff
      (foldr append null
         (list
            ; туда
            (map (lambda (d i) (cons d i)) (iota 10 #\0) (iota 10 0))  ;; 0-9
            (map (lambda (d i) (cons d i)) (iota  6 #\A) (iota 6 10))  ;; A-F
            (map (lambda (d i) (cons d i)) (iota  6 #\a) (iota 6 10))  ;; a-f
            ))))
(define ff-value-to-digit
   (list->ff
      (foldr append null
         (list
            (map (lambda (d i) (cons d i)) (iota 10 0) (iota 10 #\0))  ;; 0-9
            (map (lambda (d i) (cons d i)) (iota 6 10) (iota  6 #\a))  ;; a-f
            ))))
(define whitespaces (list #\space #\tab #\return #\newline))

(define get-rest-of-line
   (let-parses
      ((chars (get-greedy* (get-byte-if (lambda (x) (not (has? '( #\newline #\return) x))))))
       (skip  (get-greedy+ (get-byte-if (lambda (x) (has? '( #\newline #\return) x))))))
      chars))

(define get-whitespaces
   (get-greedy+ (get-byte-if (lambda (x) (has? whitespaces x)))))

(define maybe-whitespaces
   (get-greedy* (get-byte-if (lambda (x) (has? whitespaces x)))))

(define get-hex
   (get-greedy+ (get-byte-if (lambda (x) (get ff-digit-to-value x #false)))))

(define (bytes->number bytes base)
   (fold (lambda (f x) (+ (* f base) (get ff-digit-to-value x 0)))
      0 bytes))
(define (%reg->string reg) ; deprecated
   (bytes->string
      (map (lambda (i)
         (get ff-value-to-digit (band (>> reg (* i 4)) #b1111) #\?))
         (reverse (iota 8 0)))))
(define (%seg->string seg) ; deprecated
   (bytes->string
      (map (lambda (i)
         (get ff-value-to-digit (band (>> seg (* i 4)) #b1111) #\?))
         (reverse (iota 4 0)))))

; if default - number, then default is "base"
; else it's default value with hex width
(define (value->string value default)
   (if value
      (bytes->string
         (map (lambda (i)
            (get ff-value-to-digit (band (>> value (* i 4)) #b1111) #\?))
            (reverse (iota (string-length default) 0))))
      default))

; ================================================================================
(define gdb-prompt-parser
   (let-parses((prompt (get-word "(gdb) " #true)))
      prompt))
(define qemu-prompt-parser
   (let-parses((prompt (get-word "(qemu) " #true)))
      prompt))

; - GDB parsers -----------
(define gdb-greeting-parser
   (let-parses((version get-rest-of-line)
               (copyright get-rest-of-line)
               (license get-rest-of-line)
               (notes (get-greedy+ get-rest-of-line))
               (prompt gdb-prompt-parser))
      version))

(define gdb-info-parser
   (let-parses((info (get-greedy+ get-rest-of-line))
               (prompt gdb-prompt-parser))
      info))

(define gdb-continue-answer-parser
   (let-parses((skip (get-word "Continuing." #t))
               (skip get-rest-of-line)
               (answer get-rest-of-line)) ; Program received signal SIGINT, Interrupt.
      answer))

; Remote debugging using localhost:1234
; 0x0000fff0 in ?? ()
(define gdb-target-connected-parser
   (let-parses((skip (get-word "Remote debugging using " #t))
               (target get-rest-of-line)
               (skip (get-kleene*
                  (let-parses((skip (get-word "warning:" #t))
                              (skip get-rest-of-line)
                              (skip get-rest-of-line))
                     #true)))
               (ip (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\space))))))
               (skip (get-word " in " #t))
               (skip get-rest-of-line)
               (prompt gdb-prompt-parser))
      ip))
(define gdb-si-answer-parser
   (let-parses((ip (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\space))))))
               (skip (get-word " in " #t))
               (skip get-rest-of-line)
               (prompt gdb-prompt-parser))
      ip))
(define gdb-p-x-answer-parser
   (let-parses((skip (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\space))))))
               (skip (get-word " = 0x" #t))
               (value get-rest-of-line))
;               (prompt gdb-prompt-parser))
      value))

;; ;Program received signal SIGINT, Interrupt.
;; ;0x0000d5b4 in ?? ()
;; (define gdb-ctrl-c-answer-parser
;;    (let-parses((skip get-rest-of-line)
;;                (skip (get-word "0x" #t))
;;                ($pc get-hex)
;;                (skip get-rest-of-line))
;; ;               (prompt gdb-prompt-parser))
;;       $pc))
(define gdb-ctrl-c-answer-parser
   (get-word "Quit" #t))


; disassembler
(define gdb-x-i-answer-parser
   (let-parses((lines (get-greedy+
                  (let-parses((skip (get-any-of (get-word "=> " #t)
                                                (get-word "   " #t)))
                              (skip maybe-whitespaces)
                              (address (get-greedy+ (get-byte-if (lambda (x) (not (eq? x #\:))))))
                              (skip (get-imm #\:))
                              (skip maybe-whitespaces)
                              (instruction get-rest-of-line))
                     (cons (bytes->number (cddr address) 16) instruction)))))
;               (prompt gdb-prompt-parser))
      lines))

(define gdb-x-x-answer-parser
   (let-parses((lines (get-greedy+
                  (let-parses((skip (get-any-of (get-word "=> " #t)
                                                (get-word "   " #t)))
                              (skip maybe-whitespaces)
                              (address (get-greedy+ (get-byte-if (lambda (x) (not (eq? x #\:))))))
                              (skip (get-imm #\:))
                              (skip maybe-whitespaces)
                              (instruction get-rest-of-line))
                     (cons (bytes->number (cddr address) 16) instruction)))))
;               (prompt gdb-prompt-parser))
      lines))

(define gdb-monitor-x-answer-parser
   (let-parses((value (get-greedy*
                  (let-parses((skip get-hex)
                              (skip (get-imm #\:))
                              (skip get-whitespaces)
                              (bytes (get-greedy+
                                 (let-parses((skip (get-word "0x" #t))
                                             (byte get-hex)
                                             (skip maybe-whitespaces))
                                    byte))))
                     bytes)))
               (skip gdb-prompt-parser))
      (fold append '() value)))

(define gdb-monitor-info-mem-parser
   (let-parses((value (get-any-of
                  (get-word "PG disabled" #false) ; no memory information available
                  (get-greedy* ; normal memory table
                     (let-parses((start get-hex)
                                 (skip (get-imm #\-))
                                 (end   get-hex)
                                 (skip (get-imm #\space))
                                 (len   get-hex)
                                 (skip (get-imm #\space))
                                 (attr  get-rest-of-line))
                        (tuple start end len attr))))))
      value))

; =============================================================================================
; ==
(define (fork name . arguments)
(fork-server name (lambda ()
   (define In (syscall 22 #f #f #f)) ; create input/output pipes: '(read-pipe . write-pipe)
   (define Out (syscall 22 #f #f #f))
   (define Pid
      (syscall 59 (c-string (car arguments)) ; (syscall:fork)
         (map c-string arguments)
         (list (car In) (cdr Out) (cdr Out))))
   (mail 'config (tuple 'set name Pid)) ; save pid in config (for kill, for example, or other stats)

   (print "forked " name " with id " Pid)

   ; main loop:
   (let loop ()
      (let*((envelope (wait-mail))
            (sender msg envelope)) ; TBD: msg is '(message-to output-parser)
         (let*((parser command
                  (if (function? (car msg))
                     (values (car msg) (cdr msg))
                     (values #false msg))))
            ; it's good idea to free the input buffer
            (syscall 0 (car Out) 1024 #f) ; 1024 would be enought, i think...
            ; send command (if any) with newline
            (unless (null? command)
               (for-each (lambda (x) (display-to (cdr In) x)) (append command '("\n"))))
            ; process answer, if requested
            (mail sender (if parser
                  (car (fd->exp-stream (car Out) "" (car msg) syntax-fail)) #f))))
      (loop)))))

; qemu instance
;; (fork 'qemu "/usr/bin/qemu-system-i386" "-m" "256" "-hda" "winxp.img" "-monitor" "stdio" "-s" "-S")
;; (define (qemu . args)
;;    (interact 'qemu args))
;; (print (bytes->string
;; (qemu get-rest-of-line)))

; gdb instance
(fork 'gdb "/usr/bin/gdb")
(define (gdb . args)
   (interact 'gdb args))
(print (bytes->string
(gdb gdb-greeting-parser))) ; wait for gdb

; minimize popupped QEMU window (for now, just debug reasons)
;; (syscall 1017 (c-string "xdotool windowminimize $(xdotool getactivewindow)") #f #f)
; or use `wmctrl -r "windowname" -b toggle,shaded`

; сконфигурируем gdb
(gdb "set confirm off")

; прерыватель gdb "по требованию" (отправляет Ctrl+C)
; любое сообщение этой сопрограмме заканчивает ее
(define (run-gdb-breaker)
   (fork-server 'gdb-breaker (lambda ()
      (let this ((unused #f))
         (unless (check-mail)
            (begin
               (if (key-pressed #xffbf) ; f2
                  (syscall 62 (interact 'config (tuple 'get 'gdb)) 2 #f)) ; SIGINT
               (this (sleep 1))))))))

; ================================================================
; = main ==================
; подсоединим gdb к qemu
(define pc (bytes->string
   (gdb gdb-target-connected-parser "target remote 127.0.0.1:1234")))

(print "QEmu machine started with PC " pc)

(if (string-eq? pc "0x0000fff0") (begin
   (display "Executing bootstrap, please wait...")
   ; let's do few steps to leave the initial "bios" zeros at 0xfff0
   (let loop ()
      (let ((pc (gdb gdb-si-answer-parser "si")))
         (if (< (bytes->number (cddr pc) 16) #x00010000)
            (loop))))
   (print "Ok.")))
(print "Your QEmu session ready to debug")
; Looks like our machine ready to start, ok.


; -- синтаксический сахар
; пускай символы регистра вычисляются сами в себя (тоже синтаксический сахар)
(define eax 'eax) (define ecx 'ecx) (define edx 'edx) (define ebx 'ebx)
(define esp 'esp) (define ebp 'ebp) (define esi 'esi) (define edi 'edi)
(define eip 'eip) (define efl 'efl)

(define es 'es) (define cs 'cs) (define ss 'ss) (define ds 'ds)
(define fs 'fs) (define gs 'gs)

; --------------------
,load "registers.lisp"

; ================================
; qemu commands
(define save (case-lambda
   ((vm) (gdb gdb-prompt-parser "monitor savevm " vm))
   (() (gdb gdb-prompt-parser "monitor savevm my"))))
(define load (case-lambda
   ((vm) (gdb gdb-prompt-parser "monitor loadvm " vm))
   (() (gdb gdb-prompt-parser "monitor loadvm my"))))


; конфигурационные всякие штуки...
; эта функция устанавливает глобальное значение переменной
(define (set reg value)

   ; а заодно, если надо, то и регистры подправит...
   (cond
      ((has? '(eax ebx ecx edx esp ebp esi edi eip efl) reg)
         (gdb gdb-prompt-parser "set $" reg " = 0x" (%reg->string value))))

   (mail 'config (tuple 'set reg value)))

; get memory value
(define (x address count)
   (list->vector
      (map (lambda (x) (bytes->number x 16))
         (gdb gdb-monitor-x-answer-parser "monitor x /" count "b " address))))
(define (u8 vector offset)
   (ref vector offset))
(define (u16 vector offset)
   (+ (<< (ref vector (+ offset 0))  0)
      (<< (ref vector (+ offset 1))  8)))
(define (u32 vector offset)
   (+ (<< (ref vector (+ offset 0))  0)
      (<< (ref vector (+ offset 1))  8)
      (<< (ref vector (+ offset 2)) 16)
      (<< (ref vector (+ offset 3)) 24)))

(define (xp address count)
   (map (lambda (x) (bytes->number x 16))
      (gdb gdb-monitor-x-answer-parser "monitor xp /" count "b " address)))

; gdb commands
(define (step-into)
   (notify "Step Into...")
   (run-gdb-breaker)
   (notify (bytes->string
      (gdb gdb-si-answer-parser "si")))
   (mail 'gdb-breaker #f))

(define (step-over)
   (notify "Step Over...")
   (let ((code (gdb gdb-x-i-answer-parser "x/2i $eip")))
      ;(caar code) <= current ip
      ;(caadr code) <= next ip
      (gdb get-rest-of-line "tbreak *0x" (%reg->string (caadr code)))) ; <= Temporary breakpoint ? at 0x??

   (run-gdb-breaker)
   (notify (bytes->string
      (gdb gdb-continue-answer-parser "continue"))) ; Continuing. #\newline Temporary breakpoint ?, 0x??? in ?? ()
   (mail 'gdb-breaker #f))

(define (run)
   (notify "Continue...")

   (run-gdb-breaker)
   (notify (bytes->string
      (gdb gdb-continue-answer-parser "continue")))
   (mail 'gdb-breaker #f))
(define (stop)
   (mail 'gdb-breaker #f))

(define (quit)
   (locate 1 20) (set-color GREEN) (display "> quitting...") (set-color GREY)
   #|(qemu "quit")|# (gdb "quit")

   (print "ok.")
   (show-cursor)
   (syscall 1017 (c-string "stty echo") #f #f) ; enable terminal echo
   (halt 1))

; find kernel address
(define (find-kernel-address)
(call/cc (lambda (return)
   (for-each (lambda (block)  ; large memory block
               (let ((start (+ (bytes->number (ref block 1) 16) #x60000)) ; temporary add #x60000 (testing purposes)
                     (len   (- (bytes->number (ref block 3) 16) #x60000)))
                  ; temp
                  (for-each (lambda (addr)
                              (define address (* addr #x1000)) ; step
                              (if (equal? #(#\M #\Z) (x (+ start address) 2))
                                 (let ((e_lfanew (u32 (x (+ start address #x3C) 4) 0)))
                                    (if (equal? #(#\P #\E 0 0) (x (+ start address e_lfanew) 4)) ; IMAGE_NT_SIGNATURE
                                       ;; (let ((IMAGE_FILE_HEADER (x (+ start address e_lfanew #x18))))
                                       ;;    (let ((NumberOfSections (u16 (x (+ start address e_lfanew #x02) 2))))
                                       ; IMAGE_FILE_HEADER:
                                       (return (+ start address))


                                       ))))
                     (iota (floor (/ len #x1000)) 0))))
      (keep (lambda (block)
               (eq? (bytes->number (ref block 3) 16) #x400000)) ; todo: use > 0x100000
         (gdb gdb-monitor-info-mem-parser "monitor info mem"))))))


;(print (bytes->number (ref (find-kernel-address) 1) 16) "(" (bytes->number (ref (find-kernel-address) 3) 16) ")")

;; (define (find-kernel-address)
;;    (call/cc (lambda (return)
;;       (define PPEB (u32 (x (+ (selector-base (% fs)) #x30) 4) 0))
;;       (if (eq? PPEB #xFFFFFFFF)
;;          (return #false))
;;       (define PPEB_LDR_Data (u32 (x (+ PPEB #x0C) 4) 0))

;;       (return PPEB))))

; todo: find all module exports (parse MZ)
(define (load-module-exports)
   #f
)

; todo: отловить запуск пользовательского приложения
; цитата: Распознать начало 64-х битного стека легко - он, в отличие от 32-х битного всегда начитается с ntdll!LdrInitializeThunk. Эта функция - первое что выполняет нить после первого переключения из режима ядра. ntdll!LdrInitializeThunk завершает инициализацию нити и, поскольку нить выполняется в Wow64, переключается с 32-х режим и рекусривно вызывает ntdll!LdrInitializeThunk - на этот раз из 32-х битный ntdll.dll. Кадры стека, соответсвующие этому переходу - wow64!Wow64LdrpInitialize и wow64cpu!RunCpuSimulation. В отличие от Wow64, “нормальные” нити после завершения инициализации прыгают (с помощью nt!NtContinue) на ntdll!RtlUserThreadStart. Указатель стека при этом сбрасывается в начало, так что ntdll!RtlUserThreadStart становится первым кадром 32-х битного стека.

; ================================
; code window
(fork-server 'code (lambda ()
(let loop ((ff #empty))
(let*((envelope (wait-mail))
      (sender msg envelope))
   (locate 1 13)
   (let*(($eip (bytes->number (gdb gdb-p-x-answer-parser "p/x $pc") 16))
         (reuse-pc (let loop ((found #false) (n 0) (lines (get ff 'code '())))
                        (cond
                           ((null? lines)
                              found)
                           (found
                              found)
                           (else
                              ;; (print $eip ": " (caar lines))
                              (loop (if (eq? $eip (caar lines)) n) (+ n 1) (cdr lines))))))
         (code (gdb gdb-x-i-answer-parser
                  "x/8i "
                  (if reuse-pc
                     ; ok, нашел
                     (string-append "0x" (%reg->string (get ff 'eip $eip)))
                     ; else
                     "$pc"))))
      (locate 1 1) (set-color GREY)
      (for-each (lambda (ai)
            (display "                                                           \x1B;[1000D")
            ; address 
            (display "0x")
            (display (%reg->string (car ai)))
            (display (if (eq? $eip (car ai)) " * " "   "))
            
            (display (bytes->string (cdr ai)))
            (print)
            #true)
         code)
      (mail sender 'ok)

      ;(print code)
      (let ((pc (cond
                  ; такой ip у нас не числится
                  ((not reuse-pc)
                     $eip)
                  ; мы слишком далеко зашли, надо скорольнуться
                  ((> reuse-pc (- (length code) 3))
                     (caadr code))
                  ; ну или ничего не менять
                  (else
                     (get ff 'eip $eip)))))
         (loop (put
            (put ff 'eip pc)
            'code code))))))))

; ---
; почистим окно
(cls);(syscall 1017 (c-string "stty -echo") #f #f)
(define progressbar "----\\\\\\\\||||////")

; ============================================
; запустим вычислитель, это надо сделать здесь
; чтобы захватить все предыдущие дефайны
(fork-server 'evaluator (lambda ()
(let loop ((env *toplevel*))
(let*((envelope (wait-mail))
      (sender msg envelope))
   (tuple-case (eval-repl msg env #false evaluate)
      ((ok answer env)
         (mail sender answer)
         (loop env))
      (else is error
         (mail sender #false)
         (loop env)))))))

; и главный цикл обработки событий клавиатуры
; main loop
(load 0) ; временно
(let main ((dirty #true) (progress 0))
   (hide-cursor)
   (locate 58 1) (set-color GREY)
   (display (string (ref progressbar (mod progress (size progressbar)))))

   (if dirty (begin
      (interact 'code 'show)

      (info-registers) ; обновим регистры
      (interact 'registers 'show)
      
      (locate 1 20) (set-color GREY) (display ">                                                                 ")))
   (locate 3 20) (set-color DARKGREY)

   ; https://www.cl.cam.ac.uk/~mgk25/ucs/keysymdef.h
   (cond

      ; Step In
      ((key-pressed #xffc2) ; F5
         (step-into)
         (main #true 0))

      ; Step Over
      ((key-pressed #xffc3) ; F6
         (step-over)
         (main #true 0))

      ; Nothing, just refresh
      ((key-pressed #xffc5) ; F8
         (find-kernel-address)
         (main #true 0))

      ; Continue
      ((key-pressed #xffc6) ; F9
         (run)
         (main #true 0)) ; уходим в режим выполнения машины

      ; Quit
      ((key-pressed #x0051) ; Q
         (quit) ; exit
         (main #true 0)) ; just loop optimization

      ; ручные команды
      ((key-pressed #xff0d) ; XK_Return
         (locate 1 20) (set-color GREEN) (display "> ") (set-color GREY)
         ; почистим входной буфер
         (let loop ()
            (let ((in (syscall 0 stdin 1024 #f)))
               (if (or (eq? in #true) (eq? in 1024))
                  (loop))))

         (locate 3 20)
         (show-cursor)
         (syscall 1017 (c-string "stty echo") #f #f)

         (let ((command (read)))
            (print "eval: "
               (interact 'evaluator command)))
         (main #true 0))

      (else
         (yield)
         (main #f (+ 1 progress)))))

; прикол в экране загрузки WinXP: пустой цикл.
;; 0x806f1d50 * sub    $0x1,%eax                            | $eax 00008987
;; 0x806f1d53   jne    0x806f1d50                             $ebx 00000000
;; 0x806f1d55   mov    0x806fc0d0,%eax                        $ecx ff800000
;; 0x806f1d5a   call   *%eax                                  $edx 00000000
;; 0x806f1d5c   sub    %esi,%eax                              $esp f9e631fc
;; 0x806f1d5e   sbb    %ebx,%edx                              $ebp f9e6320c
;; 0x806f1d60   je     0x806f1d67                             $esi 00f05998
;; 0x806f1d62   mov    $0x7fffffff,%eax                       $edi 00000003

;;                                                            $eip 806f1d50
