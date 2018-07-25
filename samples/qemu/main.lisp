#!/usr/bin/ol

(import
   (lib rlutil)
   (only (lang sexp) fd->exp-stream)
   (only (scheme misc) string->number)
   (owl parse))
(cls)

; TEMP. will kill all previously forked qemu and gdb
(syscall 1017 (c-string "killall qemu-system-i386") #f #f)
(syscall 1017 (c-string "killall gdb") #f #f)

(define (hide-cursor) (display "\x1B;[?25l"))
(define (show-cursor) (display "\x1B;[?25h"))

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
      ((chars (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\newline))))))
       (skip  (get-imm #\newline))) ;; <- note that this won't match if line ends to eof
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
(define ($reg->string $reg)
   (bytes->string
      (map (lambda (i)
         (get ff-value-to-digit (band (>> $reg (* i 4)) #b1111) #\?))
         (reverse (iota 8 0)))))

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

; Remote debugging using localhost:1234
; 0x0000fff0 in ?? ()
(define gdb-target-connected-parser
   (let-parses((skip (get-word "Remote debugging using " #t))
               (target get-rest-of-line)
               (ip (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\space))))))
               (skip (get-word " in " #t))
               (skip get-rest-of-line)
               (prompt gdb-prompt-parser))
      ip))
(define gdb-si-answer-parser
   (let-parses((ip (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\space))))))
               (skip (get-word " in " #t))
               (skip get-rest-of-line))
;               (prompt gdb-prompt-parser))
      ip))
(define gdb-p-x-answer-parser
   (let-parses((skip (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\space))))))
               (skip (get-word " = 0x" #t))
               (value get-rest-of-line))
;               (prompt gdb-prompt-parser))
      value))

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
   (print "forked " name " with id " Pid)

   ; main loop:
   (let loop ()
      (let*((envelope (wait-mail))
            (sender msg envelope)) ; TBD: msg is '(message-to output-parser)
         ;(print "msg: " msg)
         ;(print "(cdr msg): " (cdr msg))

         ; it's good idea to free the input buffer
         (syscall 0 (car Out) 1024 #f) ; 1024 would be enought, i think...

;         (if (equal? msg '(SIGINT)) ; SIGINT = 2
;            (syscall 62 Pid 2 #f)

         (let*((parser command
                  (if (string? (car msg))
                     (values #false msg)
                     (values (car msg) (cdr msg)))))
            ;(locate 40 18) (print "parser: " parser)
            ;(locate 40 19) (print "command: " command)
         (unless (null? command) (begin
            ;(print "sending " msg)
            (for-each (lambda (x) (display-to (cdr In) x)) command)
            (display-to (cdr In) "\n")))
         
         (mail sender (if parser (car (fd->exp-stream (car Out) "" (car msg) syntax-fail)) #true))))
      (loop)))))

; qemu instance
(fork 'qemu "/usr/bin/qemu-system-i386" "-m" "256" "-hda" "winxp.img" "-monitor" "stdio" "-s" "-S")
(define (qemu . args)
   (interact 'qemu args))
(print (bytes->string
(qemu get-rest-of-line)))

; gdb instance
(fork 'gdb "/usr/bin/gdb")
(define (gdb . args)
   (interact 'gdb args))
(print (bytes->string
(gdb gdb-greeting-parser))) ; wait for gdb

; minimize popupped QEMU window (for now, just debug reasons)
(syscall 1017 (c-string "xdotool windowminimize $(xdotool getactivewindow)") #f #f)
; or use `wmctrl -r "windowname" -b toggle,shaded`

; сконфигурируем gdb
(gdb "set confirm off")


; ================================================================
; = main ==================
; подсоединим gdb к qemu
(define pc (bytes->string
   (gdb gdb-target-connected-parser "target remote localhost:1234")))

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

,load "registers.lisp"

; helper functions:


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
                     (string-append "0x" ($reg->string (get ff 'eip $eip)))
                     ; else
                     "$pc"))))
      (locate 1 1) (set-color GREY)
      (for-each (lambda (ai)
            (display "                                                           \x1B;[1000D")
            ; address 
            (display "0x")
            (display ($reg->string (car ai)))
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
(cls)(syscall 1017 (c-string "stty -echo") #f #f)
(define progressbar "-\\|/")

; main loop
(let main ((dirty #true) (interactive #true) (progress 0))
   (hide-cursor)
   (locate 58 1) (set-color GREY)
   (if progress
      (display (string (ref progressbar (mod progress (size progressbar)))))
      (display "*"))

   (if dirty (begin
      (interact 'code 'show)
      (interact 'registers 'show)))

   (locate 1 20) (set-color DARKGREY) (display ">                                                                 ")
   (locate 3 20) (set-color DARKGREY)

   (case interactive
      ; машина выполняется, нужна только одна кнопка на стоп
      (#false
         (cond
            ((or (key-pressed #xffc2) ; F5
                 (key-pressed #xffc3) ; F6
                 (key-pressed #xffc4) ; F7
                 (key-pressed #xffc5)); F8
               (gdb gdb-prompt-parser "monitor stop")
               ; и выполним одну команду, чтобы gdb засинхронизировался с qemu
               (gdb gdb-si-answer-parser "si")
               (main #true #true 0))

            ; тут надо проверить, а не пришло ли нам какое событие от дебаггера.
            ; например, а не сработал ли брекпоинт
            (else
               (main #f interactive #false))))

      ; интерактивный режим
      (#true
         (cond
            ; https://www.cl.cam.ac.uk/~mgk25/ucs/keysymdef.h

            ; Step In
            ((key-pressed #xffc2) ; F5
               (gdb get-rest-of-line "si")
               (main #true interactive 0))

            ; Step Over
            ((key-pressed #xffc3) ; F6
               (let ((code (gdb gdb-x-i-answer-parser "x/2i $eip")))
                  ;(caar code) <= current ip
                  ;(caadr code) <= next ip
                  (print "  " ($reg->string (caadr code)))
                  (gdb get-rest-of-line "tbreak *0x" ($reg->string (caadr code)))) ; <= Temporary breakpoint ? at 0x??
               (gdb get-rest-of-line "c") ; Continuing. #\newline Temporary breakpoint ?, 0x??? in ?? ()

               (main #true interactive 0))

            ; Nothing, just refresh
            ((key-pressed #xffc5) ; F8
               (main #true interactive 0))

            ; Continue
            ((key-pressed #xffc6) ; F9
               (gdb "monitor cont")
               (main #false #false #false)) ; уходим в режим выполнения машины

            ; Quit
            ((key-pressed #x0051) ; Q
               (locate 1 20) (set-color GREEN) (display "> quitting...") (set-color GREY)
               (qemu "quit") (gdb "quit")

               (print "ok.")
               (show-cursor)
               (syscall 1017 (c-string "stty echo") #f #f)
               (halt 1)) ; for now - just exit

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
                  (cond
                     ((eq? command 'save)
                        (qemu "savevm snapshot"))
                     ((eq? command 'load)
                        (qemu "loadvm snapshot"))
                     ((eq? command 'quit)
                        (print "quit?"))
                     (else
                        (print "Unknown command: " command))))
                  
               (main #true interactive 0))
            ;; ((key-pressed #x0020) ; XK_space
               ;; (let ((answer (gdb gdb-x-i-answer-parser "x/7i $pc")))
               ;;    (cls)
               ;;    (locate 1 1) (set-color GREY)
               ;;    (for-each (lambda (ai)
               ;;          (print "0x" ($reg->string (car ai)) "   " (bytes->string (cdr ai)))
               ;;          #true)
               ;;       answer))
               ;;    (interact 'registers 'show))

            (else
               (yield)
               (main #f interactive (+ 1 progress)))))))

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
