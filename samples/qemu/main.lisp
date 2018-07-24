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

(define get-rest-of-line
   (let-parses
      ((chars (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\newline))))))
       (skip  (get-imm #\newline))) ;; <- note that this won't match if line ends to eof
      chars))

(define ff-digit-values
   (list->ff
      (foldr append null
         (list
            ; туда
            (map (lambda (d i) (cons d i)) (iota 10 #\0) (iota 10 0))  ;; 0-9
            (map (lambda (d i) (cons d i)) (iota  6 #\A) (iota 6 10))  ;; A-F
            (map (lambda (d i) (cons d i)) (iota  6 #\a) (iota 6 10))  ;; a-f
            ; и обратно
            (map (lambda (d i) (cons d i)) (iota 10 0) (iota 10 #\0))  ;; 0-9
            (map (lambda (d i) (cons d i)) (iota 6 10) (iota  6 #\a))  ;; a-f
            ))))

; assert (eq? (car bytes) #\0)
; assert (eq? (cadr bytes) #\x)
(define (bytes->number bytes base)
   (fold (lambda (f x) (+ (* f base) (get ff-digit-values x 0)))
      0 bytes))
(define ($reg->string $reg)
   (bytes->string
      (map (lambda (i)
         (get ff-digit-values (band (>> $reg (* i 4)) #b1111) #\?))
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
               (skip get-rest-of-line)
               (prompt gdb-prompt-parser))
      ip))
(define gdb-p-x-answer-parser
   (let-parses((skip (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\space))))))
               (skip (get-word " = 0x" #t))
               (value get-rest-of-line)
               (prompt gdb-prompt-parser))
      value))



; =============================================================================================
; ==
(define (fork name . arguments)
(fork-server name (lambda ()
   (define In (syscall 22 #f #f #f)) ; create input/output pipes: '(read-pipe . write-pipe)
   (define Out (syscall 22 #f #f #f))
   (print In ", " Out)
   (syscall 59 (c-string (car arguments)) ; (syscall:fork)
      (map c-string arguments)
      (list (car In) (cdr Out) stderr))

   ; main loop:
   (let loop ()
      (let*((envelope (wait-mail))
            (sender msg envelope)) ; TBD: msg is '(message-to output-parser)
         ;(print "msg: " msg)
         ;(print "(cdr msg): " (cdr msg))
         (unless (null? (cdr msg)) (begin
            ;(print "sending " msg)
            (for-each (lambda (x) (display-to (cdr In) x)) (cdr msg))
            (display-to (cdr In) "\n")))
         
         (mail sender (car (fd->exp-stream (car Out) "" (car msg) syntax-fail))))
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

; minimize popupped QEMU window (for now, debug reasons)
(syscall 1017 (c-string "xdotool windowminimize $(xdotool getactivewindow)") #f #f)
; or use `wmctrl -r "windowname" -b toggle,shaded`

; =========================
; = main ==================
; подсоединим gdb к qemu
(define ip (bytes->string
(gdb gdb-target-connected-parser "target remote localhost:1234")))

(print "Current IP is " ip)
(print "Press [Ctrl+C]...")

; let's do few steps to leave the initial "bios" zeros at 0xfff0
;; (let loop ()
;;    (let ((ip (gdb gdb-si-answer-parser "si")))
;;       (if (< (bytes->number (cddr ip) 16) #x00010000)
;;          (loop))))
; Looks like our machine ready to start:

; окошко с регистрами. обновляет, добывает, рисует
(fork-server 'registers (lambda ()
(let loop ((ff #empty))
(let*((envelope (wait-mail))
      (sender msg envelope))
   (let ((eax (bytes->number (gdb gdb-p-x-answer-parser "p/x $eax") 16))
         (ebx (bytes->number (gdb gdb-p-x-answer-parser "p/x $ebx") 16))
         (ecx (bytes->number (gdb gdb-p-x-answer-parser "p/x $ecx") 16))
         (edx (bytes->number (gdb gdb-p-x-answer-parser "p/x $edx") 16))
         (show (lambda (x y reg value)
            (locate x y)
            (set-color DARKGREY)
            (for-each display (list "$" reg " "))
            (set-color (if (eq? value (get ff reg -1)) GREY RED))
            (display ($reg->string value)))))

      (show 60 1 'eax eax)
      (show 60 2 'ebx ebx)
      (show 60 3 'ecx ecx)
      (show 60 4 'edx edx)

      (mail sender 'ok)
      (loop (fold (lambda (ff v)
                     (put ff (car v) (cdr v)))
               ff `(
                  (eax . ,eax)
                  (ebx . ,ebx)
                  (ecx . ,ecx)
                  (edx . ,edx)
               ))))))))

; numeric utils
; ---

(let loop ()
   (interact 'registers 'show)

;   (locate 1 60) (for-each display (list
;      "eax " (bytes->string ))


   (locate 1 20) (set-color GREY) (display "> ")
   (let ((r (read)))
      (cond
         ((eq? r 's)
            ;(gdb get-rest-of-line "si")
            (print "Ok"))))
   (loop))
