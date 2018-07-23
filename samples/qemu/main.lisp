#!/usr/bin/ol

;; #qemu-img create -f qcow2 win7.qcow2.hd 3G
;; qemu-system-i386 -enable-kvm -m 512 -cdrom /home/uri/Downloads/W7-Super-Lite-x86-Install-2017.iso -boot d -monitor stdio win7.qcow2.hd -s -S

;; #gdb
;; #? set architecture i8086
;; #target remote localhost:1234
;; #x/i $eip



(define In (syscall 22 #f #f #f))
(define Out (syscall 22 #f #f #f))

(print "In: " In)
(print "Out: " Out)

;(define (gdb what)
;  (print-to (cdr In) what))
;   (syscall 1 (cdr In) what (size what)))

; real fork
(syscall 59 (c-string "/usr/bin/gdb")
   (map c-string (list ""))
   (list (car In) (cdr Out) stderr))

; gdb instance
(fork-server 'gdb (lambda ()
(let loop ()
   (let ((line (syscall 0 (car Out) 1024 #f)))
      (if (eq? (type line) type-vector-raw)
         (print (runes->string (vector->list line))))
      (loop)))))

;(print-to (cdr In) "file /usr/bin/ol")
;(print-to (cdr In) "b main")

(print-to (cdr In) "target remote localhost:1234")

; our repl:
(let loop ()
   (let ((line (syscall 0 stdin 1024 #f)))
      (if (eq? (type line) type-vector-raw)
         (let ((line (runes->string (vector->list line))))
            (print "line: " line)
            (cond
               ((string-eq? line "s\n")
                  (print-to (cdr In) "si"))
               ((string-eq? line "p\n")
                  (print-to (cdr In) "x/i $eip"))
               (else
                  (print-to (cdr In) line))))))
   (loop))

(print "ok.")