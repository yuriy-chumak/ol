; https://github.com/yuriy-chumak/rlutil
(define-library (lib rlutil)
   (import (r5rs core)
           (owl string)
           (owl math) (owl io) (owl list)
           (otus pinvoke))
   (export
      locate)
(begin
   (define uname (syscall 63 #f #f #f))                 ; uname -a
;   (define TERM (syscall 1016 (c-string "TERM") #f #f)) ; getenv('TERM')

   (define win32? (string-ci=? (ref uname 1) "Windows"))
   (define linux? (string-ci=? (ref uname 1) "Linux"))

   ; this check based on https://gist.github.com/ssbarnea/1316877
   ; (define ansi? (not (and win32?
   ;                        (not (and TERM
   ;                                  (string-ci=? TERM "ANSI"))))))

(define locate (cond
   (win32?
      (let*((kernel32 (dlopen "kernel32.dll"))
            (STD_OUTPUT_HANDLE -11)
            (GetStdHandle               (dlsym kernel32 type-vptr "GetStdHandle" type-fix+))
            (SetConsoleCursorPosition   (dlsym kernel32 type-vptr "SetConsoleCursorPosition" type-vptr type-int+)))
      (lambda (x y)
         (let ((COORD (+ (band x #xFFFF)
                         (<< (band y #xFFFF) 16))))
            (SetConsoleCursorPosition (GetStdHandle STD_OUTPUT_HANDLE) COORD)))))
   (linux?
      (lambda (x y)
         (for-each display `("\x1B;[" ,y ";" ,x "f"))))))

))
