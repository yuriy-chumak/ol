;!
(define (load-library name flag) (sys-prim 30 (c-string name) flag #false))
(define (get-proc-address  type dll name) ; todo: переименовать в get-proc-address ?
   (let ((function (cons type (sys-prim 31 dll (c-string name) #false)))) ; todo: избавиться от (c-string)
      (lambda args
         (sys-prim 32 (cdr function) (car function) args))))
(define (get-proc-address-c type dll name) ; todo: переименовать в get-proc-address ?
; todo: отправлять тип функции третим параметром (sys-prim 31) и в виртуальной машине
; возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) 
   (let ((function (cons (bor type 64) (sys-prim 31 dll (c-string name) #false)))) ; todo: переделать 64 во что-то поприятнее
      (lambda args
         (sys-prim 32 (cdr function) (car function) args))))
(define type-word 45)

; todo: определить константы возвращаемого типа и использовать их в описании возврата функций
; что-то вроде (define rt-int 1)

; todo: тип для (get-proc-address) - всегда число, добавить в проверку 


; my temporary stubs for opengl (у меня пока ж нет структур и т.д.)
(define kernel32_dll (load-library "kernel32" 0))
  (define GetModuleHandle (get-proc-address type-word kernel32_dll "GetModuleHandleA"))
  
(define _exe (GetModuleHandle 0))
;(define wmain          (get-proc-address type-fix+ _exe "wmain@12")) ; test
;(wmain 1 2 3)                                                        ; test
;(define cmain          (get-proc-address-c type-fix+ _exe "cmain"))  ; test
;(cmain 1 2 3)                                                        ; test


;(define WinMain        (get-proc-address type-fix+ _exe "WinMain@16"))
(define CreateGLWindow (get-proc-address-c type-fix+ _exe "CreateGLWindow")) 
(define KillGLWindow   (get-proc-address-c type-fix+ _exe "KillGLWindow"))
(define DrawGLScene    (get-proc-address-c type-fix+ _exe "DrawGLScene"))

         
; real code
(define user32_dll (load-library "user32" 0))
  (define IDOK 1)
  (define IDCANCEL 2)

  (define MessageBox (get-proc-address type-fix+ user32_dll "MessageBoxA"))
    (define MB_OK 0)
    (define MB_OKCANCEL 1)
    (define MB_ICONASTERISK 64)
  (define PeekMessage      (get-proc-address type-fix+ user32_dll "PeekMessageA"))
    (define PM_REMOVE 1)
  (define TranslateMessage (get-proc-address type-fix+ user32_dll "TranslateMessage"))
  (define DispatchMessage  (get-proc-address type-fix+ user32_dll "DispatchMessageA"))
  (define PostQuitMessage  (get-proc-address type-fix+ user32_dll "PostQuitMessage"))
  
  (define GetKeyState      (get-proc-address type-fix+ user32_dll "GetKeyState"))
  (define GetAsyncKeyState (get-proc-address type-fix+ user32_dll "GetAsyncKeyState"))
  (define GetKeyboardState (get-proc-address type-fix+ user32_dll "GetKeyboardState"))
  
  

; проверка, что все запустилось.

;(if (=
;  (MessageBox 0 "Please, press OK for test pass!" (c-string "load-library test")
;    (bor MB_OKCANCEL MB_ICONASTERISK))
;  IDOK)
;    (print "OK")
;    (print "CANCEL"))
; todo: вроде бы все строки и так заканчиваются на '\0' - проверить
;(define echo "echo server")

; в момент импорта сделать все нужные привязки
; export (MessageBox)  и т.д.

(CreateGLWindow "NeHe's OpenGL Framework" 640 480 16 0)
;(WinMain 0 0 0 0)

(define MSG (make-vector 28 0)) ; sizeof(MSG)=28
(call/cc (lambda (return)
  (define (cycle)   ;MSG
    (if (not (= (GetAsyncKeyState 27) 0)) (return))
    
    (if (= 1 (PeekMessage MSG 0 0 0 PM_REMOVE))
      (begin  
        (TranslateMessage MSG)
        (DispatchMessage MSG))
      (DrawGLScene))
    (cycle))
  (cycle)))

(KillGLWindow)
(print "@")

