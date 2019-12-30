(define-library (lib keyboard)
   (version 1.0)
   (license MIT/LGPL3)
   (description "keyboard support library")
(import
   (otus lisp) (otus ffi))

(export
   KEY_ENTER KEY_ESC
   KEY_LEFTCTRL KEY_LEFTALT KEY_LEFTSHIFT
   KEY_UP KEY_DOWN KEY_LEFT KEY_RIGHT

   ;vkQ vkW vkE vkR vkT vkY vkU vkI vkO vkP
   ;vkA vkS vkD vkF vkG vkH vkJ vkK vkL
   ;vkZ vkX vkC vkV vkB vkN vkM
   ;vkStar vkPlus vkMinus vkEqual
   
   key-pressed?)

(cond-expand
   (Windows
      (begin
         (setq user32 (load-dynamic-library "user32.dll"))
         (setq GetAsyncKeyState (user32 fft-unsigned-short "GetAsyncKeyState" fft-int))

         (define (key-pressed? key)
            (let ((state (GetAsyncKeyState key)))
               (eq? #x8000 (band state #x8000)))) ;(<< 1 15)

         ; https://docs.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes
         (define KEY_ENTER #x0D) (define KEY_ESC #x1B)
         (define KEY_LEFTCTRL #x11) (define KEY_LEFTALT #x12) (define KEY_LEFTSHIFT #x10)
         (define KEY_UP #x26) (define KEY_DOWN #x28) (define KEY_LEFT #x25) (define KEY_RIGHT #x27)
         ;; (define vkQ 24) (define vkW 25) (define vkE 26) (define vkR 27) (define vkT 28) (define vkY 29) (define vkU 30) (define vkI 31) (define vkO 32) (define vkP 33)
         ;; (define vkA 38) (define vkS 39) (define vkD 40) (define vkF 41) (define vkG 42) (define vkH 43) (define vkJ 44)(define vkK 45) (define vkL 46)
         ;; (define vkZ 52) (define vkX 53) (define vkC 54) (define vkV 55) (define vkB 56) (define vkN 57) (define vkM 58)
         ;; (define vkStar 63) (define vkPlus 86) (define vkMinus 82) (define vkEqual 21)
         )) 
   (else
      (begin
         ; https://wiki.archlinux.org/index.php/Keyboard_input
         (setq x11 (or (load-dynamic-library "libX11.so")
                       (load-dynamic-library "libX11.so.6")))
         (setq XOpenDisplay (x11 fft-void* "XOpenDisplay" type-string))
         (setq XQueryKeymap (x11 fft-void "XQueryKeymap" type-vptr type-string)) ; todo: change to type-bytevector
         (setq XKeysymToKeycode (x11 fft-int "XKeysymToKeycode" type-vptr fft-int))

         (setq display (XOpenDisplay #f))
         (define keys (make-bytevector 32))
         
         (define (key-pressed? key)
            (XQueryKeymap display keys)
            (let ((code (XKeysymToKeycode display key)))
               (not (zero?
                  (band (<< 1 (band code #b111))
                        (ref keys (>> code 3)))))))

         ; /usr/include/linux/input-event-codes.h
         (define KEY_ENTER #xff52) (define KEY_ESC #xff1b)
         (define KEY_LEFTCTRL #xffe3) (define KEY_LEFTALT #xffe9) (define KEY_LEFTSHIFT #xffe1)
         (define KEY_UP #xff52) (define KEY_DOWN #xff54) (define KEY_LEFT #xff51) (define KEY_RIGHT #xff53)
         ;; (define vkQ 24) (define vkW 25) (define vkE 26) (define vkR 27) (define vkT 28) (define vkY 29) (define vkU 30) (define vkI 31) (define vkO 32) (define vkP 33)
         ;; (define vkA 38) (define vkS 39) (define vkD 40) (define vkF 41) (define vkG 42) (define vkH 43) (define vkJ 44)(define vkK 45) (define vkL 46)
         ;; (define vkZ 52) (define vkX 53) (define vkC 54) (define vkV 55) (define vkB 56) (define vkN 57) (define vkM 58)
         ;; (define vkStar 63) (define vkPlus 86) (define vkMinus 82) (define vkEqual 21)
      )))

(begin
   #true))
