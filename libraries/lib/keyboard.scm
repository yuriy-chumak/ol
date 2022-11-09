(define-library (lib keyboard)
   (version 1.2)
   (license MIT/LGPL3)
   (description "keyboard support library")
(import
   (otus lisp) (otus ffi))

(export
   KEY_ENTER KEY_ESC
   KEY_LEFTCTRL KEY_LEFTALT KEY_LEFTSHIFT
   KEY_UP KEY_DOWN KEY_LEFT KEY_RIGHT
   KEY_MINUS KEY_PLUS KEY_EQUAL KEY_BACKSPACE KEY_TAB
   KEY_HOME

   KEY_1 KEY_2 KEY_3 KEY_4 KEY_5 KEY_6 KEY_7 KEY_8 KEY_9 KEY_0

   KEY_Q KEY_W KEY_E KEY_R KEY_T KEY_Y KEY_U KEY_I KEY_O KEY_P
   KEY_A KEY_S KEY_D KEY_F KEY_G KEY_H KEY_J KEY_K KEY_L
   KEY_Z KEY_X KEY_C KEY_V KEY_B KEY_N KEY_M

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

         (define KEY_1 #x31) (define KEY_2 #x32) (define KEY_3 #x33) (define KEY_4 #x34) (define KEY_5 #x35)
         (define KEY_6 #x36) (define KEY_7 #x37) (define KEY_8 #x38) (define KEY_9 #x39) (define KEY_0 #x30)
         (define KEY_MINUS #xBD) (define KEY_EQUAL #xBB) (define KEY_BACKSPACE #x08) (define KEY_TAB #x09)

         ;; (define vkQ 24) (define vkW 25) (define vkE 26) (define vkR 27) (define vkT 28) (define vkY 29) (define vkU 30) (define vkI 31) (define vkO 32) (define vkP 33)
         ;; (define vkA 38) (define vkS 39) (define vkD 40) (define vkF 41) (define vkG 42) (define vkH 43) (define vkJ 44)(define vkK 45) (define vkL 46)
         ;; (define vkZ 52) (define vkX 53) (define vkC 54) (define vkV 55) (define vkB 56) (define vkN 57) (define vkM 58)
         ;; (define vkStar 63) (define vkPlus 86) (define vkMinus 82) (define vkEqual 21)
         )) 
   (Android
      (begin
         (setq this (load-dynamic-library "libmain.so"))
         (setq anlKeyPressed (this fft-bool "anlKeyPressed" fft-int))

         (define key-pressed? anlKeyPressed)

         ; https://android.googlesource.com/platform/frameworks/native/+/refs/heads/master/include/android/keycodes.h
         (define KEY_ENTER 66) (define KEY_ESC 111)
         (define KEY_LEFTCTRL 113) (define KEY_LEFTALT 57) (define KEY_LEFTSHIFT 59)
         (define KEY_UP 19) (define KEY_DOWN 20) (define KEY_LEFT 21) (define KEY_RIGHT 22)

         (define KEY_MINUS 69) (define KEY_PLUS 81) (define KEY_EQUAL 70) (define KEY_BACKSPACE 67) (define KEY_TAB 61) (define KEY_HOME 122)
         (define KEY_1 8) (define KEY_2 9) (define KEY_3 10) (define KEY_4 11) (define KEY_5 12) (define KEY_6 13) (define KEY_7 14) (define KEY_8 15) (define KEY_9 16) (define KEY_0 7)

         (define KEY_Q 45) (define KEY_W 51) (define KEY_E 33) (define KEY_R 46) (define KEY_T 48) (define KEY_Y 53) (define KEY_U 49) (define KEY_I 37) (define KEY_O 43) (define KEY_P 44)
         (define KEY_A 29) (define KEY_S 47) (define KEY_D 32) (define KEY_F 34) (define KEY_G 35) (define KEY_H 36) (define KEY_J 38) (define KEY_K 39) (define KEY_L 40)
         (define KEY_Z 54) (define KEY_X 52) (define KEY_C 31) (define KEY_V 50) (define KEY_B 30) (define KEY_N 42) (define KEY_M 41)

         ))
   (Linux
      (begin
         ; https://wiki.archlinux.org/index.php/Keyboard_input
         (setq x11 (or (load-dynamic-library "libX11.so")
                       (load-dynamic-library "libX11.so.6")))
         (setq XOpenDisplay (x11 fft-void* "XOpenDisplay" type-string))
         (setq XQueryKeymap (x11 fft-void "XQueryKeymap" type-vptr type-vptr)) ; todo: change to (fft* fft-char))
         (setq XKeysymToKeycode (x11 fft-int "XKeysymToKeycode" type-vptr fft-int))

         (setq display (XOpenDisplay #f))
         (define keys (make-bytevector 32))
         
         (define (key-pressed? key)
            (XQueryKeymap display keys)
            (let ((code (XKeysymToKeycode display key)))
               (not (zero?
                  (band (<< 1 (band code #b111))
                        (ref keys (>> code 3)))))))

         ; /usr/include/X11/keysymdef.h
         (define KEY_ENTER #xff52) (define KEY_ESC #xff1b)
         (define KEY_LEFTCTRL #xffe3) (define KEY_LEFTALT #xffe9) (define KEY_LEFTSHIFT #xffe1)
         (define KEY_UP #xff52) (define KEY_DOWN #xff54) (define KEY_LEFT #xff51) (define KEY_RIGHT #xff53)

         (define KEY_MINUS #x2D) (define KEY_PLUS #x3D) (define KEY_EQUAL #x2B) (define KEY_BACKSPACE #xff08) (define KEY_TAB #xff09) (define KEY_HOME #xff50)

         (define KEY_1 #x31) (define KEY_2 #x32) (define KEY_3 #x33) (define KEY_4 #x34) (define KEY_5 #x35) (define KEY_6 #x36) (define KEY_7 #x37) (define KEY_8 #x38) (define KEY_9 #x39) (define KEY_0 #x30)

         (define KEY_Q #x71) (define KEY_W #x77) (define KEY_E #x65) (define KEY_R #x72) (define KEY_T #x74) (define KEY_Y #x79) (define KEY_U #x75) (define KEY_I #x69) (define KEY_O #x6f) (define KEY_P #x70)
         (define KEY_A #x61) (define KEY_S #x73) (define KEY_D #x64) (define KEY_F #x66) (define KEY_G #x67) (define KEY_H #x68) (define KEY_J #x6a) (define KEY_K #x6b) (define KEY_L #x6c)
         (define KEY_Z #x7a) (define KEY_X #x78) (define KEY_C #x63) (define KEY_V #x76) (define KEY_B #x62) (define KEY_N #x6e) (define KEY_M #x6d)

         ;; (define vkStar 63) (define vkPlus 86) (define vkMinus 82) (define vkEqual 21)
      ))
   (Darwin
      (begin
         (define (key-pressed? key)
            #false)

         (define KEY_ENTER #f) (define KEY_ESC #f)
         (define KEY_LEFTCTRL #f) (define KEY_LEFTALT #f) (define KEY_LEFTSHIFT #f)
         (define KEY_UP #f) (define KEY_DOWN #f) (define KEY_LEFT #f) (define KEY_RIGHT #f)
         (define KEY_1 #f) (define KEY_2 #f) (define KEY_3 #f) (define KEY_4 #f) (define KEY_5 #f) (define KEY_6 #f) (define KEY_7 #f) (define KEY_8 #f) (define KEY_9 #f) (define KEY_0 #f)
         (define KEY_MINUS #f) (define KEY_EQUAL #f) (define KEY_BACKSPACE #f) (define KEY_TAB #f)

      ))
   (else
      (begin
         (runtime-error "Unsupported platform" (syscall 63))))))
