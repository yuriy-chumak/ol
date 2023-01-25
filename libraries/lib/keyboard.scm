(define-library (lib keyboard)
   (version 1.2)
   (license MIT/LGPL3)
   (description "keyboard support library")
(import
   (otus lisp) (otus ffi))

(export
   KEY_ENTER KEY_ESC KEY_TILDE
   KEY_UP KEY_DOWN KEY_LEFT KEY_RIGHT
   KEY_BACKSPACE KEY_TAB KEY_HOME KEY_END

   KEY_1 KEY_2 KEY_3 KEY_4 KEY_5 KEY_6 KEY_7 KEY_8 KEY_9 KEY_0
   KEY_Q KEY_W KEY_E KEY_R KEY_T KEY_Y KEY_U KEY_I KEY_O KEY_P
   KEY_A KEY_S KEY_D KEY_F KEY_G KEY_H KEY_J KEY_K KEY_L
   KEY_Z KEY_X KEY_C KEY_V KEY_B KEY_N KEY_M

   KEY_F1 KEY_F2 KEY_F3 KEY_F4 KEY_F5 KEY_F6 KEY_F7 KEY_F8 KEY_F9

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
         (define KEY_ENTER 13) (define KEY_ESC 27) (define KEY_TILDE 192)
         (define KEY_LEFTCTRL 17) (define KEY_LEFTALT 00) (define KEY_LEFTSHIFT 16)
         (define KEY_UP 38) (define KEY_DOWN 40) (define KEY_LEFT 37) (define KEY_RIGHT 39)

         (define KEY_BACKSPACE 8) (define KEY_TAB 9) (define KEY_HOME 36) (define KEY_END 36)

         (define KEY_1 49) (define KEY_2 50) (define KEY_3 51) (define KEY_4 52) (define KEY_5 53) (define KEY_6 54) (define KEY_7 55) (define KEY_8 56) (define KEY_9 57) (define KEY_0 48)
         (define KEY_Q 81) (define KEY_W 87) (define KEY_E 69) (define KEY_R 82) (define KEY_T 84) (define KEY_Y 89) (define KEY_U 85) (define KEY_I 73) (define KEY_O 79) (define KEY_P 80)
         (define KEY_A 65) (define KEY_S 83) (define KEY_D 68) (define KEY_F 70) (define KEY_G 71) (define KEY_H 72) (define KEY_J 74) (define KEY_K 75) (define KEY_L 76)
         (define KEY_Z 90) (define KEY_X 88) (define KEY_C 3167) (define KEY_V 86) (define KEY_B 66) (define KEY_N 78) (define KEY_M 77)

         (define KEY_F1 112) (define KEY_F2 113) (define KEY_F3 114) (define KEY_F4 115) (define KEY_F5 116) (define KEY_F6 117) (define KEY_F7 118) (define KEY_F8 119) (define KEY_F9 120)
      ))
   (Android
      (begin
         (setq this (load-dynamic-library "libmain.so"))
         (setq anlKeyPressed (this fft-bool "anlKeyPressed" fft-int))

         (define key-pressed? anlKeyPressed)

         ; https://android.googlesource.com/platform/frameworks/native/+/refs/heads/master/include/android/keycodes.h
         (define KEY_ENTER 66) (define KEY_ESC 4) (define KEY_TILDE 68)
         (define KEY_UP 19) (define KEY_DOWN 20) (define KEY_LEFT 21) (define KEY_RIGHT 22)
         (define KEY_BACKSPACE 67) (define KEY_TAB 61) (define KEY_HOME #xff50) (define KEY_END #xff57)

         (define KEY_1 8) (define KEY_2 9) (define KEY_3 10) (define KEY_4 11) (define KEY_5 12) (define KEY_6 13) (define KEY_7 14) (define KEY_8 15) (define KEY_9 16) (define KEY_0 7)
         (define KEY_Q 45) (define KEY_W 51) (define KEY_E 33) (define KEY_R 46) (define KEY_T 48) (define KEY_Y 53) (define KEY_U 49) (define KEY_I 37) (define KEY_O 43) (define KEY_P 44)
         (define KEY_A 29) (define KEY_S 47) (define KEY_D 32) (define KEY_F 34) (define KEY_G 35) (define KEY_H 36) (define KEY_J 38) (define KEY_K 39) (define KEY_L 40)
         (define KEY_Z 54) (define KEY_X 52) (define KEY_C 31) (define KEY_V 50) (define KEY_B 30) (define KEY_N 42) (define KEY_M 41)

         (define KEY_F1 131) (define KEY_F2 132) (define KEY_F3 133) (define KEY_F4 134) (define KEY_F5 135) (define KEY_F6 136) (define KEY_F7 137) (define KEY_F8 138) (define KEY_F9 139)
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
         (define KEY_ENTER #xff0d) (define KEY_ESC #xff1b) (define KEY_TILDE #x0060)
         (define KEY_UP #xff52) (define KEY_DOWN #xff54) (define KEY_LEFT #xff51) (define KEY_RIGHT #xff53)
         (define KEY_BACKSPACE #xff08) (define KEY_TAB #xff09) (define KEY_HOME #xff50) (define KEY_END #xff57)

         (define KEY_1 #x31) (define KEY_2 #x32) (define KEY_3 #x33) (define KEY_4 #x34) (define KEY_5 #x35) (define KEY_6 #x36) (define KEY_7 #x37) (define KEY_8 #x38) (define KEY_9 #x39) (define KEY_0 #x30)
         (define KEY_Q #x71) (define KEY_W #x77) (define KEY_E #x65) (define KEY_R #x72) (define KEY_T #x74) (define KEY_Y #x79) (define KEY_U #x75) (define KEY_I #x69) (define KEY_O #x6f) (define KEY_P #x70)
         (define KEY_A #x61) (define KEY_S #x73) (define KEY_D #x64) (define KEY_F #x66) (define KEY_G #x67) (define KEY_H #x68) (define KEY_J #x6a) (define KEY_K #x6b) (define KEY_L #x6c)
         (define KEY_Z #x7a) (define KEY_X #x78) (define KEY_C #x63) (define KEY_V #x76) (define KEY_B #x62) (define KEY_N #x6e) (define KEY_M #x6d)

         (define KEY_F1 #xffbe) (define KEY_F2 #xffbf) (define KEY_F3 #xffc0) (define KEY_F4 #xffc1) (define KEY_F5 #xffc2) (define KEY_F6 #xffc3) (define KEY_F7 #xffc4) (define KEY_F8 #xffc5) (define KEY_F9 #xffc6)
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
