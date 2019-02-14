; https://github.com/yuriy-chumak/rlutil
(define-library (lib rlutil)
   (import (scheme base)
           (owl string)
           (owl math) (owl io) (owl list) (owl list-extra)
           (otus ffi))
   (export
      ; colors:
      BLACK BLUE GREEN CYAN RED MAGENTA BROWN GREY DARKGREY LIGHTBLUE LIGHTGREEN LIGHTCYAN LIGHTRED LIGHTMAGENTA YELLOW WHITE

      cls locate
      set-color set-background-color reset-color
      set-console-title
      key-pressed
      )
(begin
   (define uname (syscall 63 #f #f #f))                 ; uname -a
;   (define TERM (syscall 1016 (c-string "TERM") #f #f)) ; getenv('TERM')

   (define win32? (string-ci=? (ref uname 1) "Windows"))
   (define linux? (string-ci=? (ref uname 1) "Linux"))

   ; this check based on https://gist.github.com/ssbarnea/1316877
   ; (define ansi? (not (and win32?
   ;                        (not (and TERM
   ;                                  (string-ci=? TERM "ANSI"))))))

   (define ANSI_CLS                "\x1B;[2J\x1B;[3J")
   (define ANSI_CONSOLE_TITLE_PRE  "\x1B;]0;")
   (define ANSI_CONSOLE_TITLE_POST "\x07;")
   (define ANSI_ATTRIBUTE_RESET    "\x1B;[0m")
   (define ANSI_CURSOR_HIDE        "\x1B;[?25l")
   (define ANSI_CURSOR_SHOW        "\x1B;[?25h")
   (define ANSI_CURSOR_HOME        "\x1B;[H")
   (define ANSI_BLACK              "\x1B;[22;30m")
   (define ANSI_RED                "\x1B;[22;31m")
   (define ANSI_GREEN              "\x1B;[22;32m")
   (define ANSI_BROWN              "\x1B;[22;33m")
   (define ANSI_BLUE               "\x1B;[22;34m")
   (define ANSI_MAGENTA            "\x1B;[22;35m")
   (define ANSI_CYAN               "\x1B;[22;36m")
   (define ANSI_GREY               "\x1B;[22;37m")
   (define ANSI_DARKGREY           "\x1B;[01;30m")
   (define ANSI_LIGHTRED           "\x1B;[01;31m")
   (define ANSI_LIGHTGREEN         "\x1B;[01;32m")
   (define ANSI_YELLOW             "\x1B;[01;33m")
   (define ANSI_LIGHTBLUE          "\x1B;[01;34m")
   (define ANSI_LIGHTMAGENTA       "\x1B;[01;35m")
   (define ANSI_LIGHTCYAN          "\x1B;[01;36m")
   (define ANSI_WHITE              "\x1B;[01;37m")
   (define ANSI_BACKGROUND_BLACK   "\x1B;[40m")
   (define ANSI_BACKGROUND_RED     "\x1B;[41m")
   (define ANSI_BACKGROUND_GREEN   "\x1B;[42m")
   (define ANSI_BACKGROUND_YELLOW  "\x1B;[43m")
   (define ANSI_BACKGROUND_BLUE    "\x1B;[44m")
   (define ANSI_BACKGROUND_MAGENTA "\x1B;[45m")
   (define ANSI_BACKGROUND_CYAN    "\x1B;[46m")
   (define ANSI_BACKGROUND_WHITE   "\x1B;[47m")

   (define ANSI_BACKGROUND_COLORS (tuple
      ANSI_BACKGROUND_BLACK
      ANSI_BACKGROUND_RED
      ANSI_BACKGROUND_GREEN
      ANSI_BACKGROUND_YELLOW
      ANSI_BACKGROUND_BLUE
      ANSI_BACKGROUND_MAGENTA
      ANSI_BACKGROUND_CYAN
      ANSI_BACKGROUND_WHITE))

   (define ANSI_COLORS (tuple
      ANSI_BLACK
      ANSI_BLUE
      ANSI_GREEN
      ANSI_CYAN
      ANSI_RED
      ANSI_MAGENTA
      ANSI_BROWN
      ANSI_GREY
      ANSI_DARKGREY
      ANSI_LIGHTBLUE
      ANSI_LIGHTGREEN
      ANSI_LIGHTCYAN
      ANSI_LIGHTRED
      ANSI_LIGHTMAGENTA
      ANSI_YELLOW
      ANSI_WHITE))

   (define BLACK         0)
   (define BLUE          1)
   (define GREEN         2)
   (define CYAN          3)
   (define RED           4)
   (define MAGENTA       5)
   (define BROWN         6)
   (define GREY          7)
   (define DARKGREY      8)
   (define LIGHTBLUE     9)
   (define LIGHTGREEN   10)
   (define LIGHTCYAN    11)
   (define LIGHTRED     12)
   (define LIGHTMAGENTA 13)
   (define YELLOW       14)
   (define WHITE        15)

(define cls (cond
   (win32?
      ; Based on https://msdn.microsoft.com/en-us/library/windows/desktop/ms682022%28v=vs.85%29.aspx
      (let*((kernel32 (load-dynamic-library "kernel32.dll"))
            (STD_OUTPUT_HANDLE -11)
            (GetStdHandle               (kernel32 type-vptr "GetStdHandle" type-fix+))
            (GetConsoleScreenBufferInfo (kernel32 type-vptr "GetConsoleScreenBufferInfo" type-vptr type-vptr))
            (FillConsoleOutputCharacter (kernel32 type-vptr "FillConsoleOutputCharacterW" type-vptr type-fix+ type-int+ type-int+ type-vptr))
            (FillConsoleOutputAttribute (kernel32 type-vptr "FillConsoleOutputAttribute" type-vptr type-int+ type-int+ type-int+ type-vptr))
            (SetConsoleCursorPosition   (kernel32 type-vptr "SetConsoleCursorPosition" type-vptr type-int+)))
      (lambda ()
         (let ((hConsole (GetStdHandle STD_OUTPUT_HANDLE))
               (csbi (make-bytevector 22)) ; CONSOLE_SCREEN_BUFFER_INFO
               (cCharsWritten (make-bytevector 4))) ; DWORD
            (GetConsoleScreenBufferInfo hConsole csbi)
            (let*((dwSizeX (+ (ref csbi 0) (<< (ref csbi 1) 8)))
                  (dwSizeY (+ (ref csbi 2) (<< (ref csbi 3) 8)))
                  (dwConSize (* dwSizeX dwSizeY))
                  (wAttrib (+ (ref csbi 4) (<< (ref csbi 5) 8))))
               (FillConsoleOutputCharacter hConsole #\space dwConSize 0 cCharsWritten)
               (GetConsoleScreenBufferInfo hConsole csbi) ; again?
               (FillConsoleOutputAttribute hConsole wAttrib dwConSize 0 cCharsWritten)
               (SetConsoleCursorPosition hConsole 0))))))
   (linux?
      (lambda ()
         (for-each display `(,ANSI_CLS ,ANSI_CURSOR_HOME))))))

(define locate (cond
   (win32?
      (let*((kernel32 (load-dynamic-library "kernel32.dll"))
            (STD_OUTPUT_HANDLE -11)
            (GetStdHandle               (kernel32 type-vptr "GetStdHandle" type-fix+))
            (SetConsoleCursorPosition   (kernel32 type-vptr "SetConsoleCursorPosition" type-vptr type-int+)))
      (lambda (x y)
         (let ((COORD (+ (band (- x 1) #xFFFF)
                         (<< (band (- y 1) #xFFFF) 16))))
            (SetConsoleCursorPosition (GetStdHandle STD_OUTPUT_HANDLE) COORD)))))
   (linux?
      (lambda (x y)
         (for-each display `("\x1B;[" ,y ";" ,x "f"))))))


(define set-color (cond
   (win32?
      (let*((kernel32 (load-dynamic-library "kernel32.dll"))
            (STD_OUTPUT_HANDLE -11)
            (GetStdHandle               (kernel32 type-vptr "GetStdHandle" type-fix+))
            (GetConsoleScreenBufferInfo (kernel32 type-vptr "GetConsoleScreenBufferInfo" type-vptr type-vptr))
            (SetConsoleTextAttribute (kernel32 type-vptr "SetConsoleTextAttribute" type-vptr type-int+)))
      (lambda (color)
         (let ((hConsole (GetStdHandle STD_OUTPUT_HANDLE))
               (csbi (make-bytevector 22))) ; CONSOLE_SCREEN_BUFFER_INFO
            (GetConsoleScreenBufferInfo hConsole csbi)
            (let ((wAttrib (+ (ref csbi 4) (<< (ref csbi 5) 8))))
               (SetConsoleTextAttribute hConsole (bor (band wAttrib #xFFF0) color)))))))
   (linux?
      (lambda (color)
         (display (ref ANSI_COLORS (+ color 1)))))))

(define set-background-color (cond
   (win32?
      (let*((kernel32 (load-dynamic-library "kernel32.dll"))
            (STD_OUTPUT_HANDLE -11)
            (GetStdHandle               (kernel32 type-vptr "GetStdHandle" type-fix+))
            (GetConsoleScreenBufferInfo (kernel32 type-vptr "GetConsoleScreenBufferInfo" type-vptr type-vptr))
            (SetConsoleTextAttribute (kernel32 type-vptr "SetConsoleTextAttribute" type-vptr type-int+)))
      (lambda (color)
         (let ((hConsole (GetStdHandle STD_OUTPUT_HANDLE))
               (csbi (make-bytevector 22))) ; CONSOLE_SCREEN_BUFFER_INFO
            (GetConsoleScreenBufferInfo hConsole csbi)
            (let ((wAttrib (+ (ref csbi 4) (<< (ref csbi 5) 8))))
               (SetConsoleTextAttribute hConsole (bor (band wAttrib #xFF0F) (<< color 4))))))))
   (linux?
      (lambda (color)
         (display (ref ANSI_BACKGROUND_COLORS (+ color 1)))))))


(define reset-color (cond ; something wrong!
   (win32?
      (let*((kernel32 (load-dynamic-library "kernel32.dll"))
            (STD_OUTPUT_HANDLE -11)
            (GetStdHandle               (kernel32 type-vptr "GetStdHandle" type-fix+))
            (GetConsoleScreenBufferInfo (kernel32 type-vptr "GetConsoleScreenBufferInfo" type-vptr type-vptr))
            (SetConsoleTextAttribute (kernel32 type-vptr "SetConsoleTextAttribute" type-vptr type-int+))

            (hConsole (GetStdHandle STD_OUTPUT_HANDLE))
            (csbi (make-bytevector 22))) ; CONSOLE_SCREEN_BUFFER_INFO
         (GetConsoleScreenBufferInfo hConsole csbi)
      (let ((wAttrib (+ (ref csbi 4) (<< (ref csbi 5) 8))))
      (lambda ()
         (let ((hConsole (GetStdHandle STD_OUTPUT_HANDLE)))
            (print wAttrib)
            (SetConsoleTextAttribute hConsole wAttrib))))))
   (linux?
      (lambda ()
         (display ANSI_ATTRIBUTE_RESET)))))

(define set-console-title (cond
   (win32?
      (let*((kernel32 (load-dynamic-library "kernel32.dll"))
            (SetConsoleTitleA (kernel32 type-vptr "SetConsoleTitleA" type-string))
            (SetConsoleTitleW (kernel32 type-vptr "SetConsoleTitleW" type-string-wide)))
      (lambda (title)
         (case (type title)
            (type-string
               (SetConsoleTitleA (c-string title)))
            (type-string-wide
               (SetConsoleTitleW title))))))
   (linux?
      (lambda (title)
         (display ANSI_CONSOLE_TITLE_PRE)
         (display title)
         (display ANSI_CONSOLE_TITLE_POST)))))

(define key-pressed (cond
   (win32?
      (lambda () #f))
   (linux?
      (let*((x11 (load-dynamic-library "libX11.so.6"))
            (XOpenDisplay (x11 fft-void* "XOpenDisplay" type-string))
            (XCloseDisplay (x11 fft-void "XCloseDisplay" type-vptr))
            (XQueryKeymap (x11 fft-void "XQueryKeymap" type-vptr type-string))
            (XKeysymToKeycode (x11 fft-int "XKeysymToKeycode" type-vptr fft-int))
            (keys (list->string (repeat 0 32))))
      (lambda (key)
         (let ((display (XOpenDisplay #f)))
            (XQueryKeymap display keys)
            (let ((code (XKeysymToKeycode display key)))
               (XCloseDisplay display)
               ;(print code)
               (not (zero?
                  (band (<< 1 (band code #x7))
                        (ref keys (>> code 3))))))))))))
))
