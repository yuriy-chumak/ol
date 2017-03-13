; https://github.com/yuriy-chumak/rlutil
(define-library (lib rlutil)
   (import (r5rs core)
           (otus pinvoke))
   (export
      cls locate setConsoleTitle)
(begin

   (define rlutil (or (dlopen "rlutil.dll")
                      (dlopen "librlutil.so")
                      (runtime-error "Can't load rlutil" #f)))
   (define gotoxy (dlsym rlutil type-void "gotoxy" type-int+ type-int+))
   ;getkey
   ;nb_getch
   ;getANSIColor
   ;getANSIBackgroundColor
   ;setColor
   ;setBackgroundColor
   ;saveDefaultColor
   ;resetColor
   (define cls (dlsym rlutil type-void "cls"))
   (define locate (dlsym rlutil type-void "locate" type-int+ type-int+))
   (define setString (dlsym rlutil type-void "setString" type-string))
   (define setChar   (dlsym rlutil type-void "setChar"   type-fix+  ))
   ;...
   (define trows (dlsym rlutil type-int+ "trows"))
   (define tcols (dlsym rlutil type-int+ "tcols"))
   ;...
   (define setConsoleTitle (dlsym rlutil type-void "setConsoleTitle" type-string))

))
