#!/usr/bin/ol

(import (lib tcl-tk)
        (otus ffi)
        (scheme misc))

(define tcl (Tcl_CreateInterp))
(Tcl_Init tcl)

; let's declare function
(define calculate (vm:pin (cons
   ;     userdata  tcl       argc       argv in reverse order
   (list type-vptr type-vptr type-int+  (list type-string type-string))
   (lambda (userdata tcl argc argv)
      (let ((argv (reverse argv)))
         (print "argc: " argc)
         (print "argv: " argv)

         (let*((feets (string->number (cadr argv) 10))
               (meters (* feets 0.3048))
               (result
                  (fold string-append "" (list "{" (number->string (inexact meters)) "}"))))
            (print "result: " result)
            (Tcl_SetResult tcl (c-string result) #f)))
      TCL_OK
))))
(define calculateCallback (make-callback calculate))

; and send it to the tcl
(define cmd (Tcl_CreateCommand tcl (c-string "calculate") calculateCallback #f #f))



;(Tcl_Eval tcl (c-string "calculate 123"))
;
;
; run
;(Tk_Init tcl)
;(Tcl_Eval tcl (c-string "
;   grid [ttk::button .b -text \"---\"]
;"))
(Tcl_EvalFile tcl (c-string "p.tcl"))
(Tk_MainLoop)
(Tcl_DeleteCommand tcl (c-string "calculate"))
