#!/usr/bin/ol

(import (lib tcl-tk)
        (otus ffi)
        (scheme misc))

(define tcl (Tcl_CreateInterp))
(Tcl_Init tcl)

; let's declare function
(define calculate (vm:pin (cons
   ;     userdata  tcl       argc       argv in reverse order
   (list fft-int type-vptr type-vptr fft-int  (list type-string type-string))
   (lambda (userdata tcl argc argv)
      (let ((argv (reverse argv)))
         (print "argc: " argc)
         (print "argv: " argv)

         (let*((feets (string->number (cadr argv) 10))
               (meters (* feets 0.3048))
               (result
                  (fold string-append "" (list "{" (number->string (inexact meters)) "}"))))
            (print "result: " result)
            (Tcl_SetResult tcl result #f)))
      TCL_OK
))))
(define calculateCallback (make-callback calculate))

; and send it to the tcl
(define cmd (Tcl_CreateCommand tcl "calculate" calculateCallback #f #f))



;(Tcl_Eval tcl "calculate 123")
;
;
; run
;(Tk_Init tcl)
;(Tcl_Eval tcl "
;   grid [ttk::button .b -text \"---\"]
;")
(Tcl_EvalFile tcl "p.tcl")
(Tk_MainLoop)
(Tcl_DeleteCommand tcl "calculate")
